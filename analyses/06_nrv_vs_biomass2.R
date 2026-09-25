# Load future reef fish biomass

load("data/raw-data/00_rls_surveys.Rdata")

rls_surveys$survey_id <- as.character(rls_surveys$survey_id)

migration_ensemble <- readRDS("data/raw-data/migration_ensemble8.rds")

species_to_remove <- c("Acanthurus grammoptilus",
                       "Amblyglyphidodon curacao", 
                       "Chaetodon kleinii", "Chaetodon ornatissimus",
                       "Chromis cyanea", "Epinephelus daemelii", "Fistularia commersonii", 
                       "Gnathodentex aureolineatus", "Goniistius vestitus", "Hologymnosus annulatus", 
                       "Hypoplectrus nigricans", "Lutjanus carponotatus", 
                       "Neoglyphidodon nigroris", "Neoglyphidodon polyacanthus", "Novaculichthys taeniourus", 
                       "Parupeneus spilurus", "Plectorhinchus multivittatus", 
                       "Pomacentrus milleri", "Pomacentrus reidi", "Pseudanthias huchtii", 
                       "Pterocaesio pisang", "Stegastes nigricans", 
                       "Thalassoma amblycephalum")
list_species <- unlist(pbmcapply::pbmclapply(1:length(migration_ensemble), function(i) {
  
  unique(migration_ensemble[[i]][[1]]$species_name)
  
}, mc.cores = parallel::detectCores() - 1))

which_species_to_remove <- which(list_species %in% species_to_remove)
migration_ensemble <- migration_ensemble[-which_species_to_remove]

biomass_ssp <- lapply(1:4, function(i) { # 4 = the number of SSP 
  
  biomass_ssp_i <- lapply(migration_ensemble, '[[', i)
  biomass_sspi_reduce <- do.call(rbind, biomass_ssp_i)
  
})

biomass_ssp_all <- do.call(rbind, biomass_ssp)

biomass_ssp_all_info <- rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "site_code", "depth", "longitude", "latitude")] |> 
  dplyr::inner_join(biomass_ssp_all)
biomass_ssp_all_info <- biomass_ssp_all_info[, colnames(biomass_ssp_all_info) %in% c("survey_id",
                                                                                     "site_code",
                                                                                     "latitude",
                                                                                     "longitude",
                                                                                     "depth",
                                                                                     "species_name",
                                                                                     "scenario",
                                                                                     "2024",
                                                                                     "2075",
                                                                                     "model")]

biomass_ssp_species_site <- biomass_ssp_all_info |> 
  dplyr::select(c(site_code, depth, longitude, latitude, species_name, scenario)) |> 
  unique()

biomass_longer <- biomass_ssp_all_info |> 
  tidyr::pivot_longer(cols = "2024":"2075",  # convert year columns to long format
                      names_to = "year", 
                      values_to = "biomass_g_500m2") |> 
  dplyr::filter(biomass_g_500m2 > 0)

biomass_site <- biomass_longer |> 
  dplyr::group_by(site_code, depth, latitude, longitude, scenario, year) |> 
  dplyr::summarise(biomass = sum(biomass_g_500m2))

delta_biomass <- biomass_site |> 
  # dplyr::filter(scenario == "ssp126")
  dplyr::filter(scenario == "ssp585")

delta_log <- unlist(delta_biomass |> 
                      dplyr::ungroup() |> 
                      dplyr::filter(year == "2075") |> 
                      dplyr::select(biomass) |> 
                      dplyr::mutate(biomass = log10(biomass))) - unlist(delta_biomass |> 
                                                                          dplyr::ungroup() |> 
                                                                          dplyr::filter(year == "2024") |> 
                                                                          dplyr::select(biomass) |> 
                                                                          dplyr::mutate(biomass = log10(biomass)))

delta_biomass <- delta_biomass |> 
  dplyr::ungroup() |> 
  dplyr::filter(year == "2075") |> 
  dplyr::select(-c(year, biomass)) |> 
  dplyr::mutate(delta_biomass = delta_log) |> 
  dplyr::ungroup() |>
  dplyr::mutate(site_code = paste0(site_code, "_", depth))

load("outputs/site_nrv_migration2.Rdata")

site_nrv_h <- site_nrv_migration |> 
  dplyr::mutate(highlight = dplyr::case_when(type == "nrv" ~ TRUE,
                                             .default = FALSE)) 
site_nrv_h[site_nrv_h$type == "nrv",]$year <- "2024"

site_nrv_point <- site_nrv_h[1,]

remove_country <- site_nrv_h |> 
  dplyr::filter(year == "2024") |> 
  dplyr::group_by(country) |> 
  dplyr::summarise(n = dplyr::n()) |> 
  dplyr::filter(n >= 5)

site_nrv_h <- site_nrv_h |> 
  dplyr::filter(country %in% remove_country$country)

site_nrv_h <- rbind(site_nrv_point, site_nrv_h)

delta <- site_nrv_h[-1,] |> 
  dplyr::group_by(site_code, depth, latitude, longitude, realm, country, scenario) |> 
  dplyr::summarise(delta_dist_to_NRV = (dplyr::last(dist_to_NRV) * 100) / dplyr::first(dist_to_NRV) - 100) |>
  dplyr::ungroup() |> 
  dplyr::mutate(site_code = paste0(site_code, "_", depth)) |>
  dplyr::select(-depth)

delta_nrv <- delta_biomass |> 
  dplyr::inner_join(delta)

site_state <- delta_nrv |> 
  dplyr::mutate(state = dplyr::case_when(delta_biomass < 0 & delta_dist_to_NRV > 0 ~ "Loser-Loser",
                                         delta_biomass > 0 & delta_dist_to_NRV < 0 ~ "Winner-Winner",
                                         delta_biomass < 0 & delta_dist_to_NRV < 0 ~ "Loser-Winner",
                                         delta_biomass > 0 & delta_dist_to_NRV > 0 ~ "Winner-Loser")) |> 
  dplyr::select(site_code, latitude, longitude, state)

delta_nrv |> 
  dplyr::mutate(biomass_state = dplyr::case_when(delta_biomass < 0 ~ "loss",
                                                 delta_biomass > 0 ~ "gain")) |> 
  dplyr::group_by(biomass_state) |> 
  dplyr::summarise(n = dplyr::n()) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(sum_n = sum(n),
                pr = (n * 100) / sum_n)

delta_nrv |> 
  dplyr::mutate(nrv_state = dplyr::case_when(delta_dist_to_NRV < 0 ~ "gain",
                                             delta_dist_to_NRV > 0 ~ "loss")) |> 
  dplyr::group_by(nrv_state) |> 
  dplyr::summarise(n = dplyr::n()) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(sum_n = sum(n),
                pr = (n * 100) / sum_n)

site_state |> 
  dplyr::group_by(state) |> 
  dplyr::summarise(n = dplyr::n()) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(sum_n = sum(n),
                pr = (n * 100) / sum_n)

# SSP1-2.6            # SSP5-8.5
# Loser-Loser 21.6%   # 27.9% 
# Loser-Winner 48.0%  # 51.2%
# Winner-Loser 12.7%  # 9.12%
# Winner-Winner 17.8  # 11.7%


delta_nrv <- delta_biomass |> 
  dplyr::inner_join(delta) |> 
  dplyr::inner_join(site_state)


extreme <- delta_nrv |> 
  dplyr::mutate(q_delta_biomass_95 = quantile(delta_biomass, probs = 0.95),
                q_delta_biomass_5 = quantile(delta_biomass, probs = 0.05),
                q_delta_dist_to_NRV_95 = quantile(delta_dist_to_NRV, probs = 0.95),
                q_delta_dist_to_NRV_5 = quantile(delta_dist_to_NRV, probs = 0.05),
                extreme_delta_biomass = ifelse(delta_biomass >= q_delta_biomass_95 | delta_biomass <= q_delta_biomass_5, TRUE, FALSE),
                extreme_delta_dist_nrv = ifelse(delta_dist_to_NRV >= q_delta_dist_to_NRV_95 | delta_dist_to_NRV <= q_delta_dist_to_NRV_5, TRUE, FALSE),
                extreme = ifelse(extreme_delta_biomass | extreme_delta_dist_nrv, TRUE, FALSE))


library(ggplot2)

color_scale <- ggthemes::colorblind_pal()(8)[-1]

delta_biomass_dist_nrv_plot_migration <- delta_nrv |>
  ggplot(aes(y = delta_biomass, x = delta_dist_to_NRV, color = state)) +
  geom_point(size = 1, alpha = 0.4, position = "jitter") +
  scale_color_manual(values = c("Winner-Winner" = color_scale[3],
                                "Winner-Loser" = color_scale[2],
                                "Loser-Winner" = color_scale[4],
                                "Loser-Loser" = color_scale[1])) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(legend.position = "none",
        legend.text = element_text(size = 11),
        axis.text = element_text(size = 11)) +
  labs(y = "log10(Biomass change)", x = "Distance to MIT change (%)", color = "", title = "A.")


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sv")
world <- terra::crop(world, terra::ext(c(-180, 180, -38, 38)))

world_project <- terra::project(world, "EPSG:3832") |>
  sf::st_as_sf()

delta_nrv_sf <- terra::vect(extreme[extreme$extreme == TRUE,], geom = c("longitude", "latitude"), crs = "WGS84")


delta_nrv_sf_project <- terra::project(delta_nrv_sf, "EPSG:3832")
crds_delta <- terra::crds(delta_nrv_sf_project, df = TRUE)

delta_nrv_sf_project_df <- delta_nrv_sf_project |>
  as.data.frame() |>
  cbind(crds_delta)

delta_nrv_sf_project_df <- delta_nrv_sf_project_df |>
  dplyr::mutate(x = x + runif(nrow(delta_nrv_sf_project_df), -100000, 100000),
                y = y + runif(nrow(delta_nrv_sf_project_df), -100000, 100000))

state_Map <- ggplot(data = world_project) +
  geom_sf(color = NA) +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "blank", size = 0.5)) +
  theme_bw() +
  geom_point(data = delta_nrv_sf_project_df, aes(x = x, y = y, color = state), size = 1.5) +
  scale_color_manual(values = c("Winner-Winner" = color_scale[3],
                                "Winner-Loser" = color_scale[2],
                                "Loser-Winner" = color_scale[4],
                                "Loser-Loser" = color_scale[1])) +
  coord_sf(expand = FALSE) +
  theme(axis.text = element_text(size = 9.5),
        axis.title = element_text(size = 9.5),
        title = element_text(size = 9.5), 
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 12)) +
  labs(color = "", x = "", y = "", title = "B.")


delta_dist_nrv_migration_vs_state_Map <-
  cowplot::plot_grid(
    delta_biomass_dist_nrv_plot_migration,
    state_Map,
    ncol = 1,
    align = "v",
    axis = c("l", "r")
  ) +
  cowplot::theme_cowplot() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# ggsave("figures/delta_dist_nrv_migration_vs_state_Map_ssp585.png", delta_dist_nrv_migration_vs_state_Map, height = 7)
ggsave("figures/delta_dist_nrv_migration_vs_state_Map_ssp126.png", delta_dist_nrv_migration_vs_state_Map, height = 7)





nrv_2075 <- site_nrv_h[site_nrv_h$year == "2075",]

indadequate_intake_cr <- read.csv("data/raw-data/2017_perc_pop_deficient.csv", header = TRUE) |>
  dplyr::filter(nutrient != "Vitamin B-12") |> 
  dplyr::group_by(country, iso3) |>
  dplyr::summarise(indadequate_intake = mean(perc_deficient)) |> 
  dplyr::inner_join(nrv_2075) |> 
  dplyr::mutate(site_code = paste0(site_code, "_", depth)) 

load("data/raw-data/rls_covariates_train.RData")

site <- rls_surveys[, colnames(rls_surveys) %in% c("survey_id", "site_code", "depth", "longitude", "latitude")] |> 
  unique()
site$survey_id <- as.numeric(site$survey_id)

rls_covariates_train$survey_id <- as.numeric(rls_covariates_train$survey_id)

cov_baseline_site <- rls_covariates_train |> 
  dplyr::inner_join(site) |> 
  dplyr::select(c(site_code, depth, longitude, latitude, gdp, gravity, marine_ecosystem_dependency, hdi)) |> 
  dplyr::group_by(site_code, depth, longitude, latitude) |> 
  dplyr::summarise(dplyr::across(c(gdp:hdi), mean)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(site_code = paste0(site_code, "_", depth))

text_plot <- indadequate_intake_cr |> 
  dplyr::inner_join(cov_baseline_site) |> 
  dplyr::group_by(country, iso3) |> 
  dplyr::summarise(mean_dist_nrv = mean(dist_to_NRV),
                   indadequate_intake = mean(indadequate_intake))


text_plot[text_plot$iso3 == "PNG",]$indadequate_intake <- 87
text_plot[text_plot$iso3 == "SYC",]$indadequate_intake <- 93
text_plot[text_plot$iso3 == "TON",]$indadequate_intake <- 92
text_plot[text_plot$iso3 == "AUS",]$mean_dist_nrv <- 0.7
text_plot[text_plot$iso3 == "JPN",]$mean_dist_nrv <- 0.66
text_plot[text_plot$iso3 == "COL",]$mean_dist_nrv <- 0.61
text_plot[text_plot$iso3 == "CRI",]$indadequate_intake <- 35
text_plot[text_plot$iso3 == "SLB",]$indadequate_intake <- 34.5
text_plot[text_plot$iso3 == "PAN",]$indadequate_intake <- 43
text_plot[text_plot$iso3 == "TZA",]$indadequate_intake <- 42.5
text_plot[text_plot$iso3 == "IDN",]$indadequate_intake <- 38.5
text_plot[text_plot$iso3 == "IDN",]$mean_dist_nrv <- 0.67

text_plot[text_plot$iso3 == "ECU",]$mean_dist_nrv <- 0.57
text_plot[text_plot$iso3 == "ECU",]$indadequate_intake <- text_plot[text_plot$iso3 == "ECU",]$indadequate_intake - 2
text_plot[text_plot$iso3 == "IDN",]$mean_dist_nrv <- 0.69
text_plot[text_plot$iso3 == "IDN",]$indadequate_intake <- text_plot[text_plot$iso3 == "IDN",]$indadequate_intake - 2
text_plot[text_plot$iso3 == "TZA",]$indadequate_intake <- 41.5
text_plot[text_plot$iso3 == "TZA",]$mean_dist_nrv <- 0.65
text_plot[text_plot$iso3 == "CRI",]$indadequate_intake <- text_plot[text_plot$iso3 == "CRI",]$indadequate_intake - 4
text_plot[text_plot$iso3 == "COL",]$indadequate_intake <- text_plot[text_plot$iso3 == "COL",]$indadequate_intake - 4
text_plot[text_plot$iso3 == "SLB",]$indadequate_intake <- text_plot[text_plot$iso3 == "SLB",]$indadequate_intake - 4

indadequate_intake_nrv_plot_2075 <- indadequate_intake_cr |> 
  dplyr::inner_join(cov_baseline_site) |> 
  dplyr::filter(scenario == "ssp585") |> 
  dplyr::group_by(scenario, country, iso3, year) |> 
  dplyr::summarise(mean_dist_nrv = mean(dist_to_NRV),
                   gravity = mean(gravity),
                   gdp = mean(gdp),
                   hdi = mean(hdi),
                   med = mean(marine_ecosystem_dependency),
                   sd = sd(dist_to_NRV),
                   indadequate_intake = mean(indadequate_intake)) |> 
  ggplot(aes(x = indadequate_intake, y = mean_dist_nrv)) + 
  geom_pointrange(aes(ymin = mean_dist_nrv - sd, ymax = mean_dist_nrv + sd, fill = med), 
                  size = 1, shape = 21, alpha = 0.7) +
  scale_fill_viridis_c(option = "magma") +
  geom_text(data = text_plot, aes(x = indadequate_intake + 2, y = mean_dist_nrv, label = iso3), size = 3.5)  +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal") +
  labs(x = "Inadequate intake (%)", y = "Distance to MIT in 2075", fill = "Marine Ecosystem Dependency")
ggsave(indadequate_intake_nrv_plot_2075, file = "figures/indadequate_intake_nrv_med_plot.png", width = 10, height = 6)

