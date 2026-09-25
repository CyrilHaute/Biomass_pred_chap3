# Load future reef fish biomass

color_scale <- ggthemes::colorblind_pal()(8)[-1]

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
                                                                                     "longitude",
                                                                                     "latitude",
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
  dplyr::group_by(site_code, depth, latitude, longitude, scenario) |> 
  dplyr::summarise(delta_biomass = (dplyr::last(biomass) * 100) / dplyr::first(biomass) - 100) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(site_code = paste0(site_code, "_", depth))

delta_biomass <- biomass_site

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

load("data/raw-data/1c_species_traits_final.Rdata")

species_traits_final <- species_traits_final[which(is.na(species_traits_final$trophic_guild) == FALSE),]
species_traits_final[species_traits_final$trophic_guild == "Herbivores Microvores Detritivores",]$trophic_guild <- "herbivores"

troph_species <- species_traits_final |> 
  dplyr::select(fishbase_name, Troph) |> 
  dplyr::rename(species_name = fishbase_name) |> 
  dplyr::group_by(species_name) |> 
  dplyr::summarise(Troph = mean(Troph)) |> 
  dplyr::ungroup() |> 
  dplyr::inner_join(species_traits_final[,colnames(species_traits_final) %in% c("fishbase_name", "trophic_guild", "Length", "K", "geographic_range_Albouy19")] |> 
                      dplyr::rename(species_name = fishbase_name)) |> 
  unique() |> 
  tidyr::drop_na()

# How many species per site ?

n_species <- biomass_ssp_species_site |> 
  dplyr::group_by(site_code, depth, latitude, longitude, scenario) |> 
  dplyr::summarise(n_species = dplyr::n_distinct(species_name))

species_trait <- biomass_ssp_species_site |> 
  dplyr::inner_join(troph_species)

# Estimate species characteristics per site

troph_site <- species_trait |> 
  dplyr::group_by(site_code, depth, longitude, latitude, scenario) |> 
  dplyr::summarise(mean_troph = mean(Troph),
                   mean_length = mean(Length),
                   mean_geo = mean(geographic_range_Albouy19),
                   mean_k = mean(K))

# Number of species per troph cat and per site

n_troph_site <- species_trait |> 
  dplyr::group_by(site_code, depth, longitude, latitude, scenario, trophic_guild) |> 
  dplyr::summarise(n_species = dplyr::n_distinct(species_name)) |> 
  tidyr::pivot_wider(names_from = trophic_guild, 
                     values_from = n_species,
                     names_prefix = "n_",
                     values_fill = 0)

biomasss_troph_site <- biomass_ssp_all_info |> 
  dplyr::select(c(site_code, depth, longitude, latitude, species_name, scenario, `2075`)) |> 
  dplyr::rename(biomass = `2075`) |> 
  dplyr::inner_join(species_traits_final |> 
                      dplyr::rename(species_name = fishbase_name) |> 
                      dplyr::select(species_name, trophic_guild) |> 
                      unique()) |> 
  dplyr::group_by(site_code, depth, longitude, latitude, scenario, trophic_guild) |> 
  dplyr::summarise(biomass = sum(biomass)) |> 
  dplyr::mutate(biomass = log10(biomass + 1)) |> 
  tidyr::pivot_wider(names_from = trophic_guild, 
                     values_from = biomass,
                     names_prefix = "biomass_",
                     values_fill = 0)

biomass_site_troph <- biomass_longer |> 
  dplyr::filter(year %in% c("2024", "2075")) |>
  dplyr::inner_join(species_traits_final |> 
                      dplyr::rename(species_name = fishbase_name) |> 
                      dplyr::select(species_name, trophic_guild) |> 
                      unique()) |> 
  dplyr::group_by(site_code, depth, latitude, longitude, scenario, year, trophic_guild) |> 
  dplyr::summarise(biomass = sum(biomass_g_500m2)) |> 
  dplyr::mutate(biomass = log10(biomass + 1))

delta_biomass_troph <- biomass_site_troph |>
  dplyr::group_by(site_code, depth, latitude, longitude, scenario, trophic_guild) |>
  dplyr::summarise(delta_biomass = dplyr::last(biomass) - dplyr::first(biomass)) |>
  dplyr::ungroup() |>
  dplyr::mutate(site_code = paste0(site_code, "_", depth)) |>
  tidyr::pivot_wider(names_from = trophic_guild,
                     values_from = delta_biomass,
                     names_prefix = "deltabiomass_",
                     values_fill = 0)

# Merge all species covariates

biomass_site_n <- n_species |> 
  dplyr::inner_join(troph_site) |> 
  dplyr::inner_join(n_troph_site) |> 
  # dplyr::inner_join(biomasss_troph_site) |> 
  dplyr::inner_join(biomass_site |> 
                      dplyr::filter(year == "2075") |> 
                      dplyr::rename(biomass_total = biomass) |> 
                      dplyr::mutate(biomass_total = log10(biomass_total + 1))) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(site_code = paste0(site_code, "_", depth)) |> 
  dplyr::inner_join(delta_biomass) |> 
  dplyr::inner_join(delta_biomass_troph)

# Add env, hab, hum covariates

load("data/raw-data/env_future_raw.Rdata")
load("data/raw-data/rls_covariates_train.RData")
load("outputs/site_nrv_migration2.Rdata")

site <- rls_surveys[, colnames(rls_surveys) %in% c("survey_id", "site_code", "depth", "longitude", "latitude")] |> 
  unique()
site$survey_id <- as.numeric(site$survey_id)

rls_covariates_train$survey_id <- as.numeric(rls_covariates_train$survey_id)

cov_baseline_site <- rls_covariates_train |> 
  dplyr::inner_join(site) |> 
  dplyr::select(-c(gdp, gravity)) |> 
  dplyr::group_by(site_code, depth, longitude, latitude, scenario) |> 
  dplyr::summarise(dplyr::across(c(hdi:marine_ecosystem_dependency, Plateau_500m:Lagoon), mean)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(site_code = paste0(site_code, "_", depth))

nrv_2075 <- site_nrv_migration[-1,] |> 
  dplyr::filter(year == "2075") |> 
  dplyr::select(c(site_code, depth, latitude, longitude, country, dist_to_NRV, scenario)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(site_code = paste0(site_code, "_", depth)) |>
  dplyr::as_tibble()

cov_2075 <- env_future_raw[["2075"]]
cov_2075_site <- cov_2075 |> 
  dplyr::inner_join(site) |> 
  dplyr::group_by(site_code, depth, longitude, latitude, scenario) |> 
  dplyr::summarise(dplyr::across(chl_2075:gravity_2075, mean)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(site_code = paste0(site_code, "_", depth)) |> 
  dplyr::inner_join(biomass_site_n) |> 
  dplyr::inner_join(cov_baseline_site) |> 
  dplyr::inner_join(nrv_2075)

cov_2075_site |> 
  dplyr::group_by(country) |> 
  dplyr::summarise(n = dplyr::n()) |> 
  as.data.frame()

cov_2075_site$gravity_2075 <- log10(cov_2075_site$gravity_2075 + 1)


site_nrv_h <- site_nrv_migration |> 
  dplyr::select(site_code, depth, latitude, longitude, realm, country, scenario, dist_to_NRV)

delta <- site_nrv_h[-1,] |> 
  dplyr::group_by(site_code, depth, latitude, longitude, realm, country, scenario) |> 
  dplyr::summarise(delta_dist_to_NRV = (dplyr::last(dist_to_NRV) * 100) / dplyr::first(dist_to_NRV) - 100) |>
  dplyr::ungroup() |> 
  dplyr::mutate(site_code = paste0(site_code, "_", depth)) |>
  dplyr::select(-c(depth, realm, country))

cov_2075_site <- cov_2075_site |> 
  dplyr::inner_join(delta)

cov_2075_site$scenario <- as.factor(cov_2075_site$scenario)

dtf_var_name <- data.frame(covariates = colnames(cov_2075_site)[!colnames(cov_2075_site) %in% c("iso3",
                                                                                                "site_code",
                                                                                                "dist_to_NRV",
                                                                                                "country",
                                                                                                "year",
                                                                                                "longitude",
                                                                                                "latitude",
                                                                                                "delta_dist_to_NRV")],
                           clear_name = c("Depth", "scenario", "Sea Water Chlorophyll", "pH", "Sea Surface Salinity", "Sea Surface Temperature", 
                                          "Gross Domestic Product", "Human Gravity", "N species", "Mean trophic level", "Mean species length",
                                          "Mean species geographic range", "Mean species K", 
                                          "N corallivore", "N crustacivore",
                                          "N herbivore", "N macroinvertivore", "N microinvertivore",
                                          "N piscivore", "N planktivore", "N sessile invertivore",
                                          "Biomass total", "Biomass change",
                                          "Biomass change corallivore", "Biomass change crustacivore", "Biomass change herbivore", 
                                          "Biomass change macroinvertivore", "Biomass change microinvertiore", "Biomass change piscivore",
                                          "Biomass change planktivore", "Biomass change sessile invertivore",
                                          "Human Development Index", "Number of NGO", "Marine Ecosystem Dependency", "Plateau (%)", "Reef Crest (%)",
                                          "Terrestrial Reef Flat (%)", "Patch Reefs (%)", "Reef extent", "Reef Slope (%)", "Reef Flat (%)", "Lagoon")) |> 
  dplyr::mutate(cov_type = dplyr::case_when(clear_name %in% c("Depth", "Plateau (%)", "Reef Crest (%)", "Terrestrial Reef Flat (%)", 
                                                              "Patch Reefs (%)", "Reef extent", "Reef Slope (%)", "Reef Flat (%)", 
                                                              "Lagoon") ~ "HAB",
                                            clear_name %in% c("Sea Water Chlorophyll", "pH", "Sea Surface Salinity", "Sea Surface Temperature") ~ "ENV",
                                            clear_name %in% c("Gross Domestic Product", "Human Gravity", "Human Development Index", 
                                                              "Marine Ecosystem Dependency", "Number of NGO") ~ "HUM",
                                            clear_name %in% c("N species", "Mean trophic level", "Mean species length",
                                                              "Mean species geographic range", "Mean species K", "N corallivore", "N crustacivore",
                                                              "N herbivore", "N macroinvertivore", "N microinvertivore",
                                                              "N piscivore", "N planktivore", "N sessile invertivore",
                                                              "Biomass change corallivore", "Biomass change crustacivore", "Biomass change herbivore", 
                                                              "Biomass change macroinvertivore", "Biomass change microinvertiore", "Biomass change piscivore",
                                                              "Biomass change planktivore", "Biomass change sessile invertivore",
                                                              "Biomass total", "Biomass change") ~ "BIOT"))

model_nrv <- ranger::ranger(x = cov_2075_site[!colnames(cov_2075_site) %in% c("iso3",
                                                                              "site_code",
                                                                              "dist_to_NRV",
                                                                              "country",
                                                                              "year",
                                                                              "longitude",
                                                                              "latitude",
                                                                              "delta_dist_to_NRV")],
                            y = unlist(cov_2075_site[colnames(cov_2075_site) %in% "delta_dist_to_NRV"]),
                            num.trees = 1000,
                            importance = "impurity")
save(model_nrv, file = "outputs/nrv_model_RF.Rdata")

load("outputs/nrv_model_RF.Rdata")

source("R/importance_and_partial_plot.R")

var_imp_dtf <- var_imp_function(model_nrv) |> 
  dplyr::filter(!covariates %in% "scenario")
var_imp_dtf <- var_imp_dtf |> 
  dplyr::inner_join(dtf_var_name) |> 
  dplyr::select(-covariates) |> 
  dplyr::rename(covariates = clear_name)

pal_contribution <- c(RColorBrewer::brewer.pal(n = 9, name = "Set1"), PNWColors::pnw_palette("Bay", 6, type = "continuous"))

imp_var_nrv <- var_imp_plot_function(var_imp_dtf = dplyr::arrange(var_imp_dtf, desc(delta_pr))[1:15,],
                                     title = paste0("A. Distance to MIT change,", "  r² = ", round(model_nrv$r.squared, 2)))

var_imp_dtf |> 
  dplyr::group_by(cov_type) |> 
  dplyr::summarise(n = dplyr::n(),
                   delta_sum = sum(delta_pr),
                   delta_sum_n = delta_sum/n)
#   cov_type     n      delta_sum   delta_sum_n
# 1 BIOT        23      57.1        2.48
# 2 ENV          4      14.9        3.73
# 3 HAB          9      12.4        1.38
# 4 HUM          5      15.3        3.07

select_cov <- var_imp_function(model_nrv)$covariates[order(var_imp_function(model_nrv)$delta_pr, decreasing = TRUE)]
select_cov_10 <- select_cov[1:10]
select_cov_15 <- select_cov[1:15]

sum(sort(var_imp_dtf$delta_pr, decreasing = TRUE)[1:10])
# sum 10 best = 45.85671%

cov_2075_site_ssp126 <- cov_2075_site |> 
  dplyr::filter(scenario == "ssp126") |> 
  dplyr::select(-scenario)
cov_2075_site_ssp245 <- cov_2075_site |> 
  dplyr::filter(scenario == "ssp245") |> 
  dplyr::select(-scenario)
cov_2075_site_ssp370 <- cov_2075_site |> 
  dplyr::filter(scenario == "ssp370") |> 
  dplyr::select(-scenario)
cov_2075_site_ssp585 <- cov_2075_site |> 
  dplyr::filter(scenario == "ssp585") |> 
  dplyr::select(-scenario)

model_nrv_126 <- ranger::ranger(x = cov_2075_site_ssp126[!colnames(cov_2075_site_ssp126) %in% c("iso3",
                                                                                  "scenario",
                                                                                  "site_code",
                                                                                  "dist_to_NRV",
                                                                                  "country",
                                                                                  "year",
                                                                                  "longitude",
                                                                                  "latitude",
                                                                                  "delta_dist_to_NRV")],
                                y = unlist(cov_2075_site_ssp126[colnames(cov_2075_site_ssp126) %in% "delta_dist_to_NRV"]),
                                num.trees = 1000,
                                importance = "impurity")
model_nrv_245 <- ranger::ranger(x = cov_2075_site_ssp245[!colnames(cov_2075_site_ssp245) %in% c("iso3",
                                                                                                "scenario",
                                                                                                "site_code",
                                                                                                "dist_to_NRV",
                                                                                                "country",
                                                                                                "year",
                                                                                                "longitude",
                                                                                                "latitude",
                                                                                                "delta_dist_to_NRV")],
                                y = unlist(cov_2075_site_ssp245[colnames(cov_2075_site_ssp245) %in% "delta_dist_to_NRV"]),
                                num.trees = 1000,
                                importance = "impurity")
model_nrv_370 <- ranger::ranger(x = cov_2075_site_ssp370[!colnames(cov_2075_site_ssp370) %in% c("iso3",
                                                                                                "scenario",
                                                                                                "site_code",
                                                                                                "dist_to_NRV",
                                                                                                "country",
                                                                                                "year",
                                                                                                "longitude",
                                                                                                "latitude",
                                                                                                "delta_dist_to_NRV")],
                                y = unlist(cov_2075_site_ssp370[colnames(cov_2075_site_ssp370) %in% "delta_dist_to_NRV"]),
                                num.trees = 1000,
                                importance = "impurity")
model_nrv_585 <- ranger::ranger(x = cov_2075_site_ssp585[!colnames(cov_2075_site_ssp585) %in% c("iso3",
                                                                                                "scenario",
                                                                                                "site_code",
                                                                                                "dist_to_NRV",
                                                                                                "country",
                                                                                                "year",
                                                                                                "longitude",
                                                                                                "latitude",
                                                                                                "delta_dist_to_NRV")],
                                y = unlist(cov_2075_site_ssp585[colnames(cov_2075_site_ssp585) %in% "delta_dist_to_NRV"]),
                                num.trees = 1000,
                                importance = "impurity")

partial_var_nrv_ssp126_10 <- partial_dep_function(model = model_nrv_126,
                                                  covariates = select_cov_10,
                                                  data = cov_2075_site_ssp126,
                                                  reponse_name = "delta_dist_to_NRV")
partial_var_nrv_ssp245_10 <- partial_dep_function(model = model_nrv_245,
                                                  covariates = select_cov_10,
                                                  data = cov_2075_site_ssp245,
                                                  reponse_name = "delta_dist_to_NRV")
partial_var_nrv_ssp370_10 <- partial_dep_function(model = model_nrv_370,
                                                  covariates = select_cov_10,
                                                  data = cov_2075_site_ssp370,
                                                  reponse_name = "delta_dist_to_NRV")
partial_var_nrv_ssp585_10 <- partial_dep_function(model = model_nrv_585,
                                                  covariates = select_cov_10,
                                                  data = cov_2075_site_ssp585,
                                                  reponse_name = "delta_dist_to_NRV")

partial_var_nrv_10 <- lapply(1:length(select_cov_10), function(i) {
  
  ssp126 <- partial_var_nrv_ssp126_10[[i]]
  ssp126$scenario <- "SSP1-2.6"
  
  ssp245 <- partial_var_nrv_ssp245_10[[i]]
  ssp245$scenario <- "SSP2-4.5"
  
  ssp370 <- partial_var_nrv_ssp370_10[[i]]
  ssp370$scenario <- "SSP3-7.0"
  
  ssp585 <- partial_var_nrv_ssp585_10[[i]]
  ssp585$scenario <- "SSP5-8.5"
  
  ssp <- ssp126 |> 
    dplyr::full_join(ssp245) |> 
    dplyr::full_join(ssp370) |> 
    dplyr::full_join(ssp585)
  
})
names(partial_var_nrv_10) <- c("Sea Surface Temperature",
                               "Sea Surface Salinity",
                               "Biomass change",
                               "Biomass change herbi.",
                               "Human Gravity",
                               "Biomass change macro.",
                               "Biomass change pisci.",
                               "Biomass total",
                               "Gross Domestic Product",
                               "Reef extent")

partial_plot_nrv_10 <- partial_plot_function(partial_data = partial_var_nrv_10,
                                             color_values = c("SSP1-2.6" = color_scale[3],
                                                              "SSP2-4.5" = color_scale[2],
                                                              "SSP3-7.0" = color_scale[4],
                                                              "SSP5-8.5" = color_scale[1]),
                                             ncol = 2)

imp_partial_nrv_plot_migration <- imp_var_nrv + 
  patchwork::inset_element(partial_plot_nrv_10, left = 0.55, bottom = 0.01, right = 1, top = 0.85)
ggsave(imp_partial_nrv_plot_migration, file = "figures/imp_partial_nrv_plot_migration3.png", height = 15, width = 21)





partial_var_nrv_ssp126_15 <- partial_dep_function(model = model_nrv_126,
                                                  covariates = select_cov_15,
                                                  data = cov_2075_site_ssp126,
                                                  reponse_name = "delta_dist_to_NRV")
partial_var_nrv_ssp245_15 <- partial_dep_function(model = model_nrv_245,
                                                  covariates = select_cov_15,
                                                  data = cov_2075_site_ssp245,
                                                  reponse_name = "delta_dist_to_NRV")
partial_var_nrv_ssp370_15 <- partial_dep_function(model = model_nrv_370,
                                                  covariates = select_cov_15,
                                                  data = cov_2075_site_ssp370,
                                                  reponse_name = "delta_dist_to_NRV")
partial_var_nrv_ssp585_15 <- partial_dep_function(model = model_nrv_585,
                                                  covariates = select_cov_15,
                                                  data = cov_2075_site_ssp585,
                                                  reponse_name = "delta_dist_to_NRV")

partial_var_nrv_15 <- lapply(1:length(select_cov_15), function(i) {
  
  ssp126 <- partial_var_nrv_ssp126_15[[i]]
  ssp126$scenario <- "SSP1-2.6"
  
  ssp245 <- partial_var_nrv_ssp245_15[[i]]
  ssp245$scenario <- "SSP2-4.5"
  
  ssp370 <- partial_var_nrv_ssp370_15[[i]]
  ssp370$scenario <- "SSP3-7.0"
  
  ssp585 <- partial_var_nrv_ssp585_15[[i]]
  ssp585$scenario <- "SSP5-8.5"
  
  ssp <- ssp126 |> 
    dplyr::full_join(ssp245) |> 
    dplyr::full_join(ssp370) |> 
    dplyr::full_join(ssp585)
  
})
names(partial_var_nrv_15) <- c("Sea Surface Temperature",
                               "Sea Surface Salinity",
                               "Biomass change",
                               "Biomass change herbi.",
                               "Human Gravity",
                               "Biomass change macro.",
                               "Biomass change pisci.",
                               "Biomass total",
                               "Gross Domestic Product",
                               "Reef extent",
                               "Human Development Index",
                               "Mean species length",
                               "N planktivore",
                               "N species",
                               "Mean species K")

partial_plot_nrv_15 <- partial_plot_function(partial_data = partial_var_nrv_15,
                                             color_values = c("SSP1-2.6" = color_scale[3],
                                                              "SSP2-4.5" = color_scale[2],
                                                              "SSP3-7.0" = color_scale[4],
                                                              "SSP5-8.5" = color_scale[1]),
                                             ncol = 3)
ggsave(partial_plot_nrv_15, file = "figures/partial_nrv_plot_migration_15.png", height = 11, width = 11)






cov_2075_site_585 <- cov_2075_site |> 
  dplyr::filter(scenario == "ssp585")

cov_2075_site_585[!colnames(cov_2075_site_585) %in% c("site_code",
                                                      "longitude",
                                                      "latitude",
                                                      "scenario",
                                                      "year",
                                                      "country",
                                                      "dist_to_NRV",
                                                      "delta_dist_to_NRV")] <- scale(cov_2075_site_585[!colnames(cov_2075_site_585) %in% c("site_code",
                                                                                                                                           "longitude",
                                                                                                                                           "latitude",
                                                                                                                                           "scenario",
                                                                                                                                           "year",
                                                                                                                                           "country",
                                                                                                                                           "dist_to_NRV",
                                                                                                                                           "delta_dist_to_NRV")])

############ SEM ############ 

cov_biot_dir <- dtf_var_name |> 
  dplyr::filter(covariates %in% select_cov_10,
                cov_type == "BIOT") |> 
  dplyr::select(covariates) |> 
  unlist()

cov_abiot_indir <- dtf_var_name |> 
  dplyr::filter(covariates %in% select_cov_10,
                cov_type %in% c("ENV", "HAB", "HUM")) |> 
  dplyr::select(covariates) |> 
  unlist()

formula_biot <- paste0("delta_dist_to_NRV ~ ", paste0(c(cov_biot_dir, cov_abiot_indir), collapse = " + "))

mod_biot <- lm(formula_biot , cov_2075_site_585)
test_aic <- MASS::stepAIC(mod_biot, k = log(length(c(cov_biot_dir, cov_abiot_indir))))

cov_biot_abiot <- c(cov_biot_dir, cov_abiot_indir)

new_cov_biot <- cov_biot_abiot[!cov_biot_abiot %in% gsub("- ", "", test_aic$anova$Step[-1])]

new_formula_biot <- paste0("delta_dist_to_NRV ~ ", paste0(new_cov_biot, collapse = " + "))

new_mod_biot <- list(lm(new_formula_biot , cov_2075_site_585))
names(new_mod_biot) <- "delta_dist_to_NRV"

cov_abiot <- cov_abiot_indir

new_cov_biot <- new_cov_biot[!new_cov_biot %in% cov_abiot_indir]

mod_abiot <- lapply(1:length(new_cov_biot), function(i) {
  
  formula_i <- paste0(new_cov_biot[i], " ~ ", paste0(cov_abiot, collapse = " + "))
  
  mod_i <- lm(formula_i , cov_2075_site_585)
  
})
names(mod_abiot) <- new_cov_biot

new_mod_abiot <- lapply(1:length(mod_abiot), function(i) {
  
  test_aic_i <- MASS::stepAIC(mod_abiot[[i]], k = log(length(cov_abiot)))
  
  new_cov_abiot_i <- cov_abiot[!cov_abiot %in% gsub("- ", "", test_aic_i$anova$Step[-1])]
  
  new_formula_abiot_i <- paste0(names(mod_abiot)[[i]], " ~ ", paste0(new_cov_abiot_i, collapse = " + "))
  
  new_mod_abiot_i <- lm(new_formula_abiot_i , cov_2075_site_585)
  
})
names(new_mod_abiot) <- new_cov_biot


all_models <- c(new_mod_biot, new_mod_abiot)

sem <- piecewiseSEM::psem(all_models[[1]],
                          all_models[[2]],
                          all_models[[3]],
                          all_models[[4]],
                          all_models[[5]],
                          all_models[[6]])

a_sem <- anova(sem)

piecewiseSEM::coefs(sem)

piecewiseSEM::rsquared(sem)

coefs <- piecewiseSEM::coefs(sem, standardize = "scale")
coefs_sig <- subset(coefs, P.Value < 0.05)

edges <- paste0(
  coefs_sig$Predictor, " -> ", coefs_sig$Response,
  " [penwidth = ", pmax(abs(coefs_sig$Estimate) * 3, 0.5),
  ", color = \"", ifelse(coefs_sig$Estimate > 0, "#009E73", "#E69F00"), "\"]"
)

abiot_var <- unique(coefs_sig$Predictor)[unique(coefs_sig$Predictor) %in% dtf_var_name[dtf_var_name$cov_type %in% c("ENV", "HAB", "HUM"),]$covariates]
abiot_var_clear <- dtf_var_name[dtf_var_name$covariates %in% abiot_var,]$clear_name

biot_var <- unique(c(unique(coefs_sig$Predictor)[unique(coefs_sig$Predictor) %in% dtf_var_name[dtf_var_name$cov_type %in% c("BIOT"),]$covariates],
                     unique(coefs_sig$Response)[unique(coefs_sig$Response) %in% dtf_var_name[dtf_var_name$cov_type %in% c("BIOT"),]$covariates]))
biot_var_clear <- dtf_var_name[dtf_var_name$covariates %in% biot_var,]$clear_name


graph_code <- paste0(
  
  "digraph SEM {

  graph [label = 'B. Structural Equation Modeling path diagram', labelloc = 't', fontsize = 17, layout = dot, rankdir = LR, ratio = 0.5]
  
  node [shape = box, fontsize = 10]
  
  edge [fontsize = 10, arrowhead = vee]",
  
  # Clear covariate names
  paste0(abiot_var, ' [label = "', abiot_var_clear, '"]', collapse = "\n"),

  paste0(biot_var, ' [label = "', biot_var_clear, '"]', collapse = "\n"),
  
  'delta_dist_to_NRV [label = "Distance to MIT change"]',
  
  # Rank
  paste0("{rank = same; ", paste0(unique(coefs_sig$Predictor)[unique(coefs_sig$Predictor) %in% cov_abiot_indir], collapse = "; "), "}"), "
  ",
  paste0("{rank = same; ", paste0(unique(coefs_sig$Response)[-1], collapse = "; "), "}"), "
  ",
  paste0("{rank = same; ", unique(coefs_sig$Response)[1], "}"), "
  ",
  
  # Arrows
  paste(edges, collapse = ";"), "

}
")

DiagrammeR::grViz(graph_code)
