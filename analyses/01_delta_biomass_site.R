source("R/future_pred_scale_function.R")

load("data/raw-data/performance_bind.Rdata")
load("data/raw-data/decile_save.Rdata")

decile_save[30,] <- c(0, 0)
decile_save <- decile_save[order(decile_save$biomass_class),]
scenarii <- c("ssp126", "ssp245", "ssp370", "ssp585")

color_scale <- ggthemes::colorblind_pal()(8)[-1]

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

load("data/raw-data/00_rls_surveys.Rdata")

rls_surveys$survey_id <- as.character(rls_surveys$survey_id)

time <- as.character("2024":"2075")

future_pred <- lapply(1:length(scenarii), function(i) {
  
  sc_i <- lapply(migration_ensemble, '[[', i)
  
  sc_i <- do.call(rbind, sc_i)
  
  sc_i <- sc_i[,!colnames(sc_i) %in% c("scenario", "model")]
  
  sc_i <- sc_i |> 
    dplyr::left_join(rls_surveys)
  
  sc_i <- sc_i[complete.cases(sc_i),]
  
  delta1 <- sc_i |>
    dplyr::group_by(site_code) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(time), sum)) |>
    dplyr::mutate(delta_pr = (get(time[length(time)]) * 100 / get(time[1])) - 100) |>
    tidyr::pivot_longer(time, names_to = "year", values_to = "biomass") |>
    dplyr::mutate(scenario = scenarii[i])
  
  sc_i <- sc_i |>
    dplyr::group_by(site_code) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(time), sum))
  
  sc_i[,colnames(sc_i) %in% c("2024":"2075")] <- log10(sc_i[,colnames(sc_i) %in% c("2024":"2075")])
  
  delta_log <- unlist(sc_i[,colnames(sc_i) %in% c("2075")] - sc_i[,colnames(sc_i) %in% c("2024")])
  
  sc_i <- sc_i |> 
    dplyr::mutate(delta_log = delta_log)
  
  sc_i <- sc_i |>
    tidyr::pivot_longer(time, names_to = "year", values_to = "biomass") |>
    dplyr::mutate(scenario = scenarii[i]) |> 
    dplyr::select(-biomass)
  
  sc_i <- sc_i |> 
    dplyr::full_join(delta1)

  sc_i$year <- as.integer(sc_i$year)

  return(sc_i)
  
})

future_pred_bind <- do.call(rbind, future_pred)

future_pred_bind2 <- future_pred_bind |> 
  dplyr::ungroup() |> 
  dplyr::filter(year == "2024")


future_pred_bind2[future_pred_bind2$scenario == "ssp126",]$scenario <- "SSP1-2.6"
future_pred_bind2[future_pred_bind2$scenario == "ssp245",]$scenario <- "SSP2-4.5"
future_pred_bind2[future_pred_bind2$scenario == "ssp370",]$scenario <- "SSP3-7.0"
future_pred_bind2[future_pred_bind2$scenario == "ssp585",]$scenario <- "SSP5-8.5"

library(ggplot2)

future_pred_bind2 |> 
  dplyr::ungroup() |> 
  dplyr::group_by(scenario) |> 
  dplyr::summarise(mean_delta = mean(delta_pr),
                   median_delta = median(delta_pr),
                   q05_delta = quantile(delta_pr, probs = 0.05),
                   q95_delta = quantile(delta_pr, probs = 0.95))

future_pred_bind2[future_pred_bind2$scenario == "SSP1-2.6",]$delta_pr

delta_sites_plot <- future_pred_bind2 |> 
  ggplot() + 
  geom_point(aes(x = scenario, y = delta_log), size = 0.4, position = "jitter") +
  geom_boxplot(aes(x = scenario, y = delta_log, fill = scenario), alpha = 0.9, size = 0.6, outlier.shape = NA, show.legend = FALSE) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", size = 1, color = "red") +
  theme_bw() + 
  scale_fill_manual(values = c("SSP1-2.6" = color_scale[3],
                               "SSP2-4.5" = color_scale[2],
                               "SSP3-7.0" = color_scale[4],
                               "SSP5-8.5" = color_scale[1])) +
  labs(x = "", y = "log10(Biomass change)", fill = "", title = "A.") +
  theme(axis.title = element_text(size = 13),
        title = element_text(size = 12),
        axis.text = element_text(size = 11))

# Test ANOVA
anova_data <- future_pred_bind2 |> 
  dplyr::mutate(scenario = stringr::str_replace_all(scenario, "-", ""))
anova_result <- aov(delta_log ~ scenario, data = anova_data)

# Test post-hoc Tukey
tukey_result <- TukeyHSD(anova_result)
tukey_table <- as.data.frame(tukey_result$scenario)

diff <- tukey_table$`p adj` < 0.05
names(diff) <- rownames(tukey_table)

# Conversion en lettres avec multcompView
significance_letters <- multcompView::multcompLetters(diff,
                                                      Letters = letters)$Letters

y <- anova_data |>
  dplyr::group_by(scenario) |>
  dplyr::summarise(y = 1.2) |>
  dplyr::ungroup()

label_data <- data.frame(
  scenario = names(significance_letters),
  Letter = significance_letters
) |>
  dplyr::inner_join(y)

label_data <- label_data |> 
  dplyr::arrange(scenario) |> 
  dplyr::mutate(x = stringr::str_replace(scenario, ".SSP", "_SSP"),
                x = stringr::str_replace_all(x, "SSP1", "SSP1-"),
                x = stringr::str_replace_all(x, "SSP2", "SSP2-"),
                x = stringr::str_replace_all(x, "SSP3", "SSP3-"),
                x = stringr::str_replace_all(x, "SSP5", "SSP5-"))

delta_sites_plot_text <- delta_sites_plot + 
  geom_text(data = label_data,
            aes(x = x, y = y, label = Letter),
            inherit.aes = FALSE,
            size = 5)



delta_species <- pbmcapply::pbmclapply(1:length(migration_ensemble), function(i) {
  
  future_pred_unlog <- future_pred_scale_function(data = migration_ensemble[i],
                                                  unit = "kg",
                                                  time = as.character(c("2024":"2075")), # Define the non-analogous year
                                                  score = TRUE,
                                                  biais = TRUE)
  
  future_pred_unlog$biomass <- log10(future_pred_unlog$biomass + 1)
  
  delta_biomass <- future_pred_unlog[future_pred_unlog$year == "2075",]$biomass - future_pred_unlog[future_pred_unlog$year == "2024",]$biomass
  delta_biomass <- data.frame(delta_biomass = delta_biomass,
                              scenario = unique(future_pred_unlog$scenario),
                              species_name = unique(migration_ensemble[[i]][[1]]$species_name))
  
  future_pred_unlog <- future_pred_unlog |> 
    dplyr::select(delta, scenario) |> 
    unique() |> 
    dplyr::mutate(species_name = unique(migration_ensemble[[i]][[1]]$species_name)) |> 
    dplyr::full_join(delta_biomass) |> 
    dplyr::rename(delta_pr = delta)
  
}, mc.cores = parallel::detectCores() - 1)
delta_species_bind <- do.call(rbind, delta_species) |> 
  dplyr::mutate(model = "full")

load("data/derived-data/nutrient_data.Rdata")
delta_species_bind <- delta_species_bind |> 
  dplyr::filter(species_name %in% nutrient_data$species)

delta_species_bind |> 
  dplyr::mutate(loss = dplyr::case_when(delta_pr < 0 ~ "loss",
                                        delta_pr >= 0 ~ "gain")) |> 
  dplyr::group_by(scenario, loss) |>
  dplyr::summarise(n = dplyr::n())

load("data/raw-data/1c_species_traits_final.Rdata")
species_traits_final <- species_traits_final |> 
  dplyr::ungroup() |> 
  dplyr::rename(species_name = rls_species_name) |> 
  dplyr::select(species_name, trophic_guild, Length, IUCN_inferred_Loiseau23, geographic_range_Albouy19, family, K)
species_traits_final <- species_traits_final[complete.cases(species_traits_final),]
species_traits_final[species_traits_final$trophic_guild == "Herbivores Microvores Detritivores",]$trophic_guild <- "herbivores"

delta_species_trait <- delta_species_bind |> 
  dplyr::inner_join(species_traits_final)

delta_species_trait2 <- delta_species_bind |> 
  dplyr::inner_join(species_traits_final) |> 
  dplyr::filter(scenario %in% c("SSP1-2.6", "SSP5-8.5"))

delta_species_trait2 <- delta_species_trait2 |> 
  dplyr::mutate(trophic_guild2 = paste0(trophic_guild, "_", scenario))

delta_species_trait2 |> 
  dplyr::group_by(scenario) |>
  dplyr::summarise(mean_delta = mean(delta_pr),
                   median_delta = median(delta_pr),
                   q05_delta = quantile(delta_pr, probs = 0.05),
                   q95_delta = quantile(delta_pr, probs = 0.95),
                   min_delta = min(delta_pr),
                   max_delta = max(delta_pr))

delta_species_trait2 |> 
  dplyr::group_by(scenario, trophic_guild) |>
  dplyr::summarise(mean_delta = mean(delta_pr),
                   median_delta = median(delta_pr),
                   q05_delta = quantile(delta_pr, probs = 0.05),
                   q95_delta = quantile(delta_pr, probs = 0.95),
                   min_delta = min(delta_pr),
                   max_delta = max(delta_pr))


library(ggplot2)

delta_text <- data.frame(label = c("SSP1-2.6", "SSP5-8.5"),
                         x = c(4.5, 13.5),
                         y = c(0.8, 0.8))

delta_species_trait2$trophic_guild2 <- factor(delta_species_trait2$trophic_guild2, levels = c("herbivores_SSP1-2.6",
                                                                                              "macroinvertivore_SSP1-2.6",
                                                                                              "corallivore_SSP1-2.6",
                                                                                              "crustacivore_SSP1-2.6",
                                                                                              "piscivore_SSP1-2.6",
                                                                                              "sessile invertivores_SSP1-2.6",
                                                                                              "microinvertivore_SSP1-2.6",
                                                                                              "planktivore_SSP1-2.6",
                                                                                              " ",
                                                                                              "herbivores_SSP5-8.5",
                                                                                              "macroinvertivore_SSP5-8.5",
                                                                                              "corallivore_SSP5-8.5",
                                                                                              "crustacivore_SSP5-8.5",
                                                                                              "piscivore_SSP5-8.5",
                                                                                              "sessile invertivores_SSP5-8.5",
                                                                                              "microinvertivore_SSP5-8.5",
                                                                                              "planktivore_SSP5-8.5"))

delta_species_trait2 |> 
  dplyr::filter(scenario == "SSP5-8.5") |> 
  dplyr::mutate(trophic_guild2 = forcats::fct_reorder(trophic_guild2, delta_biomass)) |> 
  ggplot() + 
  geom_point(aes(x = trophic_guild2, y = delta_biomass), size = 0.6, position = "jitter") +
  geom_boxplot(aes(x = trophic_guild2, y = delta_biomass, fill = trophic_guild), alpha = 0.8, size = 0.4, outlier.shape = NA) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", size = 1, color = "red") +
  theme_bw() + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(n = 8, name = "Dark2")) +
  labs(x = "", y = "log10(Biomass change + 1)", fill = "", title = "B.") +
  theme(axis.text.x = element_blank(),
        title = element_text(size = 13),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        legend.direction = "horizontal")

delta_species_plot <- delta_species_trait2 |> 
  ggplot() + 
  geom_point(aes(x = trophic_guild2, y = delta_biomass), size = 0.6, position = "jitter") +
  geom_boxplot(aes(x = trophic_guild2, y = delta_biomass, fill = trophic_guild), alpha = 0.8, size = 0.4, outlier.shape = NA) +
  geom_text(data = delta_text, aes(x = x, y = y, label = label), size = 5) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", size = 1, color = "red") +
  theme_bw() + 
  scale_x_discrete(drop = FALSE) + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(n = 8, name = "Dark2")) +
  labs(x = "", y = "log10(Biomass change + 1)", fill = "", title = "B.") +
  theme(axis.text.x = element_blank(),
        title = element_text(size = 13),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        legend.direction = "horizontal")

# Test ANOVA
anova_data <- delta_species_trait2 |> 
  dplyr::mutate(scenario = stringr::str_replace_all(scenario, "-", ""))
anova_result <- aov(delta_biomass ~ interaction(trophic_guild, scenario), data = anova_data)

# Test post-hoc Tukey
tukey_result <- TukeyHSD(anova_result)
tukey_table <- as.data.frame(tukey_result$`interaction(trophic_guild, scenario)`)

diff <- tukey_table$`p adj` < 0.05
names(diff) <- rownames(tukey_table)

# Conversion en lettres avec multcompView
significance_letters <- multcompView::multcompLetters(diff,
                                                      Letters = letters)$Letters

y <- anova_data |>
  dplyr::group_by(scenario, trophic_guild) |>
  dplyr::summarise(y = 0.6) |>
  dplyr::ungroup() |> 
  dplyr::mutate(interaction = paste0(trophic_guild, ".", scenario))


label_data <- data.frame(
  interaction = names(significance_letters),
  Letter = significance_letters
) |>
  dplyr::inner_join(y)

label_data <- label_data |> 
  dplyr::arrange(scenario) |> 
  dplyr::mutate(x = stringr::str_replace(interaction, ".SSP", "_SSP"),
                x = stringr::str_replace_all(x, "SSP1", "SSP1-"),
                x = stringr::str_replace_all(x, "SSP5", "SSP5-"))

delta_species_plot_text <- delta_species_plot + 
  geom_text(data = label_data,
            aes(x = x, y = y, label = Letter),
            inherit.aes = FALSE,
            size = 4)

delta_plot <- patchwork::wrap_plots(delta_sites_plot_text, delta_species_plot_text, ncol = 1)

ggsave("figures/delta_site_trait_plot_migration.png", delta_plot, width = 9, height = 10)
