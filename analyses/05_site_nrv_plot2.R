load("outputs/site_nrv_migration2.Rdata")

site_nrv_migration[site_nrv_migration$type == "nrv",]$type <- "MIT"

site_nrv_h <- site_nrv_migration |> 
  dplyr::mutate(highlight = dplyr::case_when(type == "MIT" ~ TRUE,
                                             .default = FALSE)) 
site_nrv_h[site_nrv_h$type == "MIT",]$year <- "2024"

site_nrv_point <- site_nrv_h[1,]

remove_country <- site_nrv_h |> 
  dplyr::filter(year == "2024",
                scenario == "ssp585") |> 
  dplyr::group_by(country) |> 
  dplyr::summarise(n = dplyr::n()) |> 
  dplyr::filter(n >= 5)

site_nrv_h <- site_nrv_h |> 
  dplyr::filter(country %in% remove_country$country)

site_nrv_h <- rbind(site_nrv_point, site_nrv_h)

library(ggplot2)

delta <- site_nrv_h[-1,] |> 
  dplyr::group_by(site_code, depth, latitude, longitude, realm, country, scenario) |> 
  dplyr::summarise(delta_dist_to_NRV = (dplyr::last(dist_to_NRV) * 100) / dplyr::first(dist_to_NRV) - 100) |>
  dplyr::ungroup() |> 
  dplyr::mutate(site_code = paste0(site_code, "_", depth)) |>
  dplyr::select(-depth)

delta[delta$scenario == "ssp126",]$scenario <- "SSP1-2.6"
delta[delta$scenario == "ssp245",]$scenario <- "SSP2-4.5"
delta[delta$scenario == "ssp370",]$scenario <- "SSP3-7.0"
delta[delta$scenario == "ssp585",]$scenario <- "SSP5-8.5"

site_nrv_h[-1,] |> 
  dplyr::filter(scenario == "ssp585") |> 
  dplyr::ungroup() |> 
  dplyr::mutate(site_code = paste0(site_code, "_", depth)) |>
  dplyr::select(-depth) |> 
  dplyr::mutate(country = forcats::fct_reorder(country, dist_to_NRV)) |> 
  ggplot() +
  geom_boxplot(aes(y = country, x = dist_to_NRV, fill = year))


color_scale <- ggthemes::colorblind_pal()(8)[-1]

delta_country_stat <- delta |> 
  dplyr::group_by(scenario, country) |> 
  dplyr::summarise(mean_delta = mean(delta_dist_to_NRV),
                   sd_delta = sd(delta_dist_to_NRV),
                   state = ifelse(mean_delta < 0, "get closer", "get further"))

load("data/raw-data/iso.Rdata")

iso <- hdi_csv_rls |> 
  dplyr::select(country, iso3) |> 
  # dplyr::filter(country %in% unique(site_nrv_h_plot$country)) |> 
  unique()

delta_stat <- delta |> 
  dplyr::filter(scenario == "SSP5-8.5") |> 
  dplyr::group_by(scenario, country) |> 
  dplyr::summarise(median_delta = median(delta_dist_to_NRV),
                   sd_delta = sd(delta_dist_to_NRV))

delta_nrv_country_plot <- delta |>
  dplyr::inner_join(iso) |>
  dplyr::filter(iso3 %in% c("CHN", "TZA", "MOZ", "WLF", "WSM", "BRA", "AUS", "TON", "JPN", "COK", "PYF")) |>
  dplyr::filter(scenario %in% c("SSP1-2.6",
                                "SSP5-8.5")) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(country = paste0(country, " (", iso3, ")"),
                country = forcats::fct_reorder(country, delta_dist_to_NRV)) |>
  ggplot(aes(x = delta_dist_to_NRV, y = country, fill = scenario)) +
  geom_boxplot(size = 0.3, outlier.size = 0.5) +
  scale_fill_manual(values = c("SSP1-2.6" = color_scale[3],
                               "SSP5-8.5" = color_scale[1])) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", size = 1, color = "red") +
  theme_bw() + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 17),
        title = element_text(size = 19),
        legend.position = c(0.85, 0.85),
        legend.direction = "vertical") + 
  labs(y = "", x = "Distance to MIT change (%)", fill = "", title = "B.")


site_dist <- site_nrv_h[-1,] |> 
  dplyr::filter(year == "2075") |>
  # dplyr::filter(year == "2024") |> # to check median, mean, sd distance to MIT among reefs on the baseline
  dplyr::select(site_code, depth, latitude, longitude, scenario, dist_to_NRV, year) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(site_code = paste0(site_code, "_", depth)) |> 
  dplyr::filter(scenario == "ssp585") |> 
  dplyr::select(-scenario)

delta_vs_dist <- delta |>
  dplyr::filter(scenario == "SSP5-8.5") |>
  dplyr::inner_join(site_dist)

delta_vs_dist |> 
  ggplot(aes(y = delta_dist_to_NRV, x = dist_to_NRV)) +
  geom_point(aes(color = country))

delta_vs_dist |> 
  # dplyr::filter(country != "Brazil") |>
  dplyr::group_by(country, scenario) |> 
  dplyr::summarise(mean_delta_dist_nrv = mean(delta_dist_to_NRV),
                   sd = sd(delta_dist_to_NRV),
                   mean_dist_nrv = mean(dist_to_NRV)) |> 
  ggplot(aes(x = mean_dist_nrv, y = mean_delta_dist_nrv)) + 
  geom_pointrange(aes(ymin = mean_delta_dist_nrv - sd, ymax = mean_delta_dist_nrv + sd, fill = country), 
                  size = 1, shape = 21, alpha = 0.9) +
  facet_wrap(~scenario) +
  geom_smooth(method = "lm") +
  theme_bw()

delta_vs_dist |> 
  dplyr::group_by(scenario) |> 
  dplyr::summarise(q05 = quantile(dist_to_NRV, probs = 0.05),
                   median = median(dist_to_NRV),
                   mean = mean(dist_to_NRV),
                   sd = sd(dist_to_NRV),
                   q95 = quantile(dist_to_NRV, probs = 0.95),
                   min = min(dist_to_NRV),
                   max = max(dist_to_NRV))

delta_vs_dist |> 
  ggplot(aes(x = dist_to_NRV)) +
  geom_histogram(position = "identity", color = "#e9ecef", binwidth = 0.01) +
  facet_wrap(~scenario) +
  theme_bw() +
  labs(x = "Distance to NRV", y = "Count")

delta |> 
  dplyr::group_by(scenario) |> 
  dplyr::summarise(median_delta = median(delta_dist_to_NRV),
                   q05_delta = quantile(delta_dist_to_NRV, probs = 0.05),
                   q95_delta = quantile(delta_dist_to_NRV, probs = 0.95),
                   min_delta = min(delta_dist_to_NRV),
                   max_delta = max(delta_dist_to_NRV))

delta |> 
  dplyr::mutate(nrv_state = dplyr::case_when(delta_dist_to_NRV < 0 ~ "gain",
                                             delta_dist_to_NRV > 0 ~ "loss")) |> 
  dplyr::group_by(scenario, nrv_state) |> 
  dplyr::summarise(n = dplyr::n()) |> 
  dplyr::ungroup() |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(sum_n = sum(n),
                pr = (n * 100) / sum_n)

delta <- delta |> 
  dplyr::mutate(state = dplyr::case_when(delta_dist_to_NRV > 5 ~ "Gets further from MIT",
                                         delta_dist_to_NRV < -5 ~ "Gets closer to MIT",
                                         delta_dist_to_NRV <= 5 & delta_dist_to_NRV >= -5 ~ "Stable"))

delta |> 
  dplyr::group_by(scenario, state) |> 
  dplyr::summarise(n = dplyr::n()) |> 
  dplyr::ungroup() |> 
  dplyr::group_by(scenario) |> 
  dplyr::mutate(sum_n = sum(n),
                pr = (n * 100) / sum_n)

median(delta[delta$scenario == "SSP1-2.6",]$delta_dist_to_NRV)
quantile(delta[delta$scenario == "SSP1-2.6",]$delta_dist_to_NRV, probs = 0.05)
quantile(delta[delta$scenario == "SSP1-2.6",]$delta_dist_to_NRV, probs = 0.95)
median(delta[delta$scenario == "SSP5-8.5",]$delta_dist_to_NRV)
quantile(delta[delta$scenario == "SSP5-8.5",]$delta_dist_to_NRV, probs = 0.05)
quantile(delta[delta$scenario == "SSP5-8.5",]$delta_dist_to_NRV, probs = 0.95)

delta_nrv_dist <- delta |> 
  dplyr::mutate(state = forcats::fct_relevel(state, "Gets further from MIT", "Stable", "Gets closer to MIT")) |> 
  ggplot(aes(y = delta_dist_to_NRV, x = scenario)) +
  geom_point(aes(color = state), position = "jitter", size = 1) +
  scale_color_manual(values = c("Gets further from MIT" = color_scale[1],
                                "Stable" = color_scale[4],
                                "Gets closer to MIT" = color_scale[3])) +
  theme_bw() +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14)) + 
  geom_violin(aes(x = scenario, y = delta_dist_to_NRV), alpha = 0.5, linewidth = 0.5, show.legend = FALSE) +
  labs(y = "Distance to MIT change (%)", x = "", color = "")

ggsave(delta_nrv_dist, file = "figures/delta_nrv_dist2.png", width = 9, height = 5)

delta |>
  dplyr::mutate(latitude = abs(latitude)) |>
  ggplot(aes(y = latitude, x = delta_dist_to_NRV)) +
  geom_point(size = 0.5) +
  theme_bw()

delta_vs_dist |> 
  dplyr::mutate(latitude = abs(latitude)) |>
  ggplot(aes(y = latitude, x = dist_to_NRV)) +
  geom_point(size = 0.5) +
  theme_bw()
# ggsave(delta_nrv_latitude_plot, file = "figures/delta_nrv_latitude_plot.png")

site_nrv_h_plot <- site_nrv_h |> 
  dplyr::full_join(iso) |> 
  dplyr::filter(
    iso3 %in% c(NA, "CHN", "TZA", "MOZ", "WLF", "WSM", "BRA", "AUS", "TON", "JPN", "COK", "PYF"),
    scenario %in% c(NA, "ssp585"))

# site_nrv_h_plot <- site_nrv_h_plot[-which(site_nrv_h_plot$country == "Caribbean Sea"),]

pcoa_axis <- site_nrv_h[-1, colnames(site_nrv_h) %in% c("Axis.1", "Axis.2", "year", "scenario")] |> 
  dplyr::filter(year == "2075",
                scenario %in% c(NA, "ssp585")) |> 
  dplyr::select(-c(year, scenario))
nut_pcoa <- site_nrv_h[-1, colnames(site_nrv_h) %in% c("Calcium", "VitaminA", "Omega3", "Iron", "Selenium", "Zinc", "year", "scenario")] |> 
  dplyr::filter(year == "2075",
                scenario %in% c(NA, "ssp585")) |> 
  dplyr::select(-c(year, scenario))

ef <- vegan::envfit(pcoa_axis, nut_pcoa, permutations = 999)

vegan::scores(ef, display = "vectors")

test <- site_nrv_h |> dplyr::filter(year == "2075",
                                    scenario %in% c(NA, "ssp585"))

cor(pcoa_axis$Axis.1, test[,1:6])
cor(pcoa_axis$Axis.2, test[,1:6])

arrows_df <- as.data.frame(vegan::scores(ef, display = "vectors"))
arrows_df$nutrient <- rownames(arrows_df)

arrow_scale <- 0.2
arrows_df$xend <- arrows_df$Axis.1 * arrow_scale
arrows_df$yend <- arrows_df$Axis.2 * arrow_scale

site_nrv_country_y <- site_nrv_h_plot |>
  dplyr::group_by(year, country, iso3) |>
  dplyr::summarise(
    Axis.1 = mean(Axis.1),
    Axis.2 = mean(Axis.2)
  ) |>
  tidyr::drop_na()

arrow_country_2024 <- site_nrv_country_y |>
  dplyr::ungroup() |>
  dplyr::filter(year == "2024") |>
  dplyr::group_by(country) |>
  dplyr::summarise(start_point_x = Axis.1,
                   start_point_y = Axis.2)

arrow_country_2075 <- site_nrv_country_y |>
  dplyr::ungroup() |>
  dplyr::filter(year == "2075") |>
  dplyr::group_by(country) |>
  dplyr::summarise(end_point_x = Axis.1,
                   end_point_y = Axis.2)

arrow_country <- arrow_country_2024 |> 
  dplyr::inner_join(arrow_country_2075)

label_country <- site_nrv_h_plot |>
  dplyr::filter(year == 2075) |> 
  dplyr::group_by(country, iso3) |>
  dplyr::summarise(Axis.1 = mean(Axis.1),
                   Axis.2 = mean(Axis.2) + 0.015) |>
  tidyr::drop_na()

arrows_df$xend_label <- arrows_df$xend
arrows_df$yend_label <- arrows_df$yend

arrows_df[arrows_df$nutrient == "Omega3",]$xend_label <- 0.15
arrows_df[arrows_df$nutrient == "Omega3",]$yend_label <- 0.06
arrows_df[arrows_df$nutrient == "Zinc",]$xend_label <- -0.055
arrows_df[arrows_df$nutrient == "Selenium",]$yend_label <- -0.03


nrv_site_plot <- ggplot() + 
  geom_point(data = site_nrv_h[-1, colnames(site_nrv_h) %in% c("Axis.1", "Axis.2", "year", "scenario")] |> 
               dplyr::filter(scenario %in% c(NA, "ssp585")), 
             aes(x = Axis.1, y = Axis.2), 
             col = "grey",
             size = 1,
             alpha = 0.2) +
  geom_segment(data = arrows_df,
               aes(x = 0, y = 0, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "#377EB8",
               linewidth = 1) +
  geom_point(data = site_nrv_country_y, 
             aes(x = Axis.1, y = Axis.2, color = iso3, shape = year),
             size = 5) +
  # scale_color_manual(values = ggthemes::stata_pal("s1rcolor")(15)[-c(1,2)]) +
  geom_point(data = subset(site_nrv_h_plot, highlight == TRUE),
             aes(x = Axis.1, y = Axis.2, fill = type),
             color = "#de7309",
             size = 8) +
  geom_segment(data = arrow_country, 
               aes(x = start_point_x, y = start_point_y, xend = end_point_x, yend = end_point_y),
               arrow = arrow(length = unit(0.3, "cm")),
               linewidth = 1,
               alpha = 0.5) +
  labs(x = "PCoA axis 1 (63.4%)", y = "PCoA axis 2 (22.8%)", color = "", shape = "", title = "A.", fill = "") +
  scale_fill_manual(values = c("MIT" = "#de7309")) +
  theme_bw() +
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        title = element_text(size = 19),
        axis.title.y = element_text(vjust = -37)) +
  geom_text(data = arrows_df,
            aes(x = xend_label, y = yend_label, label = nutrient),
            color = "#377EB8", hjust = -0.1, size = 6) +
  scale_x_continuous(limits = c(-0.15, 0.21)) +
  scale_y_continuous(limits = c(-0.1, 0.37))


test_migration <- patchwork::wrap_plots(nrv_site_plot / delta_nrv_country_plot)

# ggsave(test_migration, file = "figures/test_global_migration_conv_om3.png", height = 16, width = 14)
ggsave(test_migration, file = "figures/test_global_migration_conv_om3_all.png", height = 16, width = 14)

load("data/raw-data/1c_species_traits_final.Rdata")
load("data/derived-data/distance_to_nrv_migration_conv_om3.Rdata")

imp_fishing <- species_traits_final |> 
  dplyr::select(rls_species_name, Importance) |> 
  dplyr::rename(species_name = rls_species_name) |> 
  dplyr::inner_join(distance_to_nrv_migration) |> 
  tidyr::drop_na()

imp_fishing[imp_fishing$Importance == "of no interest",]$Importance <- "Of no interest"
imp_fishing[imp_fishing$Importance == "subsistence fisheries",]$Importance <- "Subsistence fisheries"
imp_fishing[imp_fishing$Importance == "minor commercial",]$Importance <- "Minor commercial"
imp_fishing[imp_fishing$Importance == "highly commercial",]$Importance <- "Highly commercial"
imp_fishing[imp_fishing$Importance == "commercial",]$Importance <- "Commercial"

n_imp <- imp_fishing |> 
  dplyr::group_by(Importance) |> 
  dplyr::summarise(n = dplyr::n())

library(ggplot2)

fishing_vs_nrv <- imp_fishing |> 
  dplyr::inner_join(n_imp) |> 
  dplyr::ungroup() |> 
  # dplyr::mutate(Importance = paste0(Importance, " n = ", n)) |> 
  dplyr::mutate(Importance = forcats::fct_reorder(Importance, dist_to_NRV)) |> 
  ggplot(aes(x = Importance, y = dist_to_NRV)) + 
  geom_point(position = "jitter") +
  geom_boxplot(aes(fill = Importance), alpha = 0.6) +
  theme_bw() + 
  labs(y = "Species distance to NRV", x = "Species importance to fisheries")

# Test ANOVA
anova_data <- imp_fishing
anova_result <- aov(dist_to_NRV ~ Importance, data = anova_data)

# Test post-hoc Tukey
tukey_result <- TukeyHSD(anova_result)
tukey_table <- as.data.frame(tukey_result$Importance)

diff <- tukey_table$`p adj` < 0.05
names(diff) <- rownames(tukey_table)

# Conversion en lettres avec multcompView
significance_letters <- multcompView::multcompLetters(diff,
                                                      Letters = letters)$Letters

y <- anova_data |>
  dplyr::group_by(Importance) |>
  dplyr::summarise(y = 0.7) |>
  dplyr::ungroup()

# scenario = dplyr::case_when(scenario == "SSP12.6" ~ "SSP1-2.6",
#                             scenario == "SSP58.5" ~ "SSP5-8.5")

label_data <- data.frame(
  Importance = names(significance_letters),
  Letter = significance_letters
) |>
  dplyr::inner_join(y)

label_data <- label_data |> 
  dplyr::arrange(Importance)

delta_species_plot_text <- fishing_vs_nrv + 
  geom_text(data = label_data,
            aes(x = Importance, y = y, label = Letter),
            inherit.aes = FALSE,
            size = 5)

ggsave(fishing_vs_nrv, file = "figures/fishing_vs_nrv.png", width = 10)
