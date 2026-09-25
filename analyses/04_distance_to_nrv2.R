#Setting thresholds - NRVS
#Vitamin A
va_nrv <- 800

#Calcium
ca_nrv <- 1000

#Iron
fe_nrv <- 22

#Omega 3
om_nrv <- 250

#Zinc
zn_nrv <- 14

#Selenium
se_nrv <-  60

load("data/derived-data/nutrient_data.Rdata")

nutrient_data <- nutrient_data |>
  dplyr::select(-c(matches("L95|U95"), Protein, species)) |> 
  as.data.frame() |> 
  dplyr::mutate(Omega3 = Omega3 * 1000) # convert omega3 to the right unit

nrv <- c("nrv", ca_nrv, va_nrv, om_nrv, fe_nrv, se_nrv, zn_nrv) # order must match nutrient order in nutrient_data

nut_nrv <- rbind(nrv, nutrient_data)

nut_nrv[,!colnames(nut_nrv) %in% "fishbase_name"] <- lapply(nut_nrv[,!colnames(nut_nrv) %in% "fishbase_name"], as.numeric)

#1. Si une espèce a une valeur > 1/3 du NRV pour un nutriment on remplace par 1/3 du NRV pour ne pas pénaliser les espèces avec excès par rapport au NRV
nut_nrv[nut_nrv$fishbase_name == "nrv",][,-1] <- nut_nrv[nut_nrv$fishbase_name == "nrv",][,-1] / 3 #the target is 1/3 of NRV
head(nut_nrv)

nut_nrv_capped <- nut_nrv |> 
  dplyr::mutate(Calcium = ifelse(Calcium > ca_nrv / 3 , ca_nrv / 3, Calcium),
                Iron = ifelse(Iron > fe_nrv / 3 , fe_nrv / 3, Iron),
                Omega3 = ifelse(Omega3 > om_nrv / 3 , om_nrv / 3, Omega3),
                Selenium = ifelse(Selenium > se_nrv / 3 , se_nrv / 3, Selenium),
                VitaminA = ifelse(VitaminA > va_nrv / 3 , va_nrv / 3, VitaminA),
                Zinc = ifelse(Zinc > zn_nrv / 3 , zn_nrv / 3, Zinc))

Abahianus_Cspilurus <- nut_nrv_capped |> 
  dplyr::filter(fishbase_name %in% c("nrv", "Acanthurus bahianus", "Chlorurus spilurus"))

nut_nrv_capped <- nut_nrv_capped |> 
  dplyr::select(Calcium:Zinc)

head(nut_nrv_capped)
summary(nut_nrv_capped)


#2. La distance entre deux espèces A et B serait Somme Abs (Ai - Bi) / (Ai + Bi) i étant le nutriment, chaque nutriment reste dans son unité, pas de standardisation

#Run Bray-curtis

nrv <- nut_nrv_capped[1, , drop = FALSE]
species <- nut_nrv_capped[-1, ]

D <- vegan::vegdist(rbind(nrv, species), method = "bray")

#3. PCoA 
pcoa <- ape::pcoa(D)
head(pcoa$values)

print(pcoa)

coord <- as.data.frame(pcoa$vectors[,c(1:4)])
coord$species <- rownames(coord)
coord$type <- c("MIT", rep("species", nrow(species)))
# coord$type[1] <- "nrv"  

col <- c("MIT" = "#de7309", "species" = "grey")

# ef <- vegan::envfit(pcoa$vectors[-1, 1:2], species, permutations = 999)
# 
# arrows_df <- as.data.frame(vegan::scores(ef, display = "vectors"))
# arrows_df$nutrient <- rownames(arrows_df)
# 
# arrow_scale <- 0.5
# arrows_df$xend <- arrows_df$Axis.1 * arrow_scale
# arrows_df$yend <- arrows_df$Axis.2 * arrow_scale

library(ggplot2)

pcoa_nrv <- ggplot(coord, aes(x = Axis.1, y = Axis.2)) + 
  geom_point(aes(col = type, size = type)) +
  scale_color_manual(values = col) +
  scale_size_manual(
    values = c("species" = 3, "MIT" = 5)
  ) +
  guides(size = "none") +
  # geom_segment(data = arrows_df,
  #              aes(x = 0, y = 0, xend = xend, yend = yend),
  #              arrow = arrow(length = unit(0.2, "cm")),
  #              color = "#377EB8") +
  # geom_text(data = arrows_df,
  #           aes(x = xend, y = yend, label = nutrient),
  #           color = "#377EB8", vjust = 1.5, size = 4.5) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 13),
        title = element_text(size = 13)) +
  labs(x = paste0("PCoA axis 1 (", round(pcoa$values$Relative_eig[1] * 100, 2), "%)") , y = paste0("PCoA axis 2 (", round(pcoa$values$Relative_eig[2] * 100, 2), "%)"), col = "", title = "A.")

#4. extract distance to NRV from dissimilarity matrix
distm <- as.matrix(D)

dist_to_NRV <- distm[1,-1]

BC <- data.frame(dist_to_NRV)
BC$species_name <- nutrient_data$fishbase_name
head(BC)

distance_to_nrv_migration <- BC

distance_to_nrv_migration |> 
  dplyr::summarise(mean_dist = mean(dist_to_NRV),
                   min_dist = min(dist_to_NRV),
                   max_dist = max(dist_to_NRV),
                   q05 = quantile(dist_to_NRV, probs = 0.05),
                   q95 = quantile(dist_to_NRV, probs = 0.95))

# save(distance_to_nrv_migration, file = "data/derived-data/distance_to_nrv_migration.Rdata")
save(distance_to_nrv_migration, file = "data/derived-data/distance_to_nrv_migration_conv_om3.Rdata")

#end

hist(BC$dist_to_NRV)

load("data/raw-data/1c_species_traits_final.Rdata")

species_traits_final <- species_traits_final[!is.na(species_traits_final$trophic_guild),]

species_traits_final[species_traits_final$trophic_guild == "Herbivores Microvores Detritivores",]$trophic_guild <- "Herbivores"
species_traits_final[species_traits_final$trophic_guild == "piscivore",]$trophic_guild <- "Piscivore"
species_traits_final[species_traits_final$trophic_guild == "macroinvertivore",]$trophic_guild <- "Macroinvertivore"
species_traits_final[species_traits_final$trophic_guild == "corallivore",]$trophic_guild <- "Corallivore"
species_traits_final[species_traits_final$trophic_guild == "sessile invertivores",]$trophic_guild <- "Sessile invertivores"
species_traits_final[species_traits_final$trophic_guild == "crustacivore",]$trophic_guild <- "Crustacivore"
species_traits_final[species_traits_final$trophic_guild == "microinvertivore",]$trophic_guild <- "Microinvertivore"
species_traits_final[species_traits_final$trophic_guild == "planktivore",]$trophic_guild <- "Planktivore"

species_phylo <- species_traits_final |> 
  dplyr::select(fishbase_name, phylum, class, order, family, trophic_guild) |> 
  dplyr::ungroup() |> 
  dplyr::rename(species_name = fishbase_name) |> 
  unique()

validname_clean <- species_phylo |> 
  tidyr::drop_na() |> 
  dplyr::group_by(species_name) |> 
  dplyr::mutate(n = dplyr::n()) 

BC_phylo <- BC |> 
  dplyr::inner_join(validname_clean)

# BC_phylo <- BC_phylo[-191,] |> 
#   dplyr::select(-n)
BC_phylo <- BC_phylo |> 
  dplyr::select(-n)

BC_phylo <- BC_phylo |> 
  dplyr::group_by(family) |> 
  dplyr::mutate(n = dplyr::n(),
                family = paste0(family, " (n = ", n, ")")) |> 
  dplyr::ungroup()

BC_troph <- BC_phylo |> 
  dplyr::group_by(trophic_guild) |> 
  dplyr::mutate(n = dplyr::n(),
                trophic_guild = paste0(trophic_guild, " (n = ", n, ")")) |> 
  dplyr::ungroup()

BC_phylo$family <- as.factor(BC_phylo$family)
BC_troph$trophic_guild <- as.factor(BC_troph$trophic_guild)

length(unique(BC_troph$species_name))

library(ggplot2)

mean(BC_phylo$dist_to_NRV)
quantile(BC_phylo$dist_to_NRV, probs = 0.05)
quantile(BC_phylo$dist_to_NRV, probs = 0.95)

stat_phylo <- BC_phylo |> 
  dplyr::group_by(family) |> 
  dplyr::summarise(median_dist = median(dist_to_NRV),
                   q05_dist = quantile(dist_to_NRV, probs = 0.05),
                   q95_dist = quantile(dist_to_NRV, probs = 0.95))

stat_trop <- BC_phylo |> 
  dplyr::group_by(trophic_guild) |> 
  dplyr::summarise(median_dist = median(dist_to_NRV),
                   q05_dist = quantile(dist_to_NRV, probs = 0.05),
                   q95_dist = quantile(dist_to_NRV, probs = 0.95))

dist_family_nrv <- BC_phylo |> 
  dplyr::mutate(family = forcats::fct_reorder(family, dist_to_NRV)) |> 
  ggplot(aes(x = dist_to_NRV, y = family)) +
  geom_point(aes(color = family), position = "jitter", size = 1) +
  geom_boxplot(alpha = 0, size = 0.8, outlier.shape = NA) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        title = element_text(size = 14)) +
  labs(x = "Distance to MIT", y = "", title = "B.")

dist_troph_nrv <- BC_troph |> 
  dplyr::mutate(trophic_guild = forcats::fct_reorder(trophic_guild, dist_to_NRV)) |> 
  ggplot(aes(x = dist_to_NRV, y = trophic_guild)) +
  geom_point(aes(color = trophic_guild), position = "jitter", size = 1) +
  geom_boxplot(alpha = 0, size = 1, outlier.shape = NA) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        title = element_text(size = 14)) +
  labs(x = "Distance to MIT", y = "", title = "C.") 

pcoa_nrv2 <- pcoa_nrv + theme(axis.title.y = element_text(vjust = -30))

dist_nrv_plot <- patchwork::wrap_plots(
  pcoa_nrv2,
  dist_family_nrv, dist_troph_nrv,
  ncol = 2,
  heights = c(1.5, 2)
) +
  patchwork::plot_layout(
    design = "
    AA
    BC
    "
  )

# ggsave(dist_nrv_plot, file = "figures/dist_nrv_plot.png", width = 10, height = 11)
ggsave(dist_nrv_plot, file = "figures/dist_nrv_plot_conv_om3.png", width = 10, height = 11)

load("data/derived-data/nutrient_quality_migration.Rdata")

#Setting thresholds - NRVS
#Vitamin A
va_nrv <- 800 / 3

#Calcium
ca_nrv <- 1000 / 3

#Iron
fe_nrv <- 22 / 3

#Omega 3
om_nrv <- 250 / 3

#Zinc
zn_nrv <- 14 / 3

#Selenium
se_nrv <-  60 / 3

nrv <- data.frame(Calcium = ca_nrv,
                  VitaminA = va_nrv,
                  Omega3 = om_nrv,
                  Iron = fe_nrv,
                  Selenium = se_nrv,
                  Zinc = zn_nrv)

nrv <- nrv |> 
  dplyr::mutate(site_code = NA,
                depth = NA, 
                latitude = NA, 
                longitude = NA,
                realm = NA, 
                country = NA,
                scenario = NA,
                year = NA)

pcoa_scenario <- c("ssp126", "ssp245", "ssp370", "ssp585")


nutrient_quality_migration <- nutrient_quality_migration |> 
  dplyr::mutate(Omega3 = Omega3 * 1000) 

pcoa_nutrient <- pbmcapply::pbmclapply(1:length(pcoa_scenario), function(i) {
  
  nutrient_24_75 <- nutrient_quality_migration |> 
    dplyr::ungroup() |> 
    dplyr::filter(year %in% c("2024", "2075")) |> 
    dplyr::filter(scenario %in% pcoa_scenario[i])
  
  nutrient_24_75_nrv <- rbind(nrv, nutrient_24_75)
  
  D_site <- vegan::vegdist(nutrient_24_75_nrv[,c(1:6)], method = "bray")
  
  pcoa_site <- ape::pcoa(D_site)
  
  round(pcoa_site$values$Relative_eig[1] * 100, 2) # i = 1 = 65.58, i = 4 = 63.4, mean = 64.49
  round(pcoa_site$values$Relative_eig[2] * 100, 2) # i = 1 = 21.48, i = 4 = 22.76, mean = 22.12
  
  coord <- as.data.frame(pcoa_site$vectors[,c(1:4)])
  coord$site <- rownames(coord)
  coord$type <- rep("site", nrow(coord))
  coord$type[1] <- "nrv"
  
  coord <- cbind(nutrient_24_75_nrv, coord[,c(1, 2, 6)])
  
  # #4. extract distance to NRV from dissimilarity matrix
  distm <- as.matrix(D_site)
  
  dist_to_NRV <- distm[1,]
  
  BC <- data.frame(dist_to_NRV)
  coord_BC <- cbind(coord, BC)
  
  site_nrv <- coord_BC
  
}, mc.cores = 4)

site_nrv_migration <- do.call(rbind, pcoa_nutrient)

save(site_nrv_migration, file = "outputs/site_nrv_migration2.Rdata")
