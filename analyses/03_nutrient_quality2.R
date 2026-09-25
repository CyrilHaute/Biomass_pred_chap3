# Compute average nutrient concentration at the reef level using species composition

load("data/derived-data/nutrient_data.Rdata")
load("data/raw-data/00_rls_surveys.Rdata")

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

rls_surveys$survey_id <- as.character(rls_surveys$survey_id)

biomass_ssp_all_info <- rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "site_code", "depth")] |> 
  dplyr::inner_join(biomass_ssp_all)
biomass_ssp_all_info <- biomass_ssp_all_info[, colnames(biomass_ssp_all_info) %in% c("survey_id",
                                                                                     "site_code",
                                                                                     "depth",
                                                                                     "species_name",
                                                                                     "scenario",
                                                                                     "2024",
                                                                                     "2075",
                                                                                     "model")]

biomass_longer_split <- biomass_ssp_all_info |> 
  tidyr::pivot_longer(cols = "2024":"2075",  # convert year columns to long format
                      names_to = "year", 
                      values_to = "biomass_g_500m2") |> 
  dplyr::filter(biomass_g_500m2 > 0) |> # remove absences 
  dplyr::group_split(year)

nutrient_data <- nutrient_data |>
  dplyr::rename(species_name = species) |>
  dplyr::select(-c(matches("L95|U95"), Protein))

gc()

biomass_site <- pbmcapply::pbmclapply(1:length(biomass_longer_split), function(i) {
  
  year_i <- biomass_longer_split[[i]] |> 
    dplyr::group_split(scenario)
  
  scenario <- lapply(1:length(year_i), function(j) {
    
    scenario_j <- year_i[[j]] |> 
      dplyr::inner_join(rls_surveys[,colnames(rls_surveys) %in% c("survey_id", "site_code", "latitude", "longitude", "realm", "depth", "country")]) |>
      dplyr::select(-survey_id) |> 
      unique() |> 
      dplyr::group_by(site_code, depth, latitude, longitude, realm, country, species_name, scenario, year) |> 
      dplyr::summarise(biomass_g_500m2 = mean(biomass_g_500m2)) |> 
      dplyr::inner_join(nutrient_data) |> # Merge nutrient data
      dplyr::ungroup() |> 
      dplyr::select(-species_name) |> 
      dplyr::mutate(biomass_100g = biomass_g_500m2 / (500 * 100),  # Convert biomass to 100g units
                    ediblebiomass_100g = biomass_100g * 0.87) |> 
      dplyr::ungroup() |> 
      dplyr::group_by(site_code, depth, latitude, longitude, realm, country, scenario, year) |> 
      dplyr::summarise(dplyr::across(Calcium:Zinc,  # Apply to all numeric columns (nutrients)
                                     ~ weighted.mean(.x, w = ediblebiomass_100g, na.rm = TRUE)))
    
  })
  
  scenario_bind <- do.call(rbind, scenario)
  
}, mc.cores = 2)

nutrient_quality_migration <- do.call(rbind, biomass_site)

save(nutrient_quality_migration, file = "data/derived-data/nutrient_quality_migration.Rdata")
