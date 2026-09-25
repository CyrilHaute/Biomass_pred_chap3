# Compute average nutrient concentrations at the reef fish community level:

# load species data

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

species <- data.frame(species_name = 
                        unlist(pbmcapply::pbmclapply(1:length(migration_ensemble), function(i) {
                          
                          unique(migration_ensemble[[i]][[1]]$species_name)
                          
                        }, mc.cores = parallel::detectCores() - 1))
)

fisheries_importance <- read.csv("data/raw-data/fisheries_importance.csv", header = TRUE, sep = ",") |> 
  dplyr::select(family, target)

load("data/raw-data/1c_species_traits_final.Rdata")

species_trait <- species_traits_final |> 
  dplyr::select(rls_species_name, family) |> 
  dplyr::rename(species_name = rls_species_name) |> 
  dplyr::inner_join(fisheries_importance) |> 
  dplyr::inner_join(species) |> 
  dplyr::filter(target %in% c("targeted", "targeted>20cm"))

species <- species |> 
  dplyr::filter(species_name %in% species_trait$species_name)

# Check species names first

validname <- rfishbase::validate_names(species$species_name)

validname_clean <- validname[!validname %in% "Acanthurus nigros"]

validname_clean <- data.frame(species = species$species_name,
                              fishbase_name = validname_clean)

validname_clean <- validname_clean |> 
  tidyr::drop_na() |> 
  dplyr::group_by(fishbase_name) |> 
  dplyr::mutate(n = dplyr::n()) 

species_double <- validname_clean |> 
  dplyr::filter(n > 1) |> 
  dplyr::filter(species == fishbase_name)

validname_clean <- validname_clean |> 
  dplyr::filter(n == 1) |> 
  dplyr::full_join(species_double) |> 
  dplyr::select(!n)

# Extract variables from FishBase

coln <- c("Species", 
          "Calcium", "Calcium_l95", "Calcium_u95",
          "VitaminA", "VitaminA_l95", "VitaminA_u95",
          "Omega3", "Omega3_l95", "Omega3_u95",
          "Iron", "Iron_l95", "Iron_u95",
          "Selenium", "Selenium_l95", "Selenium_u95",
          "Zinc", "Zinc_l95", "Zinc_u95",
          "Protein", "Protein_l95", "Protein_u95")

nut <- rfishbase::estimate(validname_clean$fishbase_name, fields = coln)
nut_data <- nut |> 
  dplyr::rename(fishbase_name = Species)
nut_data <- validname_clean |> 
  dplyr::inner_join(nut_data)

nutrient_data <- nut_data |> 
  tidyr::drop_na()

save(nutrient_data, file = "data/derived-data/nutrient_data.Rdata")
