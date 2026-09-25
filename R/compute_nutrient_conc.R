#Compute average nutrient concentrations at the reef fish community level:

#load species data
biomass <- read.csv("data/raw-data/Biomass.csv", header = TRUE)

#Check complete list of species 

taxa <- biomass |> 
  dplyr::select(Ctenochaetus_binotatus:Sepia_spp.) |> 
  colnames() |> 
  unique()
length(taxa) #1046 'species'

#Check and clean spp. 

genuslevel <- taxa[grep("spp",taxa)]
genuslevel

#remove them for now - but this can be adapted (see at the bottom)

species <- stringr::str_replace_all(taxa[-grep("spp",taxa)],"_"," ")

#Check species names first
validname <- pbmcapply::pbmclapply(1:100, function(i) { # length(species)
  
  as.character(rfishbase::validate_names(species[i]))
  
}, mc.cores = 1)
validname_vec <- unlist(validname)

# tomerge <- data.frame(species, validname_vec)
tomerge <- data.frame(species[1:100], validname_vec)

#Check NAs
tomerge |> 
  dplyr::filter(is.na(validname)) # => only sea-snakes, turtles, jellyfish etc. 

goodsp <- tomerge |> 
  dplyr::select(validname_vec) |> 
  tidyr::drop_na() |> 
  unique() |> 
  unlist()

#Extract variables from FishBase
nut <- rfishbase::estimate(goodsp, fields = c("Calcium", "Calcium_l95", "Calcium_u95",
                                              "VitaminA", "VitaminA_l95", "VitaminA_u95",
                                              "Omega3", "Omega3_l95", "Omega3_u95",
                                              "Iron", "Iron_l95", "Iron_u95",
                                              "Selenium", "Selenium_l95", "Selenium_u95",
                                              "Zinc", "Zinc_l95", "Zinc_u95",
                                              "Protein", "Protein_l95", "Protein_u95"))

#This works but some species are 'missing' so I run extraction species by species and handle missing ones

#Extract variables from FishBase
res_list <- list()

coln <- c("species", 
          "Calcium", "Calcium_l95", "Calcium_u95",
          "VitaminA", "VitaminA_l95", "VitaminA_u95",
          "Omega3", "Omega3_l95", "Omega3_u95",
          "Iron", "Iron_l95", "Iron_u95",
          "Selenium", "Selenium_l95", "Selenium_u95",
          "Zinc", "Zinc_l95", "Zinc_u95",
          "Protein", "Protein_l95", "Protein_u95")

nut <- pbmcapply::pbmclapply(1:length(goodsp), function(i) {
  
  new <- rfishbase::estimate(goodsp[i], fields = c("Calcium", "Calcium_l95", "Calcium_u95",
                                                   "VitaminA", "VitaminA_l95", "VitaminA_u95",
                                                   "Omega3", "Omega3_l95", "Omega3_u95",
                                                   "Iron", "Iron_l95", "Iron_u95",
                                                   "Selenium", "Selenium_l95", "Selenium_u95",
                                                   "Zinc", "Zinc_l95", "Zinc_u95",
                                                   "Protein", "Protein_l95", "Protein_u95"))
  
  #check values
  
  if(nrow(new) == 1) {
    
    nut <- data.frame(goodsp[i], new)
    names(nut)[1] <- "species"
    
  }else{
    
    # If no data
    nut <- data.frame(matrix(NA, nrow = 1, ncol = length(coln)))
    names(nut) <- coln  # Match column names
    
    # Add DHS_ID to the placeholder
    nut$species <- goodsp[i]

  }
  
  return(nut)
  
}, mc.cores = 1)
nutdata_sp <- do.call(rbind, nut)

# Summary of the resulting data frame
summary(nutdata_sp)
rownames(nutdata_sp) <- NULL

nutdata_sp |> 
  dplyr::filter(is.na(Calcium)) # 5 species for which nutrient predictions are not available - could be replaced by average value at the genus level
save(nutdata_sp, file = "data/derived-data/nutdata_sp.RData")

#Remove missing species and compute average nutrient concentration at the reef level using species composition
nutd <- nutdata_sp |> 
  dplyr::filter(!is.na(Calcium)) |> 
  dplyr::mutate(species = stringr::str_replace_all(species," ","_"))

keepc <- c(seq(2, 8, 1) , which((colnames(biomass) %in% nutd$species)))

biomass_nut <-  biomass[,keepc]

# Step 1: Convert biomass_nut from wide to long format
biomass_long <- biomass_nut |> 
  tidyr::pivot_longer(cols = -(SurveyID:PrePost),  # Convert species columns to long format
                      names_to = "species", 
                      values_to = "biomass_kg_ha") |>  # Rename to clarify units
  dplyr::filter(biomass_kg_ha > 0) #remove 0 for biomass

# Step 2: Merge with nutrient data (nutd)
# Clean nutd: ensure species names match and remove unnecessary columns
nutd_clean <- nutd |> 
  #mutate(species = gsub(" ", "_", species)) %>%  # Uncomment if species names need cleaning
  dplyr::select(-matches("L95|U95"))  # Remove columns with "L95" or "U95"

# Merge biomass data with nutrient concentrations
merged_data <- biomass_long |> 
  dplyr::left_join(nutd_clean, by = "species") |>   # Merge nutrient data
  dplyr::mutate(biomass_100g = biomass_kg_ha * 10,  # Convert biomass to 100g units
                ediblebiomass_100g = biomass_100g * 0.87)  # Adjust for finfish edible portion - cf Belton et al. 2020 Nat. Comm.

# Step 3: Compute weighted mean of each nutrient per SurveyID
weighted_nutrients <- merged_data |> 
  dplyr::group_by(SurveyID) |> 
  dplyr::summarise(dplyr::across(Calcium:Protein,  # Apply to all numeric columns (nutrients)
                                 ~ weighted.mean(.x, w = ediblebiomass_100g, na.rm = TRUE), 
                                 .names = "weighted_{.col}"))

test <- merged_data |> 
  dplyr::group_by(SurveyID) |> 
  dplyr::summarise(dplyr::across(Calcium:Protein,  # Apply to all numeric columns (nutrients)
                                 ~ weighted.mean(.x, w = biomass_100g, na.rm = TRUE), 
                                 .names = "weighted_{.col}"))

# View the result
print(weighted_nutrients)
summary(weighted_nutrients)

#END
