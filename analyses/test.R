library(sf)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sv")

country_sel <- c("American Samoa", "Australia", "Belize", "Brazil", "Cayman Islands", "China",             
                 "Colombia", "Cook Islands", "Costa Rica", "Dominican Republic", "Ecuador", "French Polynesia", "Indonesia",         
                 "Japan",  "Mozambique", "Niue", "Panama", "Papua New Guinea", "Pitcairn Islands", "Samoa",             
                 "Seychelles", "Spain", "Tanzania", "Tonga", "United States", "Wallis and Futuna")
world$name[world$name == "Cayman Is."] <- "Cayman Islands"
world$name[world$name == "Cook Is."] <- "Cook Islands"
world$name[world$name == "Dominican Rep."] <- "Dominican Republic"
world$name[world$name == "Fr. Polynesia"] <- "French Polynesia"
world$name[world$name == "Pitcairn Is."] <- "Pitcairn Islands"
world$name[world$name == "United States of America"] <- "United States"
world$name[world$name == "Wallis and Futuna Is."] <- "Wallis and Futuna"

world <- world[world$name %in% country_sel,]

sea_world <- rnaturalearth::ne_download(scale = "medium", type = "ocean", category = "physical", returnclass = "sv")

buffer <- terra::buffer(world, width = 1000)
buffer_sea <- terra::intersect(buffer, sea_world)

country_lat <- lapply(1:length(country_sel), function(i) {
  
  lat_min_max <- terra::ext(buffer_sea[buffer_sea$name == country_sel[i],])[c(3,4)]
  country_lat <- data.frame(country = country_sel[i],
                            ymin = lat_min_max[1],
                            ymax = lat_min_max[2]) |> 
    dplyr::mutate(size_lat = abs(ymin - ymax))
  
})
country_lat_bind <- do.call(rbind, country_lat)


load("outputs/site_nrv_migration2.Rdata")

site_nrv_h <- site_nrv_migration |> 
  dplyr::select(site_code, depth, latitude, longitude, realm, country, scenario, dist_to_NRV)

delta <- site_nrv_h[-1,] |> 
  dplyr::group_by(site_code, depth, latitude, longitude, realm, country, scenario) |> 
  dplyr::summarise(delta_dist_to_NRV = (dplyr::last(dist_to_NRV) * 100) / dplyr::first(dist_to_NRV) - 100) |>
  dplyr::ungroup() |> 
  dplyr::mutate(site_code = paste0(site_code, "_", depth)) |>
  dplyr::select(-c(depth, realm))

delta_country <- delta |> 
  dplyr::group_by(country) |> 
  dplyr::summarise(mean_delta_dist_to_nrv = mean(delta_dist_to_NRV)) |> 
  dplyr::filter(country %in% country_sel)

delta_country_lat <- delta_country |> 
  dplyr::inner_join(country_lat_bind)

load("data/raw-data/iso.Rdata")

iso <- hdi_csv_rls |> 
  dplyr::select(country, iso3) |> 
  unique()

delta_country_lat <- delta_country_lat |> 
  dplyr::inner_join(iso)

library(ggplot2)

delta_country_lat_plot <- delta_country_lat |> 
  dplyr::mutate(x_label = mean_delta_dist_to_nrv + 0.6) |> 
  ggplot(aes(x = mean_delta_dist_to_nrv, y = size_lat, label = iso3)) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel() +
  theme_bw()

ggsave(delta_country_lat_plot, file = "figures/delta_country_lat_plot.png", width = 9, height = 5)
