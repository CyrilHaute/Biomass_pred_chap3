# pred_biomass_data = list(rf, dnn, xgboost)
# score_data = performance_bind

score_function <- function(pred_biomass_data,
                           score_data){
  
  rescale_01 <- function(x){(x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))}
  
  score_data$pearson <- rescale_01(score_data$pearson)
  
  score_data$spearman <- rescale_01(score_data$spearman)
  
  species <- mean(sapply(pred_biomass_data, length))
  
  score <- pbmcapply::pbmclapply(1:species, function(i) {
    
    sp_i <- lapply(pred_biomass_data, '[[', i)

    score_scenario <- lapply(1:length(scenarii), function(j) {
      
      scenario_j <- lapply(sp_i, '[[', j)

      scenario_j <- do.call(rbind, scenario_j)

      species_j <- unique(scenario_j$species_name)

      model <- unique(scenario_j$model)
      
      model_scenario <- lapply(1:length(model), function(k) {
        
        model_k <- scenario_j[scenario_j$model == model[k],]
        
        model_k <- model_k[,c("survey_id", "species_name", "model", "validation_predict", colnames(model_k)[which(grepl(scenarii[j], colnames(model_k)))])]
        
        colnames(model_k)[which(grepl(scenarii[j], colnames(model_k)))] <- gsub(pattern = paste0("_", scenarii[j]), replacement = "", colnames(model_k)[which(grepl(scenarii[j], colnames(model_k)))])
        
        colnames(model_k)[colnames(model_k) == "validation_predict"] <- "2022"
        
        model_k <- model_k |> 
          dplyr::mutate(dplyr::across(`2022`:`2100`, ~ dplyr::case_when(. > max(decile_save$biomass_class) ~ 29, 
                                                                        . < min(decile_save$biomass_class) ~ 0,
                                                                        TRUE ~ .)))
        
        for(l in 1:nrow(decile_save)) {
          
          model_k[model_k == decile_save$biomass_class[l]] <- decile_save$biomass[l]
          
        }
        
        model_k

      })
      
      nrow_dtf <- unlist(lapply(model_scenario, nrow))
      
      if(length(unique(nrow_dtf)) != 1) {
        
        min_dtf <- which(nrow_dtf == min(nrow_dtf))
        max_dtf <- seq_along(nrow_dtf)[-min_dtf]

        diff_max_min <- sapply(1:length(max_dtf), function(l) {
          
          unique(nrow_dtf[max_dtf][l]) - unique(nrow_dtf[min_dtf])
          
        })

        row_remove <- lapply(1:length(nrow_dtf[max_dtf]), function(l) {
          
          sample(x = 1:unique(nrow_dtf[max_dtf][l]), diff_max_min[l])
          
        })
        
        for(l in 1:length(max_dtf)){
          
          model_scenario[[max_dtf[l]]] <- model_scenario[[max_dtf[l]]][-row_remove[[l]],]
          
        }

      }

      perf_species_j <- score_data[score_data$species_name == species_j,]
      
      l_model <- length(model)
      
      dtf_score <- lapply(1:l_model, function(k) {
        
        model_scenario[[k]][colnames(model_scenario[[k]]) %in% "2022":"2100"] * perf_species_j[perf_species_j$model == unique(model_scenario[[k]]$model),]$pearson
        
      })
      # dtf_score <- round(Reduce(`+`, dtf_score) / sum(perf_species_j$pearson))
      dtf_score <- Reduce(`+`, dtf_score) / sum(perf_species_j$pearson)

      sp_info <- model_scenario[[1]][,c("survey_id", "species_name")]
      sp_info$scenario <- scenarii[j]
      sp_info$model <- "scored_model"
      
      dtf_score <- cbind(sp_info, dtf_score)

      dtf_score
      
    })
    
  }, mc.cores = parallel::detectCores() - 1)
  
}
