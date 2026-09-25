
rf <- list.files("data/raw-data/biomass_class_test_rf_hist_ns50", full.names = TRUE)
rf <- lapply(1:length(rf), function(i) {
  
  load(rf[i])
  assign(as.character(i), all_pred)
  
})
xgboost <- list.files("data/raw-data/biomass_class_test_xgboost_hist_md4_mcw25_msl3", full.names = TRUE)
xgboost <- lapply(1:length(xgboost), function(i) {
  
  load(xgboost[i])
  assign(as.character(i), all_pred)
  
})
dnn <- list.files("data/raw-data/biomass_class_test_dnn_hist_h9_lr002_ld01", full.names = TRUE)
dnn <- lapply(1:length(dnn), function(i) {
  
  load(dnn[i])
  assign(as.character(i), all_pred)
  
})

load("data/derived-data/nutrient_data.Rdata")

species <- nutrient_data$species

species_model <- sapply(1:length(rf), function(i) {
  
  unique(rf[[i]]$species_name)
  
})

species_model[which(species_model %in% species)]

read_sp_eco <- c(rf[which(species_model %in% species)],
                 dnn[which(species_model %in% species)], 
                 xgboost[which(species_model %in% species)])
read_sp_eco <- read_sp_eco[-which(unlist(lapply(1:length(read_sp_eco), function(i) { is.null(read_sp_eco[[i]]) })))]


performance <- pbmcapply::pbmclapply(1:length(read_sp_eco), function(i) {
  
  sp <- read_sp_eco[[i]]

  # Linear model between values
  lm_test <- lapply(1:length(unique(sp$cv)), function(k) {
    
    tryCatch(lm(validation_predict ~ validation_observed, data = sp[sp$cv %in% unique(sp$cv)[k],]), error = function(e) NA)
    
  })
  
  
  Pr2 <- lapply(1:length(lm_test), function(k) {
    
    sum_lm <- summary(lm_test[[k]])
    Pr2 <- sum_lm$r.squared
    
  })
  
  cor.test_pearson  <- lapply(1:length(unique(sp$cv)), function(l) {
    
    cor.test_pearson  <- tryCatch(cor.test(as.numeric(sp[sp$cv %in% unique(sp$cv)[l],]$validation_predict), 
                                           as.numeric(sp[sp$cv %in% unique(sp$cv)[l],]$validation_observed), method = 'pearson'), error = function(e) NA)
    
    Pearson <- cor.test_pearson$estimate
    
  })
  
  cor.test_spearman  <- lapply(1:length(unique(sp$cv)), function(l) {
    
    cor.test_spearman <- tryCatch(cor.test(as.numeric(sp[sp$cv %in% unique(sp$cv)[l],]$validation_predict), 
                                           as.numeric(sp[sp$cv %in% unique(sp$cv)[l],]$validation_observed), method = 'spearman'), error = function(e) NA)
    
    Spearman <- cor.test_spearman$estimate
    
  })
  
  metric_summary <- lapply(1:length(unique(sp$cv)), function(j) {
    
    data.frame(species_name = unique(sp$species_name),
               R2 = unlist(Pr2[[j]]),
               pearson = unlist(cor.test_pearson[[j]]),
               spearman = unlist(cor.test_spearman[[j]]),
               cv = unique(sp$cv)[j])
    
  })
  metric_summary <- do.call(rbind, metric_summary)
  
  metric_summary <- metric_summary |> 
    dplyr::group_by(species_name) |> 
    dplyr::summarise(r2 = mean(R2, na.rm = TRUE),
                     r2_sd = round(sd(R2, na.rm = TRUE), digits = 2),
                     sd_pearson = round(sd(pearson, na.rm = TRUE), digits = 2),
                     pearson = mean(pearson, na.rm = TRUE),
                     sd_spearman = round(sd(spearman, na.rm = TRUE), digits = 2),
                     spearman = mean(spearman, na.rm = TRUE)) |> 
    dplyr::mutate(model = unique(sp$model))
  
  return(metric_summary)
  
}, mc.cores = parallel::detectCores() - 1)

performance_bind <- do.call(rbind, performance)

performance_bind |> 
  dplyr::summarise(medianr2 = median(r2, na.rm = TRUE),
                   q05r2 = quantile(r2, probs = 0.05, na.rm = TRUE),
                   q95r2 = quantile(r2, probs = 0.95, na.rm = TRUE),
                   medianpearson = median(pearson, na.rm = TRUE),
                   q05pearson  = quantile(pearson , probs = 0.05, na.rm = TRUE),
                   q95pearson  = quantile(pearson , probs = 0.95, na.rm = TRUE),
                   medianspearman = median(spearman, na.rm = TRUE),
                   q05spearman  = quantile(spearman , probs = 0.05, na.rm = TRUE),
                   q95spearman  = quantile(spearman , probs = 0.95, na.rm = TRUE))

perf_model <- performance_bind |> 
  dplyr::group_by(model) |> 
  dplyr::summarise(medianr2 = median(r2, na.rm = TRUE),
                   q05r2 = quantile(r2, probs = 0.05, na.rm = TRUE),
                   q95r2 = quantile(r2, probs = 0.95, na.rm = TRUE),
                   medianpearson = median(pearson, na.rm = TRUE),
                   q05pearson  = quantile(pearson , probs = 0.05, na.rm = TRUE),
                   q95pearson  = quantile(pearson , probs = 0.95, na.rm = TRUE),
                   medianspearman = median(spearman, na.rm = TRUE),
                   q05spearman  = quantile(spearman , probs = 0.05, na.rm = TRUE),
                   q95spearman  = quantile(spearman , probs = 0.95, na.rm = TRUE))

performance_bind <- performance_bind |> 
  dplyr::inner_join(perf_model, by = c("model"))


performance_bind[performance_bind$model == "rf",]$model <- "RF"
performance_bind[performance_bind$model == "xgboost",]$model <- "Xgboost"
performance_bind[performance_bind$model == "dnn",]$model <- "DNN"

performance_bind <- performance_bind |> 
  dplyr::mutate(medianr2_label = paste0("Median = ", round(medianr2, 2)),
                medianpearson_label = paste0("Median = ", round(medianpearson, 2)),
                medianspearman_label = paste0("Median = ", round(medianspearman, 2)))

library(ggplot2)

color_scale <- ggthemes::colorblind_pal()(8)[-1]

pearson_dist_model <- performance_bind |> 
  dplyr::mutate(model = forcats::fct_reorder(model, medianpearson, .desc = TRUE)) |> 
  ggplot(aes(x = pearson)) +
  facet_wrap(~ model, ncol = 1) +
  geom_histogram(aes(fill = model), position = "identity", color = "#e9ecef", binwidth = 0.05) +
  geom_text(aes(x = 0.8, y = 70, label = medianpearson_label), check_overlap = TRUE) +
  scale_fill_manual(values = c("RF" = color_scale[6],
                               "Xgboost" = color_scale[5],
                               "DNN" = color_scale[3])) +
  geom_vline(aes(xintercept = medianpearson), color = "black", linetype = "dashed", size = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none",
        axis.title = element_text(size = 15),
        strip.text.x = element_text(size = 10)) +
  labs(x = "Pearson", y = "Count", title = "")

ggsave("figures/pearson_dist_model.png", pearson_dist_model, width = 10, height = 9)
