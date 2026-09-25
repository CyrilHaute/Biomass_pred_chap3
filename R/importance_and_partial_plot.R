var_imp_function <- function(model){
  
  var_imp_model <- data.frame(delta = model$variable.importance,
                              covariates = unique(names(c(model$variable.importance))))
  
  var_imp_model_pr <- var_imp_model |> 
    dplyr::mutate(delta_pr = (delta * 100) / sum(delta))
  
  return(var_imp_model_pr)
  
}

var_imp_plot_function <- function(var_imp_dtf,
                                  title){
  
  require(ggplot2)
  
  plot_imp_var <- var_imp_dtf |> 
    ggplot() +
    geom_col(aes(x = reorder(covariates, delta_pr), y = delta_pr), fill = "#377EB8") +
    theme_classic() +
    coord_flip() +
    theme(axis.text.x = element_text(size = 22),
          axis.text.y = element_text(size = 22),
          axis.title = element_text(size = 26),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.text = element_text(size = 17),
          title = element_text(size = 24)) + 
    labs(x = "",
         y = "Variable importance (%)",
         title = title,
         fill = "")
  
}

partial_dep_function <- function(model,
                                 covariates,
                                 data,
                                 reponse_name){
  
  partial_i <- pbmcapply::pbmclapply(1:length(covariates), function(i) {
    
    partial_dep <- pdp::partial(model,
                                train = data,
                                pred.var = c(covariates[i]))

    colnames(partial_dep)[colnames(partial_dep) == "yhat"] <- reponse_name

    return(partial_dep)
    
  }, mc.cores = parallel::detectCores() - 1)
  
  names(partial_i) <- covariates
  
  return(partial_i)
  
}

partial_dep_prob_function <- function(model,
                                 covariates,
                                 data,
                                 reponse_name){
  
  partial_i <- pbmcapply::pbmclapply(1:length(covariates), function(i) {
    
    partial_dep_ww <- pdp::partial(model,
                                   train = data,
                                   pred.var = c(covariates[i]),
                                   which.class = "Winner-Winner",
                                   prob = TRUE)
    partial_dep_wl <- pdp::partial(model,
                                   train = data,
                                   pred.var = c(covariates[i]),
                                   which.class = "Winner-Loser",
                                   prob = TRUE)
    partial_dep_lw <- pdp::partial(model,
                                   train = data,
                                   pred.var = c(covariates[i]),
                                   which.class = "Loser-Winner",
                                   prob = TRUE)
    partial_dep_ll <- pdp::partial(model,
                                   train = data,
                                   pred.var = c(covariates[i]),
                                   which.class = "Loser-Loser",
                                   prob = TRUE)
    
    colnames(partial_dep_ww)[colnames(partial_dep_ww) == "yhat"] <- reponse_name
    colnames(partial_dep_wl)[colnames(partial_dep_wl) == "yhat"] <- reponse_name
    colnames(partial_dep_lw)[colnames(partial_dep_lw) == "yhat"] <- reponse_name
    colnames(partial_dep_ll)[colnames(partial_dep_ll) == "yhat"] <- reponse_name
    
    partial_dep_ww$class <- "Winner-Winner"
    partial_dep_wl$class <- "Winner-Loser"
    partial_dep_lw$class <- "Loser-Winner"
    partial_dep_ll$class <- "Loser-Loser"
    
    pdp_all <- partial_dep_ww |> 
      dplyr::full_join(partial_dep_wl) |>
      dplyr::full_join(partial_dep_lw) |>
      dplyr::full_join(partial_dep_ll)
    
    return(pdp_all)
    
  }, mc.cores = parallel::detectCores() - 1)
  
  names(partial_i) <- covariates
  
  return(partial_i)

}

partial_plot_function <- function(partial_data,
                                  color_values,
                                  ncol){
  
  require(ggplot2)
  
  plot_all <- pbmcapply::pbmclapply(1:length(partial_data), function(i) {
    
    partial_data_i <- partial_data[[i]]
    
    colnames(partial_data_i) <- c("covariates", "response", "scenario")
    
    plot_i <- partial_data_i |> 
      ggplot(aes(x = covariates, y = response, color = scenario)) + 
      geom_point(size = 0.5) +
      geom_line(stat = "smooth", linewidth = 1) +
      scale_color_manual(values = color_values) +
      theme_bw() +
      theme(axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16),
            axis.title = element_text(size = 17),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.text = element_text(size = 19)) +
      labs(y = "", x = names(partial_data)[i], color = "")
    
  }, mc.cores = parallel::detectCores() - 1)

  if(length(plot_all) == 1){
    
    plot_all
    
  }else{
    
    patchwork::wrap_plots(plot_all) + 
      patchwork::plot_layout(guides = "collect", ncol = ncol) &
      theme(legend.position = "bottom",
            legend.direction = "horizontal")
    
  }

}
