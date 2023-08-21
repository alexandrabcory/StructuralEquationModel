# Load necessary packages
load_packages <- function() {
  library(lavaan)
  library(tidyverse)
  library(terra)
  library(rgdal)
  library(sf)
  library(ggplot2)
  library(ggpubr)
  conflicted::conflict_prefer("dplyr", "filter")
}

# Read data
read_data <- function(file_path) {
  return(read_csv(file_path))
}

# Read latent variable models table
read_latent_var_models <- function(file_path) {
  return(read_csv(file_path))
}

# Configure model based on dependent variable and list of latent variables
config_model <- function(dependent_var, latent_vars) {
  model_components <- list()
  
  if (dependent_var == "Resp_24") {
    physical <- "physical =~ porevolume + Conn_Pores"
    geochem <- "geochem =~ WEOC + Mn"
    organic <- "organic =~ sum_tanin + sum_lipid"
    veg <- "veg =~ landcover"
  } else if (dependent_var == "Percent_C") {
    physical <- "physical =~ porevolume + Conn_Pores"
    geochem <- "geochem =~ WEOC + Mn"
    organic <- "organic =~ sum_tanin + sum_lipid"
    veg <- "veg =~ landcover"
  } else if (dependent_var == "WEOC") {
    physical <- "physical =~ porevolume + Conn_Pores"
    geochem <- "geochem =~ WEOC + Mn"
    organic <- "organic =~ sum_tanin + sum_lipid"
    veg <- "veg =~ landcover"
  }
  
  dependent_var_title <- switch(dependent_var,
                                "Resp_24" = "24 Hour Respiration",
                                "C_Percent" = "Percent Carbon",
                                "WEOC" = "Water-Extractable Organic Carbon")
  
  latent_vars_dict <- list('physical' = physical, 'geochem' = geochem, 'organic' = organic, 'veg' = veg)
  for (latent_var in latent_vars) {
    model_components <- c(model_components, latent_vars_dict[[latent_var]])
  }
  
  working_model <- paste(
    paste(model_components, collapse = "\n"),
    sprintf("%s ~ %s", dependent_var, paste(latent_vars, collapse = " + ")),
    sep = "\n"
  )
  
  return(list(working_model, dependent_var_title))
}

# Run SEM model
run_SEM <- function(working_model, data) {
  return(sem(working_model, data = data))
}

# Generate and save the observed vs. modeled plot
save_observed_vs_modeled_plot <- function(working_model_scaled, sem_data, dependent_var, dependent_var_title, latent_vars) {
  
  #Get the scaled predicted values
  sem_preds <- lavPredict(working_model_scaled, type = "yhat")
  
  # Get the actual predicted values
  if (dependent_var == 'Resp_24') {
    sem_preds <- data.frame(sem_preds) %>% pull(Resp_24)
  } else if (dependent_var == 'C_Percent') {
    sem_preds <- data.frame(sem_preds) %>% pull(C_Percent)
  } else if (dependent_var == 'WEOC') {
    sem_preds <- data.frame(sem_preds) %>% pull(WEOC)
  }
  
  plot_data <- data.frame(
    Observed = sem_data[[dependent_var]],
    sem_preds = sem_preds
  )
  
  lin_model <- lm(sem_preds ~ Observed, data = plot_data)
  r_squared <- summary(lin_model)$r.squared
  
  p <- ggplot(plot_data, aes(x = Observed, y = sem_preds)) +
    geom_point() +
    geom_smooth(method = 'lm') +
    ggtitle(paste("Model vs. Observed: ", dependent_var_title)) +
    xlab("Observed") +
    ylab("Modeled") +
    annotate("text", x = Inf, y = Inf, label = paste("R-squared = ", round(r_squared, 2)), hjust = 1, vjust = 1)
  
  file_name <- generate_file_name_suffix(dependent_var_title, latent_vars)
  ggsave(filename = file_name, plot = p, width = 12, height = 10, dpi = 150)
}

# Generate file name suffix
generate_file_name_suffix <- function(dependent_var_title, latent_vars) {
  return(paste0(dependent_var_title, "_", paste(latent_vars, collapse = "_"), ".pdf"))
}

# Main function to call other functions
run_analysis <- function(data_file_path, latent_var_models_file_path) {
  load_packages()
  
  sem_data <- read_data(data_file_path)
  latent_var_models <- read_latent_var_models(latent_var_models_file_path)
  
  all_latent_vars <- c('physical', 'veg', 'organic', 'geochem')
  all_dependent_vars <- c("Resp_24", "C_Percent", "WEOC")
  
  for (dependent_var in all_dependent_vars) {
    for (i in 1:nrow(latent_var_models)) {
      selected_latent_vars <- all_latent_vars[as.logical(latent_var_models[i, -1])]
      model_list <- config_model(dependent_var, selected_latent_vars)
      working_model <- model_list[[1]]
      dependent_var_title <- model_list[[2]]
      working_model_scaled <- run_SEM(working_model, sem_data)
      save_observed_vs_modeled_plot(working_model_scaled, sem_data, dependent_var, dependent_var_title, selected_latent_vars)
    }
  }
}

# Run the Function
run_analysis("Data.csv", "latentVarsByModel.csv")
