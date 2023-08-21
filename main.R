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

# Configure model based on dependent variable and list of latent variables
config_model <- function(dependent_var, latent_vars) {
  model_components <- list()
  
  # Define the latent variables based on the dependent variable
  physical <- "physical =~ porevolume + Conn_Pores"
  geochem <- "geochem =~ WEOC + Mn"
  organic <- "organic =~ sum_tanin + sum_lipid"
  veg <- "veg =~ landcover"
  
  dependent_var_title <- switch(dependent_var,
                                "Resp_24" = "24 Hour Respiration",
                                "Percent_C" = "Percent Carbon",
                                "WEOC" = "Water-Extractable Organic Carbon")
  
  # Create the list of model components based on specified latent variables
  latent_vars_dict <- list('physical' = physical, 'geochem' = geochem, 'organic' = organic, 'veg' = veg)
  for (latent_var in latent_vars) {
    model_components <- c(model_components, latent_vars_dict[[latent_var]])
  }
  
  # Create the working model string
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
  
  # Create prediction data
  sem_preds <- lavPredict(working_model_scaled, type = "yhat")
  sem_preds <- data.frame(sem_preds) %>% pull(Resp_24)
  
  # Create data frame for plotting
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
generate_file_name_suffix <- function(title, latent_vars) {
  return(paste0(title, "_", paste(latent_vars, collapse = "_"), ".pdf"))
}

# Main function to call other functions
run_analysis <- function(file_path, dependent_var, latent_vars) {
  load_packages()
  sem_data <- read_data(file_path)
  model_list <- config_model(dependent_var, latent_vars)
  working_model <- model_list[[1]]
  dependent_var_title <- model_list[[2]]
  working_model_scaled <- run_SEM(working_model, sem_data)
  save_observed_vs_modeled_plot(working_model_scaled, sem_data, dependent_var, dependent_var_title, latent_vars)
}

####################      CHANGE THESE THINGS   #######################################
dependent_var = "Resp_24"

# Options for latent variables include: veg, physical, organic, geochem
# The user can do any combination and any length
latent_vars = c('veg', 'physical', 'organic', 'geochem')

#######################   Run the Function      ####################################

run_analysis("Data.csv", dependent_var, latent_vars)

