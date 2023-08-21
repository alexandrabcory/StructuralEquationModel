
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


# Function to configure the model based on the following inputs:
#     (1) dependent_var (options: Resp_24, C_Percent, WEOC)
#     (2) latent_vars < this is a list of latent variables that you'd like to relate to the dependent variable
#         there are for possible items in the list: veg, physical, geochem, and organic. You can use any combination
#         of these here (and any length). 

config_model <- function(dependent_var, latent_vars) {
  
  # Define which variables go into each latent variable. This is based on output from a random forest model, which 
  # ranked variables in terms of how well they predicted the dependent variable in question. We will, ultimately, 
  # include more variables per latent variable. However, since the pool of data at this time is so small, we 
  # must limit the contents of each latent variable.
  
  if (dependent_var == "Resp_24") {
    physical <- "physical =~ porevolume + Conn_Pores"
    geochem <- "geochem =~ WEOC + Mn"
    organic <- "organic =~ sum_tanin + sum_lipid"
    veg <- "veg =~ landcover"
  } else if (dependent_var == "C_Percent") {
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
  
  latent_vars_dict <- list('physical' = physical, 'geochem' = geochem, 'organic' = organic, 'veg' = veg)
  model_components <- list()
  
  # Collect a list of model components (latent)
  for (latent_var in latent_vars) {
    model_components <- c(model_components, latent_vars_dict[[latent_var]])
  }
  
  # Configure the working model
  working_model <- paste(
    paste(model_components, collapse = "\n"),
    sprintf("%s ~ %s", dependent_var, paste(latent_vars, collapse = " + ")),
    sep = "\n"
  )
  
  return(working_model)
}


# Function to generate and save observed vs. modeled plot
save_observed_vs_modeled_plot <- function(sem_model, data, dependent_var) {
  
  # Generate predicted values
  sem_preds <- lavPredict(sem_model, type = "yhat")
  
  # Select the appropriate dependent variable for prediction
  if (dependent_var == "C_Percent") {
    sem_preds <- data.frame(sem_preds) %>% pull(C_Percent)
  } else if (dependent_var == "WEOC") {
    sem_preds <- data.frame(sem_preds) %>% pull(WEOC)
  } else if (dependent_var == "Resp_24") {
    sem_preds <- data.frame(sem_preds) %>% pull(Resp_24)
  }
  
  # Prepare data for plotting
  plot_data <- data.frame(
    Observed = data[[dependent_var]],
    Modeled = sem_preds
  )
  
  # Fit linear model
  lin_model <- lm(Modeled ~ Observed, data = plot_data)
  
  # Compute R-squared value
  r_squared <- summary(lin_model)$r.squared
  
  # Generate the plot
  p <- ggplot(plot_data, aes(x = Observed, y = Modeled)) +
    geom_point() +
    geom_smooth(method = 'lm') +
    annotate("text", x = Inf, y = Inf, label = paste("R-squared = ", round(r_squared, 2)), hjust = 1, vjust = 1)
  
  # Save the plot
  ggsave(filename = paste0("Observed_vs_Modeled_", dependent_var, ".pdf"), plot = p, width = 12, height = 10, dpi = 150)
}

# Main function that integrates all the steps
run_analysis <- function(data_file_path, latent_var_models_file_path) {
  
  load_packages()
  
  # Directly read the data into data frames
  sem_data <- read_csv(data_file_path)
  latent_var_models <- read_csv(latent_var_models_file_path)
  
  all_latent_vars <- c('physical', 'veg', 'organic', 'geochem')
  all_dependent_vars <- c("Resp_24", "C_Percent", "WEOC")
  
  for (dependent_var in all_dependent_vars) {
    for (i in 1:nrow(latent_var_models)) {
      
      # Use the boolean mask to get the latent variables
      latent_vars <- all_latent_vars[as.logical(latent_var_models[i, -1])]
      
      # configure the model 
      working_model <- config_model(dependent_var, latent_vars)
      
      # run the model
      sem_model <- sem(working_model, data = semdata)
      
      # make and save a plot of observed vs. measured values
      save_observed_vs_modeled_plot(sem_model, sem_data, dependent_var)
    }
  }
}

# Run the main function
run_analysis("Data.csv", "latentVarsByModel.csv")


