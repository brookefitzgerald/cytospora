# Annual treatment cost from $0-2000
# Annual treatment effectiveness at reducing spread from 0%-100%
# Percent of branch cankers missed from 0%-100% 
# Replanting year from year 13-20
# 100 simulations

require(mc2d, include.only = "qpert")
require(lhs, include.only="randomLHS")
require(data.table, include.only="CJ")
library(tidyverse)
library(stringr)

library(GGally) 

constants <- read_json("project_constants.json", simplifyVector = T)
SETTINGS_MAIN_CHANGING_FP <- constants$simulation_main_changing_settings_filepath
SETTINGS_BASE_FP <- constants$simulation_base_settings_filepath
N_SAMPLES <- constants$n_samples
MEAN_MAX_YIELD <- constants$mean_max_yield_in_simulations

gen_settings_and_save_to_csv <- function(main_changing_settings_path=SETTINGS_MAIN_CHANGING_FP, base_settings_path=SETTINGS_BASE_FP){
  set.seed(43)
  systematic_variables <- list(
    treatable_detection_probability = seq(0.0,1, 0.1),
    control_effort = seq(0,1, 0.05), # treatment_spread_reduction
    control_cost=seq(100, 2000, 100),
    replanting_year = 13:20
  )
  
  systematic_var_set <- CJ(
    treatable_detection_probability=unlist(systematic_variables["treatable_detection_probability"]),
    control_effort=unlist(systematic_variables["control_effort"]),
    control_cost=unlist(systematic_variables["control_cost"]),
    replant_years=unlist(systematic_variables["replanting_year"]),
    sorted=FALSE)
  
  write_csv(systematic_var_set, main_changing_settings_path)
  
  n_samples <- N_SAMPLES
  other_variables <- list(
    max_yield = c(18, MEAN_MAX_YIELD, 24),
    inf_starts = c(round(256*0.01), round(256*0.03), round(256*0.05)) # low 1%, mid 3%, high 5%
  )
  
  # create a latin hypercube sample for all parameters
  x <- randomLHS(n_samples, 41) # 40 are for each year of the max yield

  sampled_settings <- data.frame(x)
  names(sampled_settings) <- c("inf_starts", sapply(1:40, \(yr) str_c("max_yield_", yr)))
  i <- 1
  for (col_num in 1:ncol(sampled_settings)) {
    setting <- if_else(col_num == 1, "inf_starts", "max_yield")
    sampled_settings[, col_num] <- qpert(
      x[,i], # latin hyper cube sampled probability
      min=other_variables[[setting]][1],
      mode=other_variables[[setting]][2],
      max=other_variables[[setting]][3]
    )
    i <- i+1
  }
  #Scatter_Matrix <- ggpairs(sampled_settings,columns = 1:12, 
  #                          title = "Scatter Plot Matrix", 
  #                          axisLabels = "show") 
  #Scatter_Matrix
  
  sampled_settings_full <- sampled_settings  %>%
    mutate(
      inf_starts = round(inf_starts),
      start_year = 0,
      TH=40,
      t_disease_year = 1,
      t_treatment_year =1,
      remove_cost_tree=30,
      replant_cost_tree=45,
      replant_orchard=TRUE,
      remove_trees=FALSE, 
      replant_trees=FALSE,
      replant_cost_orchard = 5500,
      disease_growth_rate = 0.2,
      annual_cost=5800,
      output_price=1.1,
      input_annual_price_change=0.0,
      output_annual_price_change=0.0
    )
  
  
  write_csv(sampled_settings_full, base_settings_path)
}
full_sampled_settings <- gen_settings_and_save_to_csv()
