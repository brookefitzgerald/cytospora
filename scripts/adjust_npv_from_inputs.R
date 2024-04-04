library(tidyverse)
library(jsonlite)

constants         <- read_json("project_constants.json", simplifyVector = T)
N_SAMPLES         <- constants$n_samples
MEAN_MAX_YIELD    <- constants$mean_max_yield_in_simulations
PERCENT_INTEREST  <- constants$percent_interest
DISCOUNT_FACTORS  <- (1/(1+PERCENT_INTEREST)^0:40)
N_TREES           <- 24*24
#changing_settings <- read_csv(SETTINGS_MAIN_CHANGING_FP)
base_settings     <- read_csv(SETTINGS_BASE_FP)

get_npv_sd_adjustments_from_yield_dist <- function(yield){
 
  my_min           <- input$max_yield_min
  my_avg           <- input$max_yield_avg
  my_max           <- input$max_yield_max
  
  
}

adjust_npv <- function(my_avg,
                       new_annual_cost, 
                       new_replant_cost, 
                       new_price,
                       treatment_cost,
                       replant_year,
                       npv){

  replant_cost     <- base_settings$replant_cost_orchard[[1]]
  annual_cost      <- base_settings$annual_cost[[1]]
  old_price        <- base_settings$output_price[[1]]
  old_yield_avg    <- MEAN_MAX_YIELD
  
  costs_vec               <- rep(treatment_cost + annual_cost, 40)
  costs_vec[replant_year] <- costs_vec[replant_year] + replant_cost
  
  discounted_costs       <- sum(costs_vec*DISCOUNT_FACTORS)
  annualized_yield_value <- (npv + discounted_costs)/sum(DISCOUNT_FACTORS)
  annualized_yield       <- annualized_yield_value/old_price
  # update mean of yield
  annualized_yield <- annualized_yield + (my_avg - old_yield_avg)*N_TREES
  
  new_costs_vec               <- rep(treatment_cost + new_annual_cost, 40)
  new_costs_vec[replant_year] <- new_costs_vec[replant_year] + new_replant_cost
  new_net_output              <- annualized_yield*new_price - new_costs_vec
  new_npv                     <- sum(new_net_output*DISCOUNT_FACTORS)
  new_npv
}
adjust_npv(27, 5800, 5500, 1.9, 300, 20, -30000)
