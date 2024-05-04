library(tidyverse)
library(jsonlite)
library(arrow)

constants         <- read_json("project_constants.json", simplifyVector = T)
N_SAMPLES         <- constants$n_samples
PERCENT_INTEREST  <- constants$percent_interest
DISCOUNT_FACTORS  <- (1/(1+PERCENT_INTEREST)^0:40)
N_TREES           <- 24*24
MAX_YIELD_AVG     <- constants$mean_max_yield_in_simulations
MAX_YIELD_MIN     <- constants$min_max_yield_in_simulations
MAX_YIELD_MAX     <- constants$max_max_yield_in_simulations
ANNUAL_COST       <- constants$annual_cost_in_simulations
REPLANT_COST      <- constants$replant_cost_in_simulations
OUTPUT_PRICE      <- constants$output_price_in_simulations

#base_settings     <- read_csv(SETTINGS_BASE_FP)

get_npv_sd_adjustments_from_yield_dist <- function(yield){
 
  my_min           <- input$max_yield_min
  my_avg           <- input$max_yield_avg
  my_max           <- input$max_yield_max
  
  
}

calc_discounted_annual_costs <- function(treatment_cost){
  sapply(treatment_cost, function(cost){
    annual_costs <- rep((cost + ANNUAL_COST), 40)
    matrix(annual_costs, nrow=1)%*%matrix(DISCOUNT_FACTORS, ncol=1)
  })}

add_changing_settings_and_annualized_yield_to_simulations <- function(){
  simulations       <- open_csv_dataset(constants$simulation_results_filepath)
  changing_settings <- read_csv(constants$simulation_main_changing_settings_filepath) %>% 
    mutate(changing_setting_id=row_number()) %>% 
    arrow_table()
  sum_discount_factors <- sum(DISCOUNT_FACTORS)
  simulations <- simulations %>% 
    mutate(changing_setting_id = as.integer(ceiling(id/N_SAMPLES))) %>% 
    inner_join(changing_settings, 'changing_setting_id') %>% 
    collect() %>% 
    mutate(
      base_setting_id         = id - (N_SAMPLES*(changing_setting_id-1)),
      annual_costs_discounted = calc_discounted_annual_costs(control_cost),
      replant_cost_discounted = REPLANT_COST*(1/(1+PERCENT_INTEREST)^replant_years), # replant_years is only one year.
      total_discounted_costs  = annual_costs_discounted + replant_cost_discounted,
      annualized_yield_value  = (npv + total_discounted_costs)/sum_discount_factors,
      annualized_yield        = annualized_yield_value/OUTPUT_PRICE
    ) %>% select(
      id, changing_setting_id, base_setting_id, annualized_yield, npv, total_discounted_costs,
      treatable_detection_probability, control_effort, control_cost, replant_years
    )
  return(simulations)
}



get_npv_adjustment_by_replant_year <- function(replant_year=18,
                                               new_revenue=MAX_YIELD_AVG*OUTPUT_PRICE,
                                               new_annual_cost=ANNUAL_COST, 
                                               new_replant_cost=REPLANT_COST){
  #Note that this does not adjust existing yield for the new price (since it is yield dependent)
  
  change_in_annualized_revenue         <- new_revenue - MAX_YIELD_AVG*OUTPUT_PRICE*N_TREES
  change_in_replant_costs              <- new_replant_cost - REPLANT_COST
  change_in_annual_costs               <- rep(new_annual_cost - ANNUAL_COST, 40)
  change_in_annual_costs[replant_year] <- change_in_annual_costs[replant_year] + change_in_replant_costs
  change_in_net_profit                 <- change_in_annualized_revenue - change_in_annual_costs
  npv_adjustment                       <- sum(change_in_net_profit*DISCOUNT_FACTORS)
  npv_adjustment
}

get_npv_adjustments_all_replant_years <- function(new_revenue=MAX_YIELD_AVG*OUTPUT_PRICE,
                                                  new_annual_cost=ANNUAL_COST,
                                                  new_replant_cost=REPLANT_COST){
  sapply(13:20, \(x) {get_npv_adjustment_by_replant_year(x, new_revenue, new_annual_cost, new_replant_cost)})
  }