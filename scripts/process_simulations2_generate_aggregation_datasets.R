library(tidyverse)
require(conflicted, include.only = "conflict_prefer")
require(data.table, include.only = "CJ")
conflict_prefer("mutate", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

source('~/cytospora/scripts/process_simulations1_adjust_npv_from_inputs.R')
simulations <- add_changing_settings_and_annualized_yield_to_simulations()

# Function to compute percentiles over each simulation set (changing_setting_id)
add_percentiles <- function(df, grouping_vars){
  percentiles <- seq(1, 99, by = 1)
  percentile_values <- quantile(df$npv, probs = percentiles/100)
  
  df <- df %>% 
    dplyr::summarize(
      across(all_of(grouping_vars), ~first(.)),
      percentile_000 = min(npv),
      percentile_100 = max(npv), 
      npv_mean       = mean(npv), 
      npv_median     = median(npv), 
      npv_sd         = sd(npv))
  
  # Create a new column for each percentile
  for (i in 1:length(percentiles)) {
    percentile <- percentiles[i]
    column_name <- paste0("percentile_", str_pad(percentile, width=3, side='left', pad='0'))
    df[[column_name]] <- percentile_values[i]
  }
  return(df %>% select(sort(names(df))))
}

gen_cost_effectiveness_percentiles_per_treatment <- function(replant_year=20){
  simulations %>%  filter(replant_years==replant_year) %>% 
    group_by(control_effort, control_cost) %>% 
    group_map(~add_percentiles(., grouping_vars=c("control_effort", "control_cost")), .keep=T) %>% 
    bind_rows() 
}
gen_cost_effectiveness_percentiles_per_replant_year <- function(input_control_effort=NULL, input_control_cost=NULL){
  df <- simulations
  # both need to be input to not throw an error
  if(!is.null(input_control_effort) & !is.null(input_control_cost)){
    df <- df %>% filter(round(control_effort, 2)==input_control_effort, round(control_cost)==input_control_cost)
  }
  df %>% 
    group_by(replant_years) %>% 
    group_map(~add_percentiles(., grouping_vars=c("replant_years")), .keep=T) %>% 
    bind_rows() 
}
gen_cost_effectiveness_percentiles_per_detection_prob <- function(input_control_effort=NULL, input_control_cost=NULL){
  df <- simulations
  # both need to be input to not throw an error
  if(!is.null(input_control_effort) & !is.null(input_control_cost)){
    df <- df %>% filter(control_effort==input_control_effort, control_cost==input_control_cost)
  }
  df %>% 
    group_by(treatable_detection_probability) %>% 
    group_map(~add_percentiles(., grouping_vars=c("treatable_detection_probability")), .keep=T) %>% 
    bind_rows()
}

N <- 1
stochastically_dominates_O1 <- function(first_id, second_id, dfm=sims){
  if(mod(N, 1000)==0){
    print(N)
  }
  N <<- N + 1
  # Only first order stochastically dominates if it's higher at each percentile than the other 
  dfm[c(first_id, second_id), ] %>% 
    dplyr::summarize(across(starts_with("percentile"), ~if_else(.[1]>.[2], T, F))) %>%
    as.vector() %>% 
    as.logical() %>% 
    all()
}
N <- 1
stochastically_dominates_O2 <- function(first_id, second_id, dfm=sims){
  # second order stochastically dominates if a) the mean is higher and b) the 
  # area under the first probability density function (cumulative density function) 
  # is less than the second (lower risk of a bad outcome).
  mean_overall <- mean((dfm[c(first_id, second_id), ])$npv_mean, na.rm=T)
  if(mod(N, 1000)==0){
    print(N)
  }
  N <<- N + 1
  dfm <- dfm[c(first_id, second_id), ] %>%
    select(starts_with("percentile")) %>% 
    t() %>% 
    data.frame() 
  names(dfm) <- c('X1','X2')
  dfm <- dfm %>% 
    pivot_longer(cols=c('X1','X2')) %>% 
    dplyr::arrange(value) %>% 
    dplyr::mutate(sum_one = cumsum(if_else(name=='X1', 1, 0)),
           sum_two = cumsum(if_else(name=='X2', 1, 0)),
           comp    = (sum_one <= sum_two) | (value >= mean_overall))
  
  all(dfm$comp)
}

higher_mean <-function(first_id, second_id, dfm=sims){
  (dfm[c(first_id, second_id), ] %>% 
     summarize(if_else(npv_mean[1]>=npv_mean[2], T, F)))[[1]]
}

