library(arrow)
library(jsonlite)
library(tidyverse)

constants <- read_json("project_constants.json", simplifyVector = T)
N_SAMPLES <- constants$n_samples
changing_settings <- read_csv(SETTINGS_MAIN_CHANGING_FP)
base_settings     <- read_csv(SETTINGS_BASE_FP)

plot(as.numeric(as.vector((base_settings[1,2:41]))), type='l', col = alpha('black', 0.2), ylab="Maximum yield", xlab="Year")
for (i in 2:N_SAMPLES){
  
  lines(as.numeric(as.vector((base_settings[i,2:41]))), col = alpha('black', 0.2))
}
lines(as.numeric(as.vector(apply(base_settings[,2:41], 2, mean))))
title("Max yield for simulations")

# arrow allows you to open many csv files
results <- open_csv_dataset(constants$simulation_results_filepath)
subset <- results %>% head(1000) %>% 
  # the same as the parse_id function in run_many_parallel_simulations.R
  mutate(changing_setting_id=ceiling(id/N_SAMPLES),
         base_setting_id=id-(N_SAMPLES*(changing_setting_id-1))) %>% 
  collect()
npv <- results %>% select(npv) %>% collect()

# Function to compute percentiles over each simulation set (changing_setting_id)
add_percentiles <- function(df){
  percentiles <- seq(1, 99, by = 1)
  percentile_values <- quantile(df$npv, probs = percentiles/100)
  
  df <- df %>% 
    summarize(
      changing_setting_id = first(changing_setting_id),
      percentile_000 = min(npv),
      percentile_100 = max(npv), 
      npv_mean       = mean(npv), 
      npv_sd         = sd(npv))
  
  # Create a new column for each percentile
  for (i in 1:length(percentiles)) {
    percentile <- percentiles[i]
    column_name <- paste0("percentile_", str_pad(percentile, width=3, side='left', pad='0'))
    df[[column_name]] <- percentile_values[i]
  }
  return(df %>% select(sort(names(df))))
}

subset2 <- subset %>% 
  group_by(changing_setting_id) %>% 
  group_map(~add_percentiles(.), .keep=T) %>% 
  bind_rows() 

stochastically_dominates_O1 <- function(first_id, second_id){
  subset2[c(first_id, second_id), ] %>% 
    summarize(across(starts_with("percentile"), ~if_else(.[1]>.[2], T, F))) %>%
    as.vector() %>% 
    as.logical() %>% 
    all()
}

stochastically_dominates_O2 <- function(first_id, second_id){
  
  mean_overall <- mean((subset2[c(first_id, second_id), ])$npv_mean, na.rm=T)
  
  dfm <- subset2[c(first_id, second_id), ] %>%
    select(starts_with("percentile")) %>% 
    t() %>% 
    data.frame() %>% 
    pivot_longer(cols=c('X1','X2')) %>% 
    arrange(value) %>% 
    mutate(sum_one = cumsum(if_else(name=='X1', 1, 0)),
           sum_two = cumsum(if_else(name=='X2', 1, 0)),
           comp    = (sum_one <= sum_two) | (value >= mean_overall))
  
  print(dfm[c(1:5, (nrow(dfm) - 5):nrow(dfm)),])
  all(dfm$comp)
}

higher_mean <-function(first_id, second_id){
  (subset2[c(first_id, second_id), ] %>% 
    summarize(if_else(npv_mean[1]>=npv_mean[2], T, F)))[[1]]
}
  
higher_mean(1, 7)
stochastically_dominates_O1(1, 7)
stochastically_dominates_O2(1, 7)
 
plot_stochastic_dom <- function(first_id, second_id){
  plot(as.numeric(as.vector((subset2[first_id, ] %>% select(starts_with("percentile"))))), 0:100, type='s', col = "blue", ylab="percentile", xlab="npv")
  lines(as.numeric(as.vector((subset2[second_id, ] %>% select(starts_with("percentile"))))), 0:100, type='s')
  abline(v=subset2$npv_mean[[first_id]], col="blue")
  abline(v=subset2$npv_mean[[second_id]])
  title(str_c("Empirical cdfs for id ", first_id, " over ", second_id, "   -   ",
              " FO: ", stochastically_dominates_O1(first_id, second_id), 
              " SO: ", stochastically_dominates_O2(first_id, second_id)))
}

plot_stochastic_dom(1, 7)
plot_stochastic_dom(1, 2)
plot_stochastic_dom(1, 3)
plot_stochastic_dom(1, 9)
plot_stochastic_dom(1, 4)
plot_stochastic_dom(1, 5)
plot_stochastic_dom(1, 6)
plot_stochastic_dom(1, 8)



# What to ask
# max yield min/max/average
# peach output price
# annual cost
# replanting cost



