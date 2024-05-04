library(arrow)
library(glue)
library(tidyverse)

source('~/cytospora/scripts/process_simulations2_generate_aggregation_datasets.R')


# 3 steps: 
# 1) calc stochastic dominance for each treatment cost/treatment effectiveness
# 2) calc stochastic dominance for each replant year
# 3) calc stochastic dominance for each percentage accuracy


OUTPUT_DIR <- '/data/bfitzgerald/cytospora/stoch_dom_results'
calc_simulation_stochastic_dominance <- function(pairwise_combinations_dfm, simulations_dfm, name_prefix, output_dir=OUTPUT_DIR, chunk_size = 50000){
  output_sub_dir <- glue('{output_dir}/{name_prefix}')
  if (!file.exists(output_sub_dir)) {
    # Create the directory if it doesn't exist
    dir.create(output_sub_dir)
  }
  # First, calculate the pairwise stochastic dominances and save the output
  start <- 1
  end <- min(chunk_size, nrow(pairwise_combinations_dfm))
  n_chunks <- ceiling(nrow(pairwise_combinations_dfm)/chunk_size)
  for (section_N in 1:n_chunks) {
    print(glue("Starting new batch: {section_N}"))
    N <- 1
    print("Starting first order calculations")
    fd_stoch_dom <- apply(pairwise_combinations_dfm[start:end,], 1, \(ids) stochastically_dominates_O1(ids[1], ids[2], simulations_dfm))
    N <- 1
    print("Starting second order calculations")
    sd_stoch_dom <- apply(pairwise_combinations_dfm[start:end,], 1, \(ids) stochastically_dominates_O2(ids[1], ids[2], simulations_dfm))
    write_csv(pairwise_combinations_dfm[start:end,] %>% mutate(fd_stoch_dom=fd_stoch_dom, sd_stoch_dom=sd_stoch_dom), glue('{output_sub_dir}/stochastic_dom_{name_prefix}_{section_N}.csv'))
    start <- start + chunk_size
    end <- min(end + chunk_size, nrow(pairwise_combinations_dfm))
  }
  
  # Second, aggregate the output and save to output dir
  stoch_dom_dfm <- open_csv_dataset(output_sub_dir) %>% collect()
  sd_dfm_summary <- stoch_dom_dfm  %>% 
    pivot_longer(cols=c(first, second), names_to = "order", values_to="id") %>% 
    group_by(id) %>% 
    dplyr::summarize(
      across(ends_with("stoch_dom"), ~sum(if_else(order=="first", ., !.))),
      n_combinations = n()
    ) %>% 
    mutate(across(ends_with("stoch_dom"), ~./n_combinations, .names="{.col}_pct"))
  
  write_csv_arrow(stoch_dom_dfm, glue("{output_dir}/stochastic_dom_{name_prefix}_all.csv"))
  write_csv(simulations_dfm %>% select(-starts_with("percentile")) %>% bind_cols(sd_dfm_summary), glue("{output_dir}/stochastic_dom_{name_prefix}_agg.csv"))
}


# STEP ONE: TREATMENT COST/EFFECTIVENESS
sims <- gen_cost_effectiveness_percentiles_per_treatment(replant_year=20)
pairwise_combinations <- CJ(first=1:nrow(sims), second=1:nrow(sims)) %>% filter(first!=second)
calc_simulation_stochastic_dominance(pairwise_combinations, sims, name_prefix = 'treatment')

sims <- gen_cost_effectiveness_percentiles_per_treatment(replant_year=14)
pairwise_combinations <- CJ(first=1:nrow(sims), second=1:nrow(sims)) %>% filter(first!=second)
calc_simulation_stochastic_dominance(pairwise_combinations, sims, name_prefix = 'treatment14')

sims <- gen_cost_effectiveness_percentiles_per_treatment(replant_year=16)
pairwise_combinations <- CJ(first=1:nrow(sims), second=1:nrow(sims)) %>% filter(first!=second)
calc_simulation_stochastic_dominance(pairwise_combinations, sims, name_prefix = 'treatment16')

sims <- gen_cost_effectiveness_percentiles_per_treatment(replant_year=18)
pairwise_combinations <- CJ(first=1:nrow(sims), second=1:nrow(sims)) %>% filter(first!=second)
calc_simulation_stochastic_dominance(pairwise_combinations, sims, name_prefix = 'treatment18')


# STEP TWO: REPLANT YEAR
replant_sims <- gen_cost_effectiveness_percentiles_per_replant_year()
pairwise_replant_combinations <- CJ(first=1:nrow(replant_sims), second=1:nrow(replant_sims)) %>% filter(first!=second)
calc_simulation_stochastic_dominance(pairwise_replant_combinations, replant_sims, name_prefix = "replant_year")

replant_sims <- gen_cost_effectiveness_percentiles_per_replant_year(input_control_effort=0.5, input_control_cost=500)
pairwise_replant_combinations <- CJ(first=1:nrow(replant_sims), second=1:nrow(replant_sims)) %>% filter(first!=second)
calc_simulation_stochastic_dominance(pairwise_replant_combinations, replant_sims, name_prefix = "replant_year2")

replant_sims <- gen_cost_effectiveness_percentiles_per_replant_year(input_control_effort=0.8, input_control_cost=1200)
pairwise_replant_combinations <- CJ(first=1:nrow(replant_sims), second=1:nrow(replant_sims)) %>% filter(first!=second)
calc_simulation_stochastic_dominance(pairwise_replant_combinations, replant_sims, name_prefix = "replant_year3")


# STEP THREE: PERCENTAGE ACCURACY
detection_prob_sims <- gen_cost_effectiveness_percentiles_per_detection_prob()
pairwise_detection_prob_combinations <- CJ(first=1:nrow(detection_prob_sims), second=1:nrow(detection_prob_sims)) %>% filter(first!=second)
calc_simulation_stochastic_dominance(pairwise_detection_prob_combinations, detection_prob_sims, name_prefix = "detection_prob")

detection_prob_sims <- gen_cost_effectiveness_percentiles_per_detection_prob(input_control_effort=0.5, input_control_cost=500)
pairwise_detection_prob_combinations <- CJ(first=1:nrow(detection_prob_sims), second=1:nrow(detection_prob_sims)) %>% filter(first!=second)
calc_simulation_stochastic_dominance(pairwise_detection_prob_combinations, detection_prob_sims, name_prefix = "detection_prob2")

detection_prob_sims <- gen_cost_effectiveness_percentiles_per_detection_prob(input_control_effort=0.8, input_control_cost=1200)
pairwise_detection_prob_combinations <- CJ(first=1:nrow(detection_prob_sims), second=1:nrow(detection_prob_sims)) %>% filter(first!=second)
calc_simulation_stochastic_dominance(pairwise_detection_prob_combinations, detection_prob_sims, name_prefix = "detection_prob3")
