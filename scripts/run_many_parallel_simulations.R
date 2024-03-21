library(tidyverse)
library(parallel)
library(jsonlite)
library(glue)

constants <- read_json("project_constants.json", simplifyVector = T)
SETTINGS_MAIN_CHANGING_FP <- constants$simulation_main_changing_settings_filepath
SETTINGS_BASE_FP <- constants$simulation_base_settings_filepath
N_SAMPLES <- constants$n_samples
SIMULATION_RESULTS_PATH <- constants$simulation_results_filepath

# This code will run the orchard simulation with the settings. It is supposed to be ran in parallel and in chunks.

cores <- detectCores()
cl <- makeCluster(20) # for big server
clusterEvalQ(cl, {
  constants <- read_json("project_constants.json", simplifyVector = T)
  SETTINGS_MAIN_CHANGING_FP <- constants$simulation_main_changing_settings_filepath
  SETTINGS_BASE_FP <- constants$simulation_base_settings_filepath
  N_SAMPLES <- constants$n_samples
  SIMULATION_RESULTS_PATH <- constants$simulation_results_filepath
  source("global.R", local = TRUE, chdir = TRUE, keep.source = FALSE, encoding = "utf-8")
  changing_settings <- read_csv(SETTINGS_MAIN_CHANGING_FP)
  base_settings <- read_csv(SETTINGS_BASE_FP) %>% mutate(row_num=row_number())
  ## takes max_yield_{#} columns and turns them into a list which is one column
  max_yields <- base_settings %>% 
    select(c(row_num, starts_with("max_yield"))) %>% 
    gather(key="key", value="value", starts_with("max_yield")) %>% 
    group_by(row_num) %>% 
    summarize(max_yield = list(value))
  base_settings <- base_settings %>% 
    select(-starts_with("max_yield")) %>% 
    inner_join(max_yields) %>% 
    select(-row_num)
  PERCENT_INTEREST <- 0.03
  
  parse_ids <- function(i){
    # From an overall setting id, returns the changing setting id `j` and the base setting id `n`
    # i=1-500, j=1; i=501-1000, j=2
    # i = 1, 501 -> n=1; i=500, 1000 -> n=500
    j <- ceiling(i/N_SAMPLES)
    n <- i-(N_SAMPLES*(j-1))
    return(c('j'=j, 'n'=n))
  }
})




run_single_simulation <- function(i){
  tryCatch(
    {
      ids <- parse_ids(i)
      setting_i <- c(changing_settings[ids[['j']], ], base_settings[ids[['n']], ])
      setting_i$max_yield <- setting_i$max_yield[[1]]
      (do.call(tree_sim, setting_i) %>% 
          dplyr::group_by(time, .drop=F) %>%
          dplyr::summarize(
            costs=sum(realized_costs),
            net_returns=sum(net_returns),.groups="keep") %>%
          dplyr::ungroup() %>% 
          summarize(npv=sum(net_returns*(1/((1 + PERCENT_INTEREST)**(time))))))$npv
    },
    error=function(e){
      e},
    NULL
  )
}


# how many simulations to do before writing results chunk to disk
CHUNK_SIZE <- 10000
N_SIMULATIONS <- nrow(base_settings)*nrow(changing_settings)
MAX_CHUNK <- ceiling(N_SIMULATIONS/CHUNK_SIZE)


runAllSettingsInChunks <- function(STARTING_CHUNK=1){
  for (chunk_id in STARTING_CHUNK:MAX_CHUNK) {
    print(glue("Starting Chunk # {chunk_id}"))
    # makes sure we aren't pulling ids that don't exist if CHUNK_SIZE = 1000, but our max n simulations is 99500, 
    # this will return chunks of 1000 smiulations until the last which will be 500 (99001:99500)
    if(chunk_id == MAX_CHUNK) {
      id_set <- (CHUNK_SIZE*(MAX_CHUNK - 1) + 1):N_SIMULATIONS
    } else {
      id_set <- (CHUNK_SIZE*(chunk_id - 1)  + 1):(chunk_id*CHUNK_SIZE)
    }
    
    system.time({
      results_list <- parallel::parLapply(cl,
                                          id_set,
                                          run_single_simulation)
    })
    df <- tibble(npv=unlist(results_list), id=id_set)
    write_csv(df, str_c(SIMULATION_RESULTS_PATH, '/simulation_results_chunk_', chunk_id, '.csv', sep=''))
  }
  
}

runAllSettingsInChunks(STARTING_CHUNK=2)

results_list <- parallel::parLapply(cl,
                                    1:CHUNK_SIZE,
                                    run_single_simulation)
df <- tibble(npv=unlist(results_list), id=1:CHUNK_SIZE)
write_csv(df, str_c(SIMULATION_RESULTS_PATH, '/simulation_results_chunk_1.csv', sep=''))
