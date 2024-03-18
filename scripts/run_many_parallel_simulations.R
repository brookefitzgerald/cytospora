library(tidyverse)
library(parallel)


# This code will run the orchard simulation with the settings. It is supposed to be ran in parallel and in chunks.

cores <- detectCores()
cl <- makeCluster(cores-1)
clusterEvalQ(cl, {
  source("global.R", local = TRUE, chdir = TRUE, keep.source = FALSE, encoding = "utf-8")
  
})


full_sampled_settings <- read_csv("full_sampled_settings.csv") %>% mutate(row_num=row_number())
max_yields <- full_sampled_settings %>% 
  select(c(row_num, starts_with("max_yield"))) %>% 
  gather(key="key", value="value", starts_with("max_yield")) %>% 
  group_by(row_num) %>% 
  summarize(max_yield = list(value))# %>% 
  arrange(row_num)
full_sampled_settings <- full_sampled_settings %>% 
  select(-starts_with("mutate")) %>% 
  cbind(max_yields)

CHUNK_SIZE <- 5000
PERCENT_INTEREST <- 0.03
N_SETTINGS <- 7560000 #nrow(full_sampled_settings)
N_CHUNKS <- ceiling(N_SETTINGS/CHUNK_SIZE)
chunk_map <- list(
  1=list(max_chunk=200,      id_sub=0),
  2=list(max_chunk=400,      id_sub=200*CHUNK_SIZE - 1),
  3=list(max_chunk=600,      id_sub=400*CHUNK_SIZE - 1),
  4=list(max_chunk=800,      id_sub=600*CHUNK_SIZE - 1),
  5=list(max_chunk=1000,     id_sub=800*CHUNK_SIZE - 1),
  6=list(max_chunk=1200,     id_sub=1000*CHUNK_SIZE - 1),
  7=list(max_chunk=N_CHUNKS, id_sub=1200*CHUNK_SIZE - 1),
)

run_single_simulation <- function(i){
  tryCatch(
    {
      max_yield <- full_sampled_settings[i, sapply(1:40, \(yr) str_c("max_yield_", yr))]
      (do.call(tree_sim, full_sampled_settings[i,]) %>% 
          dplyr::group_by(time, .drop=F) %>%
          dplyr::summarize(
            costs=sum(realized_costs),
            net_returns=sum(output_price*yield)-costs,.groups="keep") %>%
          dplyr::ungroup() %>% 
          summarize(npv=sum(net_returns*(1/((1 + PERCENT_INTEREST)**(time))))))$npv
    },
    error=function(e){
      #print(paste("error ",e, "setting: ",full_settings, collapse=', '))
      e},
    NULL
  )
}

run_single_simulation(1)

get_row_ids_from_chunk <- function(chunk_min, chunk_max){
  ((chunk_min-1)*CHUNK_SIZE + 1):(chunk_max*CHUNK_SIZE)
}

CURR_MAX_CHUNK_LOADED <- 200
runSettingsChunk <- function(cluster, chunk_min, chunk_max=chunk_min){
  if(chunk_max > CURR_MAX_CHUNK_LOADED){
    clusterEvalQ(cluster, {
      
    })
  }
  
  id_set <- get_row_ids_from_chunk(chunk_min, chunk_max)
  system.time({
    results_list <- parallel::parLapply(cl,
                                        id_set,
                                        run_single_simulation)
  })
  print(results_list)
  df <- tibble(npv=unlist(results_list), id=id_set)
  chunk_label <- if_else(chunk_min==chunk_max, chuck_min, str_c(chunk_min, chunk_max, sep='_'))
  write_csv(df, str_c(simulation_results, 'chunk', chunk_label, sep='_'))
}
system.time({runSettingsChunk(cl, 1)})

system.time({runSettingsChunk(cl, 2, 7)})

