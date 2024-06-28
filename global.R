#Orchard simulation functions and constants
require(mc2d, include.only = "qpert")
require(lhs, include.only="randomLHS")
library(tidyverse)
library(jsonlite)

constants <- read_json("project_constants.json", simplifyVector = T)
L1 <- constants$l1_neighbors_mult_parameter
L2 <- constants$l2_neighbors_mult_parameter
PERCENT_INFECTIONS_IN_BRANCHES <- constants$pct_infections_in_branches
PROB_SCAFFOLD_LOSS <- constants$probability_of_scaffold_loss
PERCENT_YIELD_LOST_FROM_SCAFFOLD_LOSS <- constants$pct_yield_lost_from_scaffold_loss
TREE_FIRST_FULL_YIELD_YEAR <- constants$tree_first_full_yield_year
TREE_LAST_YIELD_YEAR <- constants$tree_last_yield_year


get_year_from_date <- function(date){
  return(as.integer(format(date, "%Y")))
}
get_indices <- function(i, k, n){
  return(max(1, i-k):min(n, i+k))
}

tree_sim <- function(o_rows=24, #Block dimension row
                     o_cols=24, #Block dimension col
                     TH=40,     #Time Horizon
                     start_year=2022, #year simulation starts (only used to transform time at end)
                     remove_trees=FALSE, #Remove tree if the tree dies
                     replant_trees=FALSE, #Replant tree if the tree dies
                     replant_orchard=TRUE, #Replant orchard at replant year
                     replant_years=c(20),  # Year(s) the orchard will be replanted
                     replant_cost_orchard=3000, # Cost to replant the entire orchard
                     replant_cost_tree=20, #Cost to replant an individual tree
                     remove_cost_tree=10, #Cost to remove an individual tree (without replanting)
                     replant_tree_block_size=0, # n trees around replanted tree to also replant
                     remove_tree_block_size=0, # n trees around removed tree to also remove
                     t_disease_year=1, #Year infection starts in the orchard
                     t_treatment_year=1, # Year the disease begins to be treated
                     inf_starts=2,   #Number of infectious starts
                     max_yield=rep(15, 40),  #Yield at maturity per year
                     disease_random_share_of_spread=0.0, # Share of disease spread that is random vs. only to adjacent trees. (0 is completely determined by neighbors, 1 is completely random)
                     disease_spread_rate=.10, #Rate of disease spread to adjacent trees in all directions (can be made more flexible)
                     disease_growth_rate=.2, #Rate of disease growth within a tree
                     control_effort=0.0, #Rate of control effort applied per year
                     control_cost=0.0, #Cost of the control applied
                     output_price=30,  #Output price of product (peaches)
                     annual_cost=10,   #Annual production cost
                     input_annual_price_change=0.0, # Annual percentage change in input prices
                     output_annual_price_change=0.0, # Annual percentage change in output prices
                     treatable_detection_probability = 0.95,
                     random_seed = NULL){
  require(plyr, include.only = "alply"); library(dplyr)
  require(purrr)
  require(matrixcalc)
  require(pracma)
  
  if(length(max_yield)==1){
    max_yield <- rep(max_yield, TH)
  }
  
  #There is an orchard of nxm trees.  
  orc_mat <- ones(o_rows, o_cols)
  
  #Seed infection 
  set.seed(random_seed)
  inf_mat <- zeros(o_rows, o_cols)
  if(inf_starts>0){
    inf_location<-t(replicate(inf_starts,c(sample(o_rows,1),sample(o_cols,1)))) 
    for(i in 1:dim(inf_location)[1]){
      inf_mat[inf_location[i,1],inf_location[i,2]] <- 1
    }
  }
  
  #Disease spread matrices
  tmat_x <- (shift.right(eye(o_rows,o_cols)) + shift.left(eye(o_rows,o_cols)))
  tmat_y <- tmat_x
  tmat_x2 <- (shift.right(eye(o_rows,o_cols), cols=2) + shift.left(eye(o_rows,o_cols), cols=2))
  tmat_y2 <- tmat_x2
  
  #Initial conditions
  #Trees
  tree_shell <- vector("list",TH)
  tree_shell[[1]] <- orc_mat
  #Targeted Pruning Yield Impacts
  pruning_yield_impacts <-  array(data = NA, dim=c(o_rows, o_cols, TH)) 
  pruning_yield_impacts[,,1] <- zeros(o_rows, o_cols)
  #Disease
  disease_shell <- array(data = NA, dim=c(o_rows, o_cols, TH)) 
  # making disease shell an array to initialize when the disease starts ahead of time
  # keeping tree_shell and age_shell vectors because the access time of elements in vectors is higher than a slice of an array
  disease_shell[,,1:(t_disease_year - 1)] <- 0
  disease_shell[,,t_disease_year] <- inf_mat
  
  infected_state <- array(data = NA, dim=c(o_rows, o_cols, TH)) 
  infected_state[,,1:(t_disease_year - 1)] <- inf_mat
  infected_state_branch <- array(data = NA, dim=c(o_rows, o_cols, TH)) 
  infected_state_trunk <- array(data = NA, dim=c(o_rows, o_cols, TH)) 
  
 
  initial_trunk_infection_mask <- runif(inf_mat)*inf_mat >= PERCENT_INFECTIONS_IN_BRANCHES
  infected_state_branch[,,1:(t_disease_year - 1)] <- 0
  infected_state_branch[,,t_disease_year] <- (!initial_trunk_infection_mask)*inf_mat
  
  infected_state_trunk[,,1:(t_disease_year - 1)] <- 0
  infected_state_trunk[,,t_disease_year] <- (initial_trunk_infection_mask)*inf_mat
  
  yield_disease_penalty <- function(disease, current_max_yield){
    # disease is a matrix with 0 if there is no disease, and between 1 and 11 if there is disease
    ((1/(1+150*exp(-0.1-disease)))*current_max_yield)*(disease>0)
  }
  
  get_yearly_yields <- function(tree_ages, pruning_impacts, max_yield, tree_first_full_yield_year=TREE_FIRST_FULL_YIELD_YEAR, tree_last_yield_year=TREE_LAST_YIELD_YEAR){
    # compute the change in max yields that we should see for each age
    yields <- c(0,
                seq(0,max_yield, by=max_yield/(tree_first_full_yield_year-1)), 
                rep(max_yield, tree_last_yield_year-tree_first_full_yield_year))
    #return matrix of yields based on how old each tree is
    if(is.null(dim(tree_ages))){
      # Sometimes, ages is coerced into a single number - this re-expands it into a matrix
      tree_ages <- matrix(rep(tree_ages, o_rows*o_cols), nrow=o_rows)
    }
    apply(tree_ages, c(1,2), function(t){coalesce(yields[t], 0)})*(1-pruning_impacts)
  }
  
  # Setting replanting/removal block size
  if (replant_trees) {
    replant_or_remove_tree_block_size <- replant_tree_block_size
    replant_or_remove_cost <- replant_cost_tree
  } else if (remove_trees){
    replant_or_remove_tree_block_size <- remove_tree_block_size
    replant_or_remove_cost <- remove_cost_tree
  }
  
  #Tree Age
  age_shell <- vector("list",TH)
  age_shell[[1]] <- orc_mat
  n_trees_replanted = 0
  
  #Costs
  cost_shell <- vector("numeric",TH)
  activated_control_effort <- control_effort
  control_effort <- ifelse(t_treatment_year==1, activated_control_effort, 0.0) # will check every year if treatment has started, otherwise 0
  first_year_control_cost <- ifelse(control_effort>0, control_cost, 0.0)
  cost_shell[[1]] <- annual_cost + first_year_control_cost
  
  ####################################
  for(t in 1:(TH-1)){
    #Orchard costs money to run
    cost_shell[[t+1]] <- annual_cost
    
    #Trees age each year
    age_shell[[t+1]] <- age_shell[[t]] + 1
    
    #Grow trees subject to yearly max yield
    current_max_yield <- get_yearly_yields(age_shell[[t]], pruning_yield_impacts[,,t], max_yield[t])
    
    # if the tree is infected, but was infected as a baby, their yield is limited to half of the maximum yield of a full grown tree
    #yield_limits <- (1-((disease_shell[,,t]>0) & ((age_shell[[t]] - disease_shell[,,t]) <=2))*0.5)*max_yield[t]
    tree_shell[[t+1]] <- pmin(current_max_yield - yield_disease_penalty(disease_shell[,,t], current_max_yield))

    # Treatment costs for all years that we are treating the disease
    control_effort <- ifelse(t >= t_treatment_year, activated_control_effort, 0.0)
    cost_shell[[t+1]] <- cost_shell[[t+1]] + ifelse(control_effort==0.0, 0, control_cost)
    
    #Propagate disease if disease spread has started
    if (t >= t_disease_year){
      ##### Calculating new disease incidence
      n_neighboring_trees_w_disease <- disease_shell[,,t]%*%tmat_x + t(t(disease_shell[,,t])%*%tmat_y)
      n_2_neighboring_trees_w_disease <- disease_shell[,,t]%*%tmat_x2 + t(t(disease_shell[,,t])%*%tmat_y2)
      n_2_neighboring_trees_w_disease <- ( # adding the corners of spread
        n_2_neighboring_trees_w_disease + 
        shift.up(n_neighboring_trees_w_disease, 1) + 
        shift.down(n_neighboring_trees_w_disease, 1)
      )
      
      # From probability of infection to changing infection states
      # closest and second level infections always have a 1% chance of getting infected
      prob_infected <- 1-exp(-(L1*n_neighboring_trees_w_disease + L2*n_2_neighboring_trees_w_disease)) + if_else(inf_starts>0, 0.01, 0) 
      # control effort reduces the probability of spread, bounded between 0 and 1, only non-zero if t_treatment_year>=t
      prob_infected <- prob_infected*(1-control_effort)
      infected_state[,,t+1] <- infected_state[,,t] | (runif(o_rows*o_cols) <= prob_infected)
      
      new_infections <- infected_state[,,t+1] - infected_state[,,t]
      new_trunk_infections <- new_infections*runif(new_infections) > PERCENT_INFECTIONS_IN_BRANCHES 
      
      # add new infections 
      infected_state_trunk[,,t+1] <-  infected_state_trunk[,,t] + new_trunk_infections
      infected_state_branch[,,t+1] <- infected_state_branch[,,t] + new_infections - new_trunk_infections
      
      # The disease grows, and new infections only have baseline disease (1)
      disease_shell[,,t+1] <- disease_shell[,,t] + infected_state[,,t+1]
      
      
      # if there is control, remove noticeable branch infections
      if (t>=t_treatment_year){
        treated_branch_infections <- infected_state_branch[,,t+1] & (
          # if a branch disease has been around longer than two years or if disease was probabilistically detected:
          (disease_shell[,,t+1]>=3) | (runif(prob_infected) < treatable_detection_probability)
        ) 
        
        yield_reduction_probs <- runif(prob_infected)
        pruning_yield_impacts[,,t+1] <- 0
        # if treated and full scaffold removal is necessary
        pruning_yield_impacts[,,t+1][treated_branch_infections & (yield_reduction_probs< PROB_SCAFFOLD_LOSS)] <- PERCENT_YIELD_LOST_FROM_SCAFFOLD_LOSS
        
        disease_shell[,,t+1][treated_branch_infections]<- 0
        infected_state[,,t+1][treated_branch_infections]<- 0
        infected_state_branch[,,t+1][treated_branch_infections]<- 0
      }# bounds disease to the maximum amount. Disease is 0 if healthy, and between 1-11 if unhealthy
      disease_shell[,,t+1] <- pmin(11, disease_shell[,,t+1]) 
    }
    
    #Replace dead trees if part of mitigation strategy
    if((replant_trees==TRUE || remove_trees==TRUE) && t != 1){
      to_replant_or_remove <- tree_shell[[t+1]] <= 0.0 # Boolean indicating true if dead, false otherwise
      dead_trees <- which(to_replant_or_remove, arr.ind = TRUE)
      if (nrow(dead_trees)>0){
        for (tree_i in 1:nrow(dead_trees)) {
          neighboring_tree_rows_to_replant_or_remove <- get_indices(i=dead_trees[tree_i, 1], k=replant_or_remove_tree_block_size, n=o_rows)
          neighboring_tree_cols_to_replant_or_remove <- get_indices(i=dead_trees[tree_i, 2], k=replant_or_remove_tree_block_size, n=o_cols)
          to_replant_or_remove[neighboring_tree_rows_to_replant_or_remove, neighboring_tree_cols_to_replant_or_remove] <- TRUE
        }
      }
      if (replant_trees) {
        age_shell[[t+1]][to_replant_or_remove] <- 1.0 # Replanted tree is 1 year old
        tree_shell[[t+1]][to_replant_or_remove] <- 1.0 # Replanted tree has initial yields
        disease_shell[,,t+1][to_replant_or_remove] <- 0.0 # Replanted tree is healthy
        infected_state[,,t+1][to_replant_or_remove] <- 0.0 # Replanted tree is healthy
        infected_state_branch[,,t+1][to_replant_or_remove] <- 0.0 # Replanted tree is healthy
        infected_state_trunk[,,t+1][to_replant_or_remove] <- 0.0 # Replanted tree is healthy
        pruning_yield_impacts[,,t+1][to_replant_or_remove] <- 0.0 # Removed tree doesn't have yield decreases
      } else {
        age_shell[[t+1]][to_replant_or_remove] <- 0.0 # Tree is removed
        tree_shell[[t+1]][to_replant_or_remove] <- 0.0 
        disease_shell[,,t+1][to_replant_or_remove] <- 0.0 # Removed tree does not infect neighboring trees
        infected_state[,,t+1][to_replant_or_remove] <- 0.0 # Removed tree does not infect neighboring trees
        infected_state_branch[,,t+1][to_replant_or_remove] <- 0.0 # Removed tree does not infect neighboring trees
        infected_state_trunk[,,t+1][to_replant_or_remove] <- 0.0 # Removed tree does not infect neighboring trees
        pruning_yield_impacts[,,t+1][to_replant_or_remove] <- 0.0 # Removed tree doesn't have yield decreases
      }
      cost_shell[[t+1]] <- cost_shell[[t+1]] + sum(to_replant_or_remove)*replant_or_remove_cost # Number of trees replanted times their cost
    }
    # Replant orchard if part of mitigation strategy
    if((replant_orchard==TRUE) & (t %in% replant_years)){
      age_shell[[t+1]] <- 1.0 # Replanted tree is 1 year old
      tree_shell[[t+1]] <- 1.0 # Replanted tree has initial yields
      shuffled_inf_mat <- matrix(sample(inf_mat), nrow=o_rows)
      disease_shell[,,t+1] <- shuffled_inf_mat # Replanted trees have initial number of disease starts, but different places
      infected_state[,,t+1] <- shuffled_inf_mat 
      infected_state_trunk[,,t+1] <- shuffled_inf_mat*runif(shuffled_inf_mat) > PERCENT_INFECTIONS_IN_BRANCHES
      infected_state_branch[,,t+1] <-  shuffled_inf_mat - infected_state_trunk[,,t+1]
      pruning_yield_impacts[,,t+1] <- 0.0 # Removed tree doesn't have yield decreases
      cost_shell[[t+1]] <- cost_shell[[t+1]] + replant_cost_orchard
    }
    tree_shell[[t+1]] <- pmax(zeros(o_rows, o_cols), tree_shell[[t+1]]) # set negative yields to zero
  }
  disease_list <- alply(disease_shell, 3)
  tree_health <- pmap_df(list(tree_shell, c(1:TH), cost_shell, age_shell, max_yield, disease_list),
                         function(tree, cntr, yearly_cost, age, yearly_max_yield, disease){
                           # subtracting one from the yield so that the initial yield is truly 0
                           # (non-zero yield is initially necessary for the growth function to work)
                           # but making sure that it doesn't go negative (some trees  
                           # impacted by disease can have a yield < 1)
                           bind_cols(
                             expand_grid(x=c(1:o_cols),y=c(1:o_rows)),
                             yield=pmin( # bounded between 0 and yearly max yield
                               pmax(as.vector(tree)-1, 0),
                               yearly_max_yield
                             ),
                             disease=as.vector(disease),
                             tree_age=as.vector(age)) %>%
                             add_column(
                               time=start_year + cntr - 1,
                               input_price_adjustment_factor=(1+input_annual_price_change)**cntr,
                               output_price_adjustment_factor=(1+output_annual_price_change)**cntr,
                               realized_costs=yearly_cost/numel(orc_mat),
                             )
                         }) %>%
    # adjust the input and output prices by our input and output adjustment percentages
    mutate(net_returns=output_price*output_price_adjustment_factor*yield - realized_costs*input_price_adjustment_factor) %>%
    select(-c(input_price_adjustment_factor, output_price_adjustment_factor))
  
  return(tree_health)
}

## Function to run different disease control scenarios
simulateControlScenarios <- function(year_start,
                                     year_end,
                                     inf_intro,
                                     control1,
                                     t1_cost,
                                     control2,
                                     t2_cost,
                                     replanting_strategy,
                                     include_nd_and_nt=TRUE,
                                     # dots indicate all other arguments are passed on verbatim to the tree_sim function
                                     ...){ 
  # set all of the same settings that are shared between simulations 
  time_horizon = year_end - year_start
  
  tree_sim_with_shared_settings <- partial(tree_sim,
                                           o_rows=24,
                                           o_cols=24,
                                           TH=time_horizon,
                                           start_year=year_start,
                                           replant_trees=(replanting_strategy=='tree_replant'),
                                           replant_orchard=(replanting_strategy=='orchard_replant'),
                                           remove_trees=(replanting_strategy=='tree_remove'),
                                           ...)
  
  #Simulate two control simulations
  #Treatment 1
  t1 <- tree_sim_with_shared_settings(inf_starts = inf_intro,
                                      control_effort = control1,
                                      control_cost = t1_cost) %>%
    rename_with(~str_c("t1_",.),-c(x,y,time))
  
  #Treatment 2
  t2 <- tree_sim_with_shared_settings(inf_starts = inf_intro,
                                      control_effort = control2,
                                      control_cost = t2_cost) %>%
    rename_with(~str_c("t2_",.),-c(x,y,time))
  
  results <- inner_join(t1, t2, by = c("x", "y", "time")) %>%
    dplyr::rename(`Treatment 1`=t1_yield,`Treatment 2`=t2_yield)
  
  if (include_nd_and_nt){
      tree_health_max <- tree_sim_with_shared_settings(inf_starts = 0) %>%   #inf_starts=0 implies no infection for Disease Free
        rename_with(~str_c("max_",.),-c(x,y,time))
      
      tree_health_nt <- tree_sim_with_shared_settings(inf_starts = inf_intro) %>%   #nt implies no treatment
        rename_with(~str_c("nt_",.),-c(x,y,time))
      
      results <- inner_join(tree_health_nt,tree_health_max, by = c("x", "y", "time")) %>%
        inner_join(results, by = c("x", "y", "time")) %>%
        dplyr::rename(`Disease Free`=max_yield,`No Treatment`=nt_yield)
  }
  results
}

generate_setting_change_sets <- function(changing_settings, n_control_settings){

  n_samples <- 400
  # create a latin hypercube sample for all parameters
  x <- randomLHS(n_samples, length(changing_settings)+1)
  sampled_settings <- data.frame(x)
  names(sampled_settings) <- c(names(changing_settings), "control_setting_id")
  i <- 1
  for (setting in names(changing_settings)) {
    sampled_settings[[setting]] <- qpert(
      x[,i], # latin hyper cube sampled probability
      min=changing_settings[[setting]][1],
      max=changing_settings[[setting]][2],
      mode=changing_settings[[setting]][3]
    )
    i <- i+1
  }
  ## The number of infectius introductions is an integer: 
  sampled_settings[["inf_intro"]] <- round(sampled_settings[["inf_intro"]])
  ## Add integers of the three control settings:
  sampled_settings[["control_setting_id"]] <- ceiling(x[,i]*n_control_settings)
  sampled_settings
}

generateManySimulations <- function(simulation_outcome, unchanging_settings, changing_settings, control_settings){
  n_control_settings <- length(control_settings)
  setting_change_sets <- generate_setting_change_sets(changing_settings, n_control_settings)
  #not used in the simulation, just in the npv calculation
  percent_interest <- unchanging_settings$percent_interest
  unchanging_settings$percent_interest <- NULL
  # it doesn't like a list when combining lists
  max_yield <- unchanging_settings$max_yield
  unchanging_settings$max_yield <- NULL
  results_dfm <- data.frame()
  for (i in 1:nrow(setting_change_sets)){
    setting_set <- setting_change_sets[i, ]
    control_setting <- control_settings[setting_set$control_setting_id]
    
    full_settings <- flatten(c(setting_set, control_setting, unchanging_settings))
    #re-adding the list of yields
    full_settings$max_yield <- max_yield
    full_settings$control_setting_id <- NULL
    full_settings$include_nd_and_nt <- FALSE
    if((i%%100==0)||(i==1)){
      print(i)
    }
    simulation_results <- tryCatch(
      {
        do.call(simulateControlScenarios, full_settings) %>%
          select(t1_net_returns, t2_net_returns, x, y, time) %>%
          group_by(time) %>%
          summarize(across(c(t1_net_returns, t2_net_returns),~sum(.,na.rm = T))) %>%
          ungroup() %>%
          mutate(npv_multiplier=1/((1+percent_interest/100.0)**(time - unchanging_settings$year_start))) %>%
          summarize(across(-c(time),list(avg=~mean(.,na.rm = T), npv=~sum(.*npv_multiplier)), .names="{.col}_{.fn}")) %>% 
          mutate(simulation_control_scenario=setting_set$control_setting_id)
      },
      error=function(e){
        print(paste("error in index",i,e, "setting: ",setting_set, collapse=', '))
        NULL},
      NULL)
    if(!is.null(simulation_results)){
      tryCatch({
        prev_names <- names(results_dfm)
        
        full_settings[['replant_years']]<- paste(full_settings[['replant_years']], collapse=" ")
        
        full_settings[['max_yield']]<- paste(full_settings[['max_yield']], collapse=" ")
        current_results <- cbind(simulation_results, full_settings)
        results_dfm <- rbind(results_dfm, current_results)
        },
        error=function(e){
          print(paste("error 2 in index", i, e, "different names:", 
                      paste(setdiff(names(current_results), prev_names), collapse=', ')))
        }
      )
      
    }
    
  }
  results_dfm
}



############## Plotting Functions #####################

plot_line_at_current_year <- function(currentyear) {
  return(geom_vline(xintercept = currentyear,linetype="dashed"))
}
base_plot <- function(.data, ylab, title, ytickfn=label_comma()){
  return(
    .data %>% dplyr::mutate(`<b>Year</b>`=tooltip_str) %>% 
    ggplot(aes(x=time,y=value,color=name, label=`<b>Year</b>`)) +
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = ytickfn) +
      labs(x="Year",y=ylab,title=title) +
      theme_bw(base_size = 15) +
      theme(legend.title = element_blank(),legend.position = "bottom")
  )
}

plot_net_returns <- function(df) {
  return(
    df %>% select(c(ends_with("net_returns"), time, x, y)) %>%
      dplyr::group_by(time) %>%
      dplyr::summarize(across(-c(x,y),~sum(.,na.rm = T))) %>%
      dplyr::ungroup() %>% 
      dplyr::rename(`Disease Free`=max_net_returns,`No Treatment`=nt_net_returns,`Treatment 1`=t1_net_returns,`Treatment 2`=t2_net_returns) %>%
      tidyr::pivot_longer(c(-time)) %>% 
      dplyr::mutate(tooltip_str=paste0('             ', time,
                                       '<br><b>Net Returns</b>: $', format(round(value), big.mark=","),
                                       '<br><b>Category</b>:       ', name)) %>%
      base_plot(ylab="Net returns ($)", title="Net Returns Over Time")
  )
}

plot_returns_to_treatment <- function(df) {
  return(
    df %>% 
      dplyr::select(c(ends_with("net_returns"), time, x, y)) %>%
      dplyr::group_by(time) %>%
      dplyr::summarize(across(-c(x,y),~sum(.,na.rm = T))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(t1_net_returns=(t1_net_returns - nt_net_returns),
             t2_net_returns=(t2_net_returns - nt_net_returns)) %>%
      dplyr::select(c(t1_net_returns, t2_net_returns, time)) %>%
      dplyr::rename(`Treatment 1`=t1_net_returns,`Treatment 2`=t2_net_returns) %>%
      tidyr::pivot_longer(c(-time)) %>% 
      dplyr::mutate(tooltip_str=paste0('                        ', time,
                                       '<br><b>Treatment Returns</b>: $', format(round(value), big.mark=","),
                                       '<br><b>Category</b>:                  ', name)) %>%
      base_plot(ylab="Returns to Treatment ($)",title="Returns to Treatment Over Time")
  )
}

plot_npv <- function(df, r, t0) {
  return(
    df %>% 
      dplyr::select(c(ends_with("net_returns"), time, x, y)) %>%
      dplyr::group_by(time) %>%
      dplyr::summarize(across(-c(x,y),~sum(.,na.rm = T))) %>%
      dplyr::ungroup() %>% 
      dplyr::mutate(t=time-t0, 
             npv_multiplier=((1+r)**-t)) %>% 
      dplyr::arrange(desc(t)) %>% 
      dplyr::mutate(across(-c(time, npv_multiplier, t),~cumsum(.*npv_multiplier), .names="{.col}_npv")) %>%
      dplyr::select(c(ends_with("_npv"), time)) %>%
      dplyr::select(-starts_with("max")) %>% 
      # TODO: change color of traces to be the same as the other plots
      dplyr::rename(`No Treatment`=nt_net_returns_npv,`Treatment 1`=t1_net_returns_npv,`Treatment 2`=t2_net_returns_npv) %>%
      tidyr::pivot_longer(c(-time)) %>% 
      dplyr::mutate(tooltip_str=paste0('                       ', time,
                                       '<br><b>Net Present Value</b>: $', format(round(value), big.mark=","),
                                       '<br><b>Category</b>:                ', name)) %>%
      base_plot(ylab="Net present value ($)",title="Net Present Value Over Time")
  )
}
plot_orchard_yield <- function(df) {
  return (
    df %>%
      dplyr::select(-ends_with(c("net_returns", "realized_costs", "disease"))) %>%
      dplyr::group_by(time) %>%
      dplyr::summarize(across(-c(x,y),~sum(./1000,na.rm = T))) %>%
      tidyr::pivot_longer(c(-time)) %>% 
      dplyr::mutate(tooltip_str=paste0('          ', time,
                                       '<br><b>Yield (lbs)</b>: ', format(round(value*1000), big.mark=","),
                                       '<br><b>Category</b>:    ', name)) %>%
      base_plot(ylab="Yield (lbs/ac)", title="Orchard Yield Over Time", ytickfn=unit_format(unit = "K"))
  )
}
plot_tree_yield <- function(df, x_coord, y_coord){
  return(
    df %>%
      dplyr::select(-ends_with(c("net_returns", "realized_costs", "disease"))) %>%
      dplyr::filter(x==x_coord & y==y_coord) %>%
      dplyr::group_by(time) %>%
      dplyr::summarize(across(-c(x,y),~sum(.,na.rm = T))) %>%
      tidyr::pivot_longer(c(-time)) %>% 
      dplyr::mutate(tooltip_str=paste0('          ', time,
                                       '<br><b>Yield (lbs)</b>: ', format(round(value), big.mark=","),
                                       '<br><b>Category</b>:    ', name)) %>% 
      base_plot(ylab="Yield (lbs/tree)",title="Tree Yield Over Time")
  )
}

base_treatment_comparison_plot <- function(df, ylabel="", label_function=label_dollar()){
  ggplot(df, aes(x=Treatment, fill=Treatment, y=avg, ymin=lower, ymax=upper)) +
    geom_bar(stat="identity") +
    geom_errorbar(width = 0.5, linewidth=1) +
    scale_y_continuous(labels=label_function) +
    labs(y=ylabel) +
    theme_minimal(base_size = 15)
}

plot_treatment_simulation_averages <- function(df){
  df %>%
  tidyr::pivot_longer(
    c(t1_net_returns_npv, t2_net_returns_npv),
    names_to="Treatment",
    names_pattern="(..)_net_returns_npv", 
    values_to="returns_to_treatment") %>%
  dplyr::group_by(Treatment) %>%
  dplyr::summarize(avg=mean(returns_to_treatment),
            se_=1.96*sd(returns_to_treatment),
            lower=avg - se_,
            upper=avg + se_) %>%
  base_treatment_comparison_plot(ylabel="Net present value of each treatment")
}
plot_treatment_simulation_proportions <- function(df){
  df %>%
    dplyr::summarize(
      n_=n(),
      p_t1 = sum(t1_net_returns_npv > t2_net_returns_npv)/n_,
      p_t2 = sum(t2_net_returns_npv > t1_net_returns_npv)/n_
    ) %>%
    dplyr::mutate(
      se_=1.96*sqrt(p_t1*p_t2/n_)
    ) %>% 
    tidyr::pivot_longer(
      starts_with("p_"),
      names_to = "Treatment",
      names_prefix = "p_",
      values_to="avg"
    ) %>% 
    dplyr::mutate(
      lower=avg - se_,
      upper=avg + se_
    ) %>%
    base_treatment_comparison_plot(ylabel="Percentage of simulations with higher net present value", label_function=label_percent())
}
