#Orchard simulation functions and constants

TREE_FIRST_FULL_YIELD_YEAR <- 5

get_year_from_date <- function(date){
  return(as.integer(format(date, "%Y")))
}
get_indices <- function(i, k, n){
  return(max(1, i-k):min(n, i+k))
}

grow_tree_function <- function(tree_ages,                                              #Matrix or vector of tree ages
                               max_yield,                                              #Yield at maturity
                               tree_first_full_yield_year=TREE_FIRST_FULL_YIELD_YEAR,  #Year tree reaches maturity
                               tree_end_year=40){                                      #Productive life of tree
  
  #Growth function - trees mature at `tree_first_full_yield_year`, 
  #                        survive until `tree_end_year`
  growth_function <- approxfun(
    x=c(0, 2, tree_first_full_yield_year+1, tree_end_year+1),
    y=c(0, max_yield/(tree_first_full_yield_year-1), 0, 0),
    method = "constant")
  
  # Calculates the growth rate of trees with arbitrary ages (e.g. from replanting)
  tree_yield_growth <- growth_function(tree_ages)
  
  # If the trees are in a matrix, return a matrix with the same dimensions (by default approxfun flattens inputs to apply the function)
  if (!is_null(ncol(tree_ages))) {
    tree_yield_growth <- matrix(tree_yield_growth, ncol=ncol(tree_ages), byrow=FALSE)
  }
  
  return(tree_yield_growth)
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
                     sim_seed=25){
  
  require(dplyr)
  require(purrr)
  require(matrixcalc)
  require(pracma)
  
  set.seed(sim_seed) #Setting seed for random number generation
  
  #There is an orchard of nxm trees.  
  orc_mat <- ones(o_rows, o_cols)
  
  #Seed infection 
  inf_mat <- zeros(o_rows, o_cols)
  if(inf_starts>0){
    inf_location<-t(replicate(inf_starts,c(sample(o_rows,1),sample(o_cols,1)))) 
    for(i in 1:dim(inf_location)[1]){
      inf_mat[inf_location[i,1],inf_location[i,2]] <- 1
    }
  }
  
  grow_trees <- function(tree_age_matrix, year){
    #Returns tree growth as a function of age for the given maximum yield
    grow_tree_function(tree_age_matrix, max_yield=max_yield[year], tree_end_year=TH)
  }
  
  #Disease spread matrix 
  tmat_identity  <- eye(o_rows,o_cols)*(1+disease_growth_rate)
  tmat_x <- (shift.right(eye(o_rows,o_cols)) + shift.left(eye(o_rows,o_cols)))*disease_spread_rate
  tmat_y <- tmat_x
  
  #Initial conditions
  #Trees
  tree_shell <- vector("list",TH)
  tree_shell[[1]] <- orc_mat
  #Disease
  disease_shell <- array(data = NA, dim=c(o_rows, o_cols, TH)) 
  # making disease shell an array to initialize when the disease starts ahead of time
  # keeping tree_shell and age_shell vectors because the access time of elements in vectors is higher than a slice of an array
  disease_shell[,,1:(t_disease_year - 1)] <- 0
  disease_shell[,,t_disease_year] <- inf_mat
  
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
    tree_shell[[t+1]] <- tree_shell[[t]] + grow_trees(age_shell[[t]], t) - disease_shell[,,t]
    
    # Treatment costs for all years that we are treating the disease
    control_effort <- ifelse(t >= t_treatment_year, activated_control_effort, 0.0)
    cost_shell[[t+1]] <- cost_shell[[t+1]] + ifelse(control_effort==0.0, 0, control_cost)
    
    #Propagate disease if disease spread has started
    if (t >= t_disease_year){
      change_in_disease_spread <- disease_shell[,,t]%*%tmat_x + t(t(disease_shell[,,t])%*%tmat_y)
      disease_shell[,,t+1] <- disease_shell[,,t]%*%tmat_identity + change_in_disease_spread*(1-disease_random_share_of_spread)
      
      # If disease is spread randomly, shuffle the 
      if (disease_random_share_of_spread > 0){
        random_disease_growth_amounts <- change_in_disease_spread*disease_random_share_of_spread
        shuffled_random_spread <- sample(random_disease_growth_amounts, o_rows*o_cols)
        disease_shell[,,t+1] <- disease_shell[,,t+1] + matrix(shuffled_random_spread, nrow=o_rows)
      }
      
      #Disease is mitigated if present
      disease_shell[,,t+1] <- disease_shell[,,t+1]*(1-control_effort)
      disease_shell[,,t+1] <- pmax(zeros(o_rows, o_cols), disease_shell[,,t+1]) # sets any negatives to zero
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
      } else {
        age_shell[[t+1]][to_replant_or_remove] <- 0.0 # Tree is removed
        tree_shell[[t+1]][to_replant_or_remove] <- 0.0 
        disease_shell[,,t+1][to_replant_or_remove] <- 0.0 # Removed tree does not infect neighboring trees
      }
      cost_shell[[t+1]] <- cost_shell[[t+1]] + sum(to_replant_or_remove)*replant_or_remove_cost # Number of trees replanted times their cost
    }
    # Replant orchard if part of mitigation strategy
    if((replant_orchard==TRUE) & (t %in% replant_years)){
      age_shell[[t+1]] <- 1.0 # Replanted tree is 1 year old
      tree_shell[[t+1]] <- 1.0 # Replanted tree has initial yields
      shuffled_inf_mat <- matrix(sample(inf_mat), nrow=o_rows)
      disease_shell[,,t+1] <- shuffled_inf_mat # Replanted trees have initial number of disease starts, but different places
      cost_shell[[t+1]] <- cost_shell[[t+1]] + replant_cost_orchard
      # control_effort <- 0 
      # TODO: Figure out if disease should spread again/if control effort should continue. 
    }
    tree_shell[[t+1]] <- pmax(zeros(o_rows, o_cols), tree_shell[[t+1]]) # set negative yields to zero
  }
  
  tree_health <- pmap_df(list(tree_shell, c(1:TH), cost_shell, age_shell, max_yield),
                         function(tree, cntr, yearly_cost, age, yearly_max_yield){
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
  
  tree_health_max <- tree_sim_with_shared_settings(inf_starts = 0) %>%   #inf_starts=0 implies no infection for Disease Free
    rename_with(~str_c("max_",.),-c(x,y,time))
  
  tree_health_nt <- tree_sim_with_shared_settings(inf_starts = inf_intro) %>%   #nt implies no treatment
    rename_with(~str_c("nt_",.),-c(x,y,time))
  
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
  
  inner_join(tree_health_nt,tree_health_max, by = c("x", "y", "time")) %>%
    inner_join(t1, by = c("x", "y", "time")) %>%
    inner_join(t2, by = c("x", "y", "time")) %>%
    rename(`Disease Free`=max_yield,`No Treatment`=nt_yield,`Treatment 1`=t1_yield,`Treatment 2`=t2_yield)
}

generate_setting_change_sets <- function(changing_settings, n_control_settings){
  require(EnvStats, include.only = "rtri")
  n_samples <- 100
  get_sample_from_triangle_distribution <-function(name, n=n_samples){
    rtri(n,
         min=changing_settings[[name]][1],
         max=changing_settings[[name]][2],
         mode=changing_settings[[name]][3])
  }
  changed_settings <- data.frame(
    inf_intro                     =get_sample_from_triangle_distribution("inf_intro"),
    disease_random_share_of_spread=get_sample_from_triangle_distribution("disease_random_share_of_spread"),
    disease_growth_rate           =get_sample_from_triangle_distribution("disease_growth_rate"),
    control1                      =get_sample_from_triangle_distribution("control1"),
    control2                      =get_sample_from_triangle_distribution("control2"),
    annual_cost                   =get_sample_from_triangle_distribution("annual_cost"),
    control_setting_id            =rep(1, n_samples)
  )
  for (i in 2:n_control_settings){
    changed_settings <- changed_settings %>% 
      bind_rows(
        changed_settings %>%
          mutate(control_setting_id=i)
      )
  }
  changed_settings
}

generateManySimulations <- function(simulation_outcome, unchanging_settings, changing_settings, control_settings){
  n_control_settings <- length(control_settings)
  setting_change_sets <- generate_setting_change_sets(changing_settings, n_control_settings)
  max_yield <- unchanging_settings$max_yield
  unchanging_settings$max_yield <- NULL
  results_dfm <- data.frame()
  for (i in 1:nrow(setting_change_sets)){
    setting_set <- setting_change_sets[i, ]
    control_setting <- control_settings[setting_set$control_setting_id]
    
    full_settings <- flatten(c(setting_set, control_setting, unchanging_settings))
    full_settings$max_yield <- 15 #max_yield
    full_settings$control_setting_id <- NULL
    if((i%%100==0)||(i==1)){
      print(i)
    }
    simulation_results <- tryCatch(
      do.call(simulateControlScenarios, full_settings) %>%
        select(t1_net_returns, t2_net_returns, x, y, time) %>%
        group_by(time) %>%
        summarize(across(c(t1_net_returns, t2_net_returns),~sum(.,na.rm = T))) %>%
        ungroup() %>%
        summarize(across(-c(time),~mean(.,na.rm = T))) %>% 
        mutate(simulation_control_scenario=setting_set$control_setting_id),
      error=function(e){
        print(paste("error in index",i,e, "setting: ",setting_set, collapse=', '))
        NULL},
      NULL)
    if(!is.null(simulation_results)){
      tryCatch({
        prev_names <- names(results_dfm)
        current_results <- cbind(simulation_results, full_settings)
        results_dfm <- rbind(results_dfm, current_results)
        },
        error=function(e){
          #print(glimpse(current_results))
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
    ggplot(data=.data, aes(x=time,y=value,color=name)) +
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
      group_by(time) %>%
      summarize(across(-c(x,y),~sum(.,na.rm = T))) %>%
      ungroup() %>% 
      rename(`Disease Free`=max_net_returns,`No Treatment`=nt_net_returns,`Treatment 1`=t1_net_returns,`Treatment 2`=t2_net_returns) %>%
      pivot_longer(c(-time)) %>%
      base_plot(ylab="Net returns ($)", title="Net Returns Over Time")
  )
}

plot_returns_to_treatment <- function(df) {
  return(
    df %>% 
      select(c(ends_with("net_returns"), time, x, y)) %>%
      group_by(time) %>%
      summarize(across(-c(x,y),~sum(.,na.rm = T))) %>%
      ungroup() %>%
      mutate(t1_net_returns=(t1_net_returns - nt_net_returns),
             t2_net_returns=(t2_net_returns - nt_net_returns)) %>%
      select(c(t1_net_returns, t2_net_returns, time)) %>%
      rename(`Treatment 1`=t1_net_returns,`Treatment 2`=t2_net_returns) %>%
      pivot_longer(c(-time)) %>%
      base_plot(ylab="Returns to Treatment ($)",title="Returns to Treatment Over Time")
  )
}

plot_npv <- function(df, r, t0) {
  return(
    df %>% 
      select(c(ends_with("net_returns"), time, x, y)) %>%
      group_by(time) %>%
      summarize(across(-c(x,y),~sum(.,na.rm = T))) %>%
      ungroup() %>% 
      mutate(t=time-t0, 
             npv_multiplier=((1+r)**-t)) %>% 
      arrange(desc(t)) %>% 
      mutate(across(-c(time, npv_multiplier, t),~cumsum(.*npv_multiplier), .names="{.col}_npv")) %>%
      select(c(ends_with("_npv"), time)) %>%
      select(-starts_with("max")) %>% 
      # TODO: change color of traces to be the same as the other plots
      rename(`No Treatment`=nt_net_returns_npv,`Treatment 1`=t1_net_returns_npv,`Treatment 2`=t2_net_returns_npv) %>%
      pivot_longer(c(-time)) %>%
      base_plot(ylab="Net present value ($)",title="Net Present Value Over Time")
  )
}
plot_orchard_yield <- function(df) {
  return (
    df %>%
      dplyr::select(-ends_with(c("net_returns", "realized_costs"))) %>%
      group_by(time) %>%
      summarize(across(-c(x,y),~sum(./1000,na.rm = T))) %>%
      pivot_longer(c(-time)) %>%
      base_plot(ylab="Yield (lbs/ac)", title="Orchard Yield Over Time", ytickfn=unit_format(unit = "K"))
  )
}
plot_tree_yield <- function(df, x_coord, y_coord){
  return(
    df %>%
      dplyr::select(-ends_with(c("net_returns", "realized_costs"))) %>%
      filter(x==x_coord & y==y_coord) %>%
      group_by(time) %>%
      summarize(across(-c(x,y),~sum(.,na.rm = T))) %>%
      pivot_longer(c(-time)) %>%
      base_plot(ylab="Yield (lbs/tree)",title="Tree Yield Over Time")
  )
}

