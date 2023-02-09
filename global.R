#Orchard simulation functions and constants

TREE_FIRST_FULL_YIELD_YEAR <- 5

get_year_from_date <- function(date){
  return(as.integer(format(date, "%Y")))
}
get_indices <- function(i, k, n){
  return(max(1, i-k):min(n, i+k))
}

grow_tree_function <- function(tree_ages,                                      #Matrix or vector of tree ages
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
                     replant_trees=FALSE, #Replant tree if the tree dies
                     replant_orchard=TRUE, #Replant orchard at replant year
                     replant_years=c(20),  # Year(s) the orchard will be replanted
                     replant_cost_orchard=3000, # Cost to replant the entire orchard
                     replant_cost_tree=10, #Cost to replant an individual tree
                     replant_tree_block_size=0, # n trees around replanted tree to also replant
                     t_disease_year=1, #Year infection starts in the orchard
                     t_treatment_year=1, # Year the disease begins to be treated
                     inf_starts=2,   #Number of infectious starts
                     max_yield=15,   #Yield at maturity
                     disease_spread_rate=.10, #Rate of disease spread to adjacent trees in all directions (can be made more flexible)
                     disease_growth_rate=.2, #Rate of disease growth within a tree
                     control_effort=0.0, #Rate of control effort applied per year
                     control_cost=0.0, #Cost of the control applied
                     output_price=30,  #Output price of product (peaches)
                     annual_cost=10,   #Annual production cost
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
  
  grow_trees <- function(tree_age_matrix){
    #Returns tree growth as a function of age for the given maximum yield
    grow_tree_function(tree_age_matrix, max_yield=max_yield, tree_end_year=TH)
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
    
    #Grow trees subject to damage
    tree_shell[[t+1]] <- tree_shell[[t]] + grow_trees(age_shell[[t]]) - disease_shell[,,t]
    
    # Treatment costs for all years that we are treating the disease
    control_effort <- ifelse(t >= t_treatment_year, activated_control_effort, 0.0)
    cost_shell[[t+1]] <- cost_shell[[t+1]] + ifelse(control_effort==0.0, 0, control_cost)
    
    #Propagate disease if disease spread has started
    if (t >= t_disease_year){
      disease_shell[,,t+1] <- disease_shell[,,t]%*%(tmat_x + tmat_identity) + t(t(disease_shell[,,t])%*%tmat_y)
      
      #Disease is mitigated if present
      disease_shell[,,t+1] <- disease_shell[,,t+1]*(1-control_effort)
      disease_shell[,,t+1] <- pmax(zeros(o_rows, o_cols), disease_shell[,,t+1]) # sets any negatives to zero
    }
    
    #Replace dead trees if part of mitigation strategy
    if(replant_trees==TRUE && t != 1){
      to_replant <- tree_shell[[t+1]] <= 0.0 #Boolean indicating true if dead, false otherwise
      dead_trees <- which(to_replant, arr.ind = TRUE)
      if (nrow(dead_trees)>0){
        for (tree_i in 1:nrow(dead_trees)) {
          neighboring_tree_rows_to_replant <- get_indices(i=dead_trees[tree_i, 1], k=replant_tree_block_size, n=o_rows)
          neighboring_tree_cols_to_replant <- get_indices(i=dead_trees[tree_i, 2], k=replant_tree_block_size, n=o_cols)
          to_replant[neighboring_tree_rows_to_replant, neighboring_tree_cols_to_replant] <- TRUE
        }
      }
      
      age_shell[[t+1]][to_replant] <- 1.0 # Replanted tree is 1 year old
      tree_shell[[t+1]][to_replant] <- 1.0 # Replanted tree has initial yields
      disease_shell[,,t+1][to_replant] <- 0.0 # Replanted tree is healthy
      cost_shell[[t+1]] <- cost_shell[[t+1]] + sum(to_replant)*replant_cost_tree # Number of trees replanted times their cost
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
  
  tree_health <- pmap_df(list(tree_shell, c(1:TH), cost_shell),
                         function(tree, cntr, yearly_cost){
                           # subtracting one from the yield so that the initial yield is truly 0
                           # (non-zero yield is initially necessary for the growth function to work)
                           bind_cols(expand_grid(x=c(1:o_cols),y=c(1:o_rows)),yield=pmax(as.vector(tree)-1, integer(length(as.vector(tree))))) %>%
                             add_column(
                               time=start_year + cntr - 1,
                               realized_costs=yearly_cost/numel(orc_mat),
                             )
                         }) %>%
    mutate(net_returns=output_price*yield - realized_costs)
  
  return(tree_health)
}

## Function to run different disease control scenarios
simulateControlScenarios <- function(year_start,
                                     year_end,
                                     t_disease_year,
                                     t_treatment_year,
                                     disease_spread_rate,
                                     disease_growth_rate,
                                     replanting_strategy,
                                     replant_cost_tree,
                                     replant_cost_orchard,
                                     replant_years,
                                     replant_tree_block_size,
                                     max_yield,
                                     output_price,
                                     annual_cost,
                                     inf_intro,
                                     control1,
                                     t1_cost,
                                     control2,
                                     t2_cost){
  # set all of the same settings that are shared between simulations 
  time_horizon = year_end - year_start
  
  tree_sim_with_shared_settings <- partial(tree_sim, 
                                           TH=time_horizon,
                                           start_year=year_start,
                                           t_disease_year=t_disease_year,
                                           t_treatment_year=t_treatment_year,
                                           replant_trees=(replanting_strategy=='tree_replant'),
                                           replant_orchard=(replanting_strategy=='orchard_replant'),
                                           replant_years=replant_years,
                                           replant_cost_tree=replant_cost_tree,
                                           replant_cost_orchard=replant_cost_orchard,
                                           replant_tree_block_size=replant_tree_block_size,
                                           max_yield=max_yield,
                                           annual_cost=annual_cost,
                                           output_price=output_price,
                                           disease_spread_rate=disease_spread_rate,
                                           disease_growth_rate=disease_growth_rate)
  
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

