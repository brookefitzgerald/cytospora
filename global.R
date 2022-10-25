#Orchard simulation functions

grow_tree_function <- function(tree_ages,             #Matrix or vector of tree ages
                       max_yield,                     #Yield at maturity
                       tree_first_full_yield_year=5,  #Year tree reaches maturity
                       tree_last_full_yield_year=15,  #Year tree starts declining in yield
                       tree_end_year=20){             #Productive life of tree
  
  #Growth function - trees mature at `tree_first_full_yield_year`, 
  #                        start declining at `tree_last_full_yield_year`,
  #                        survive until `tree_end_year`
  growth_function <- approxfun(
    x=c(0, 2, tree_first_full_yield_year+1, tree_last_full_yield_year+1, tree_end_year+1),
    y=c(0, max_yield/(tree_first_full_yield_year+1), 0, -max_yield/(tree_end_year-tree_last_full_yield_year+3), 0),
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
                     replant_year=20,  # Year the orchard will be replanted
                     replant_cost_orchard=3000, # Cost to replant the entire orchard
                     replant_cost_tree=10, #Cost to replant an individual tree
                     start_disease_year=1, #Year infection starts in the orchard
                     inf_starts=2,   #Number of infectious starts
                     max_yield=15,   #Yield at maturity
                     disease_spread_rate=.10, #Rate of disease spread to adjacent trees in all directions (can be made more flexible)
                     disease_growth_rate=.2, #Rate of disease growth within a tree
                     control_effort=0, #Rate of control effort applied per year
                     control_cost=0, #Cost of the control applied
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
    grow_tree_function(tree_age_matrix, max_yield=max_yield)
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
  disease_shell[,,1:(start_disease_year - 1)] <- 0 
  disease_shell[,,start_disease_year] <- inf_mat
  
  #Tree Age
  age_shell <- vector("list",TH)
  age_shell[[1]] <- orc_mat
  n_trees_replanted = 0
  
  #Costs
  cost_shell <- vector("numeric",TH)
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
    tree_shell[[t+1]] <- pmax(zeros(o_rows, o_cols), tree_shell[[t+1]]) # set negative yields to zero
    
    #Propagate disease if disease spread has started
    if (t >= start_disease_year){
      disease_shell[,,t+1] <- disease_shell[,,t]%*%(tmat_x + tmat_identity) + t(t(disease_shell[,,t])%*%tmat_y)
      
      #Disease is detected with some prob, and action is taken to curtail disease spread
      #if()
      
      #Disease is mitigated if present
      disease_shell[,,t+1] <- disease_shell[,,t+1]*(1-control_effort)
      disease_shell[,,t+1] <- pmax(zeros(o_rows, o_cols), disease_shell[,,t+1]) # sets any negatives to zero
      cost_shell[[t+1]] <- cost_shell[[t+1]] + ifelse(control_effort==0, 0, control_cost)
      
      #Replace dead trees if part of mitigation strategy
      if(replant_trees==TRUE){
        to_replant <- tree_shell[[t+1]] <= 0.0 #Boolean indicating true if dead, false otherwise
        age_shell[[t+1]][to_replant] <- 1.0 # Replanted tree is 1 year old
        tree_shell[[t+1]][to_replant] <- 1.0 # Replanted tree has initial yields
        disease_shell[,,t+1][to_replant] <- 0.0 # Replanted tree is healthy
        cost_shell[[t+1]] <- cost_shell[[t+1]] + sum(to_replant)*replant_cost_tree # Number of trees replanted times their cost
      }
      
      # Replant orchard if part of mitigation strategy
      if((replant_orchard==TRUE) & (t==replant_year)){
        age_shell[[t+1]] <- 1 # Replanted tree is 1 year old
        tree_shell[[t+1]] <- 1.0 # Replanted tree has initial yields
        disease_shell[,,t+1] <- inf_mat # Replanted trees have initial disease starts.
        cost_shell[[t+1]] <- cost_shell[[t+1]] + replant_cost_orchard
        # control_effort <- 0 
        # TODO: Figure out if disease should spread again/if control effort should continue. 
      }
    }
  }
  
  tree_health <- pmap_df(list(tree_shell, c(1:TH), cost_shell),
                         function(tree, cntr, yearly_cost){
                           bind_cols(expand_grid(x=c(1:o_cols),y=c(1:o_rows)),yield=as.vector(tree)) %>%
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
                                     start_disease_year,
                                     disease_spread_rate,
                                     disease_growth_rate,
                                     replanting_strategy,
                                     replant_cost_tree,
                                     replant_cost_orchard,
                                     replant_year,
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
                                           start_disease_year=start_disease_year,
                                           replant_trees=(replanting_strategy=='tree_replant'),
                                           replant_orchard=(replanting_strategy=='orchard_replant'),
                                           replant_year=replant_year,
                                           replant_cost_tree=replant_cost_tree,
                                           replant_cost_orchard=replant_cost_orchard,
                                           max_yield=max_yield,
                                           annual_cost=annual_cost,
                                           output_price=output_price,
                                           disease_spread_rate=disease_spread_rate,
                                           disease_growth_rate=disease_growth_rate)
  
  tree_health_max <- tree_sim_with_shared_settings(inf_starts = 0) %>%   #inf_starts=0 implies no infection for max yield
    rename_with(~str_c("max_",.),-c(x,y,time))
  
  tree_health_nt <- tree_sim_with_shared_settings(inf_starts = inf_intro) %>%   #nt implies no treatment
    rename_with(~str_c("nt_",.),-c(x,y,time))
  
  #Simulate two control simulations
  #Treatment 1
  t1 <- tree_sim_with_shared_settings(control_effort = control1,
                                      control_cost = t1_cost) %>%
    rename_with(~str_c("t1_",.),-c(x,y,time))
  
  #Treatment 2
  t2 <- tree_sim_with_shared_settings(control_effort = control2,
                                      control_cost = t2_cost) %>%
    rename_with(~str_c("t2_",.),-c(x,y,time))
  
  inner_join(tree_health_nt,tree_health_max, by = c("x", "y", "time")) %>%
    inner_join(t1, by = c("x", "y", "time")) %>%
    inner_join(t2, by = c("x", "y", "time")) %>%
    rename(`Max Yield`=max_yield,`No Treatment`=nt_yield,`Treatment 1`=t1_yield,`Treatment 2`=t2_yield)
}
