#Orchard simulation functions

tree_sim <- function(o_rows=24, #Block dimension row
                     o_cols=24, #Block dimension col
                     TH=40,     #Time Horizon
                     start_year=2022, #year simulation starts (only used to transform time at end)
                     replant_trees=FALSE, #Replant tree if the tree dies
                     replant_cost_tree=10, #Cost to replant an individual tree
                     start_disease_year=1, #Year infection starts in the orchard
                     mature_year=5,  #Year tree reaches maturity
                     tree_end=40,    #Productive life of tree
                     max_yield=10,   #Yield at maturity
                     inf_starts=2,   #Number of infectious starts
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
  
  #Growth function - trees mature in y years and survive for yT years
  growth_function <- approxfun(x=c(0,2,mature_year+1,tree_end),
                               y=c(0,max_yield/mature_year,0,0),method = "constant")
  
  #Applies the growth function to a matrix of trees with arbitrary ages (e.g. from replanting)
  grow_trees <- function(tree_age_matrix) {
    matrix(growth_function(tree_age_matrix), ncol=o_cols, byrow=FALSE)
  }
  # plot_time=c(1:40)
  # tibble(x=plot_time,
  #        growth=growth_function(plot_time),
  #        cum_growth=cumsum(growth_function(plot_time))) %>%
  #   add_case(x=0,growth=2,cum_growth=0) %>%
  #   pivot_longer(-x) %>%
  #   ggplot(aes(x,value,color=name)) +
  #   geom_line() +
  #   scale_x_continuous(breaks = plot_time) +
  #   scale_y_continuous(breaks = c(0:max_yield))
  
  
  #Seed infection 
  inf_mat <- zeros(o_rows, o_cols)
  if(inf_starts>0){
    inf_location<-t(replicate(inf_starts,c(sample(o_rows,1),sample(o_cols,1)))) 
    for(i in 1:dim(inf_location)[1]){
      inf_mat[inf_location[i,1],inf_location[i,2]] <- 1
    }
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

  #TreeAge
  age_shell <- vector("list",TH)
  age_shell[[1]] <- orc_mat
  n_trees_replanted = 0
  
  ####################################
  for(t in 1:(TH-1)){
    #Trees age each year
    age_shell[[t+1]] <- age_shell[[t]] + 1
    
    #Grow trees subject to damage
    tree_shell[[t+1]] <- tree_shell[[t]] + grow_trees(age_shell[[t]]) - disease_shell[,,t]
    
    #Propagate disease if disease spread has started
    if (t >= start_disease_year){
      disease_shell[,,t+1] <- disease_shell[,,t]%*%(tmat_x + tmat_identity) + t(t(disease_shell[,,t])%*%tmat_y)
      
      #Disease is detected with some prob, and action is taken to curtail disease spread
      #if()
      
      #Disease is mitigated if present
      if(control_effort>0){
        disease_shell[,,t+1] <- disease_shell[,,t+1]*(1-control_effort)
        disease_shell[,,t+1] <- pmax(zeros(o_rows, o_cols), disease_shell[,,t+1]) # sets any negatives to zero
      }
      
      #Replace dead trees if part of mitigation strategy
      if(replant_trees==TRUE){
        to_replant <- tree_shell[[t+1]] <= 0.0 #Boolean indicating true if dead, false otherwise
        age_shell[[t+1]][to_replant] <- 1.0 # Replanted tree is 1 year old
        tree_shell[[t+1]][to_replant] <- 1.0 # Replanted tree has initial yields
        disease_shell[,,t+1][to_replant] <- 0.0 # Replanted tree is healthy
        n_trees_replanted <- n_trees_replanted + sum(to_replant)
      }
    }
  }
  
  realized_costs <- annual_cost
  if(control_effort>0){
    realized_costs <- realized_costs + control_cost
  }
  if (n_trees_replanted>0){
    # TODO: figure out why the replanting cost has such a huge impact 
    realized_costs <- realized_costs + (n_trees_replanted*replant_cost_tree)
  }
  
  tree_health <- map2_dfr(tree_shell,c(1:TH),
                          function(tree,cntr){
                            bind_cols(expand_grid(x=c(1:o_cols),y=c(1:o_rows)),value=as.vector(tree)) %>%
                              add_column(time=start_year + cntr - 1)
                          }) %>%
    mutate(net_returns=output_price*value - (realized_costs/numel(orc_mat)))
  
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
                                           replant_trees=(replanting_strategy=='yr1_replant'),
                                           replant_cost_tree=replant_cost_tree,
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
    rename(`Max Yield`=max_value,`No Treatment`=nt_value,`Treatment 1`=t1_value,`Treatment 2`=t2_value)
}


# tree_health_input=tree_yield
# tree_econ <- function(
#     tree_health_input,
#     output_price=30,
#     annual_cost=10,
#     treatment_1_cost=5,
#     treatment_2_cost=2){
#   
#   #Expected net returns for a single grow (calculate daily for orchard, then sum at end)
#   tree_health_input %>%
#     mutate(across(-c(x,y,time),~output_price*. - annual_cost - ))
#   
#   
# }