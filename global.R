#Orchard simulation functions

tree_sim <- function(o_rows=24, #Block dimension row
                     o_cols=24, #Block dimension col
                     TH=40,     #Time Horizon
                     start_year=2022, #year simulation starts (only used to transform time at end)
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
  orc_mat <- ones(o_rows,o_cols)
  
  #Growth function - trees mature in y years and survive for yT years
  growth_function <- approxfun(x=c(0,2,mature_year+1,tree_end),
                               y=c(0,max_yield/mature_year,0,0),method = "constant")
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
  inf_mat <- zeros(o_rows,o_cols)
  if(inf_starts>0){
    inf_location=t(replicate(inf_starts,c(sample(o_rows,1),sample(o_cols,1)))) 
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
  disease_shell <- vector("list",TH)
  disease_shell[[1]] <- inf_mat
  
  
  ####################################
  for(t in 1:(TH-1)){
    #Grow trees subject to damage
    tree_shell[[t+1]] <- tree_shell[[t]] + growth_function(t) - disease_shell[[t]]
    
    #Propogate disease
    disease_shell[[t+1]] <- disease_shell[[t]]%*%(tmat_x + tmat_identity) + t(t(disease_shell[[t]])%*%tmat_y)
    
    #Disease is detected with some prob, and action is taken to curtail disease spread
    #if()
    
    #Disease is mitigated if present
    if(control_effort>0){
      for(i in 1:ncol(disease_shell[[t+1]])){
        for(j in 1:nrow(disease_shell[[t+1]])){
          if(disease_shell[[t+1]][j,i]>0){
            disease_shell[[t+1]][j,i] <- disease_shell[[t+1]][j,i]*(1-control_effort)
            if(disease_shell[[t+1]][j,i]<0) disease_shell[[t+1]][j,i] <- 0
          }
        }
      }
    }
    
  }
  
  if(control_effort>0){
    realized_costs = annual_cost + control_cost
  } else {
    realized_costs = annual_cost
  }
  
  tree_health <- map2_dfr(tree_shell,c(1:TH),
                          function(tree,cntr){
                            bind_cols(expand_grid(x=c(1:o_cols),y=c(1:o_rows)),value=as.vector(tree)) %>%
                              add_column(time=start_year + cntr - 1)
                          }) %>%
    mutate(net_returns=output_price*value - (realized_costs/numel(orc_mat)))
  
  return(tree_health)
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