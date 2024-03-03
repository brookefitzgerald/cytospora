require(data.table, include.only = "CJ")
library(jsonlite)
library(nloptr)
library(pracma)
library(scales)
library(tidyverse)


dfm <-data.frame(l1=  c(4,  0,  4,  1,  0, 0), 
                 l2=  c(0,  4,  4,  0,  1, 2),
                 prob=c(90,70, 99, 50, 10, 15))
#View(dfm)

minss <- function(r, dfm=dfm) {
  sum(apply(dfm, 1, function(x){((1-exp(-(r[1]*x[1] + r[2]*x[2]))+0.01)*100 - x[3])^2}))
}
results <- nloptr(c(0.9, 0.07),
       minss,
       dfm=dfm,
       lb=c(0.2, 0.01),
       ub=c(1, 1),
       opts=list("algorithm"="NLOPT_LN_COBYLA", "xtol_abs" = 1.0e-10, "maxeval"=20000)
       )
results$solution
sol <- results$solution
constants <- read_json("project_constants.json", simplifyVector = T)
constants$l1_neighbors_mult_parameter <- sol[[1]]
constants$l2_neighbors_mult_parameter <- sol[[2]]

write_json(constants, "project_constants.json", pretty=T)


# Creating graphs for user guide
CJ(n1=0:4, n2=0:8) %>% mutate (p=1 - exp(-(sol[[1]]*n1 + sol[[2]]*n2))+ 0.01, color_col=if_else(p>0.6, "black", "white")) %>%
  ggplot(aes(x=n2, y=n1, fill=p)) +
  geom_tile(color="black")+theme_minimal(base_size=18) +
  geom_text(aes(label=label_percent()(round(p, 2)), , color=color_col), size=8) +
  scale_color_manual(values=c("black"="black", "white"="white"), guide="none")+
  scale_x_continuous("Infected Second Degree Neighbors") +scale_y_continuous("Infected First Degree Neighbors") +
  scale_fill_continuous("Probability \nof Infection", labels=label_percent()) +
  labs(title="Probability of Infection if Neighbors are Infected")

data.frame(n_non_neighbors=0:(24^2 - 12)) %>% mutate(probability_infection_med=(1-exp(-(results$solution[3]*n_non_neighbors))),
                                                     probability_infection_low=(1-exp(-(results2$solution[3]*n_non_neighbors))),
                                                     probability_infection_high=(1-exp(-(results3$solution[3]*n_non_neighbors)))) %>% 
  pivot_longer(cols=starts_with("probability_infection"),
               names_to = "spread_level",
               names_pattern="probability_infection_(.*)",
               values_to = "probability_infection") %>% 
  mutate(spread_level=factor(spread_level, levels=c("low", "med", "high"))) %>% 
  ggplot(aes(x=n_non_neighbors, y=probability_infection, color=spread_level))+
    geom_line(size=2) +
  theme_minimal(base_size = 16) +
  scale_y_continuous(labels=label_percent()) +
  labs(x="Number of Infected Non-Neighboring Trees", y="Probability of Infection")


g <- ones(7, 7)
g[4, 4] <- 0
g[4, c(3,5)] <- 0.25
g[c(3,5), 4] <- 0.25
g[4, c(2, 6)] <- 0.5
g[c(2, 6), 4] <- 0.5
g[c(3,5), c(3,5)] <- 0.5
bind_cols(expand.grid(x=1:7, y=1:7), yield=unlist(lapply(1:7, function(i){g[,i]}))) %>%
  ggplot(aes(y=x, x=y, fill=factor(yield))) +
  scale_y_reverse()+
  scale_fill_manual("Neighboring Trees", 
                    values=c("0"="green", "0.25"="slateblue4", "0.5"="lightslateblue", "1"="lightgrey"),
                    labels = c("Tree", "Degree 1 neighbors", "Degree 2 neighbors", "Not neighbors"))+
  geom_tile(color="black")+theme_void(base_size=24)   + theme(aspect.ratio=1)

