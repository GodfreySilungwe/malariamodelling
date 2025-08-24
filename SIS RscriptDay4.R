library(tidyverse)
library(deSolve)
###Define model eqns rstudio staged
vector_human <- function(t,x,parms) {
  
  with(as.list(c(parms, x)), {
    
###Total population
    
    M = Sm+Im
    H = S + I
### Vector eqn
    
    
    dSm = mu_u*M - beta*I/H*Sm - mu_u*Sm
    dIm = beta*I/H*Sm -mu_u*Im
    
    
### Human eqns
    
    
    dS=-alpha*Im/H*S + gamma*I
    dI=alpha*Im/H*S - gamma*I
    
    output <- c(dSm, dIm, dS, dI)
    list(output)
    
  }
)
}


###Initial Pop compart
start <- c(
  Sm=40000,
  Im=30000,
  S=5000,
  I=1000
)

###Define parameters in this model

parms <- c(
  mu_u = 1/50,
  alpha=0.12,
  beta=0.25,
  gamma = 1/20
  )


###Time interval

times <- seq(0, 100, 0.01)

###Solve Eqns

vector_model <- ode(
  
 times = times,
 parms = parms,
 func= vector_human,
 y = start
 
 )

###Make data plotable

vmplot <- as_tibble(as.data.frame(vector_model)) %>%
  pivot_longer(names_to = "variable", cols = !1)

vmplot %>%
  group_by(variable) %>%
  ggplot() +
  geom_line(aes(x = time, y = value, colour = variable)) +
  theme_minimal() +
  labs(title = "Vector-Human Compartments", 
       y = "population", colour = "Species") +
  facet_wrap(~variable)
source("mysim.R")






