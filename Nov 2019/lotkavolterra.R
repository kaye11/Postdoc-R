library(tidyverse)
library(deSolve)
library(FME)

# parameters

###BIOLOGICAL INFECTABILITY####
#1. adsorption
#viruses have same adsorption across the board
#entities differ. calculated adsorptivity are Nc=0.05, Cc=0.004, Li=0.36 (see evernote note on how this was calculated)

#2. infectivity
#fast viruses have higher infectivity (0.3), slow viruses have lower infectivity (0.06) from Jozef's paper

#3. host susceptability
#CJ's data 
#calcified cells are less susceptible, infection will go lower (prob=0.25)
#naked cells are more susceptible, infection will go higher (prob=1)

#delta is burst size, gamma is mortality, beta is summary of beta_all, burst size from Vardi paper

naked_calm_fast<- c(alpha = 1.04, beta = 4e-07, delta = 2500, gamma = 0.33, hostmort = 0.59, ads=0.05, hst=1, inf=0.3)
calcified_calm_fast<- c(alpha = 1.65, beta = 3e-06, delta = 25, gamma = 0.33, hostmort = 0.67, ads=0.13, hst=0.25, inf=0.3)
navice_calcified_0701<- c(alpha = 1.65, beta = 4.7e-06, delta = 25, gamma = 0.33, hostmort = 0.67, ads=0.13, hst=0.25, inf=0.3)
navice_naked_0701<- c(alpha = 1.04, beta = 3.9e-07, delta = 2500, gamma = 0.33, hostmort = 0.59, ads=0.05, hst=1, inf=0.3)
lith_calm <- c(alpha=)


pars=navice_calcified_0701 #do this one by one

##same but inside a whole function
lv_model <- function(pars, times = seq(0, 120, by = 0.1)) {
  # initial state 
  state <- c(x = 1e3, y = 1e4) #low density
  # derivative
  deriv <- function(t, state, pars) {
    with(as.list(c(state, pars)), {
      d_x <- (alpha * x) - ((beta*ads*hst*inf) * x * y) - (hostmort * x)
      d_y <- delta * (beta*ads*hst*inf) * x * y - gamma * y
      return(list(c(x = d_x, y = d_y)))
    })
  }
  # solve
  ode(y = state, times = times, func = deriv, parms = pars)
}

lv_results <- lv_model(pars = pars, times = seq(0, 15, by = 0.1))

lv_results %>% 
  data.frame() %>% 
  gather(var, pop, -time) %>% 
  mutate(var = if_else(var == "x", "Ehux", "EhV")) %>% 
  ggplot(aes(x = time, y = log10(pop))) +
  geom_line(aes(color = var)) +
  scale_color_brewer(NULL, palette = "Set1") +
  labs(title = "Lotka-Volterra Ehux naked_calm_fast EhV model",
       subtitle = paste(names(pars), pars, sep = " = ", collapse = "; "),
       x = "day", y = "log10 abundance")

