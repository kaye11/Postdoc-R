ggplot(data = world)+ geom_sf()+ coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=melted_NAdata.betas_comb %>% filter (entity=="Li") %>% filter (!(step=="infcomb")), 
             aes(x = lon, y = lat, color = log10(value)))+
  scale_color_viridis(breaks = c(seq(-6, 10, 2))) + theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (8, "line"), legend.key.width = unit (2, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank()) + facet_grid(watcon~step2)  #5 minutes

library(snowfall)
library(parallel)

# number of cores
cores <- detectCores()

# clustering
sfInit(parallel = TRUE, cpus = 60, type = "SOCK")
#sfLibrary(tidyverse)
sfLibrary(sf)
source("inspack_map_sf.R")

Sys.time()
ggplot()+ geom_sf(data=world, na.rm=TRUE, lwd=0)+ coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=melted_NAdata.betas_comb %>% filter (entity=="Cc"), aes(x = lon, y = lat, color = log10(value)))+
  scale_color_viridis(breaks = c(seq(-6, 10, 2)), discrete=FALSE) + expand_limits(colour = c(seq(-6, 10, 2))) +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (8, "line"), legend.key.width = unit (2, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank()) + facet_grid(watcon~step2) 
Sys.time()

Sys.time()
ggplot(data = world)+ geom_sf(na.rm=TRUE, lwd=0, fill=NA) + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=melted_NAdata.betas_comb %>% filter (entity=="Cc"), aes(x = lon, y = lat, color = log10(value)))+
  scale_color_viridis(breaks = c(seq(-6, 10, 2)), discrete=FALSE) + expand_limits(colour = c(seq(-6, 10, 2))) +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (8, "line"), legend.key.width = unit (2, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank()) + facet_grid(watcon~step2) 
Sys.time()

#detach packages
devtools::install_github("romainfrancois/nothing")
library(nothing)
loadedNamespaces()
