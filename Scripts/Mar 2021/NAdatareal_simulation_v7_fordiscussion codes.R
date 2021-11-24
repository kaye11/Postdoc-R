##density_merged (for discussion)
density_merged$xexp <- (10)^(density_merged$x)
dens.freq <- density_merged %>% group_by (depthf, entity) %>% filter (y>0.039)  %>% 
  mutate (xexp=(10)^x, percent=(10^(x)*100))

density_merged %>% group_by (depthf, entity ) %>% filter (y>0.04) %>%  summarise (freq=sum(y*100))
