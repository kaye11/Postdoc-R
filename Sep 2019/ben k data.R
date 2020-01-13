library(readr)
coup <- read_csv("D:/Postdoc/Ben K/Master OneStep Complete noRedundant noContaminationSW.csv")
View(coup)

library (dplyr)
amsoup <- coup %>% filter (Expt_Name == "Francis Bacon")

library(ggplot2)

ggplot(data=coup %>% 
         filter(Expt_Name %in% c("Junk Food", "Mental Rent")), 
       aes(x=Day_Number, y=log10(`Ehux mL` +1 ), linetype=Media)) +
  geom_point(size=5, aes(colour=Media, shape=Media)) + facet_grid(Treatment~Density) +
  geom_smooth(method = 'loess', aes(colour=Media, fill=Media), alpha=0.2, size=1.5) + 
  labs (y="log10 E.huxleyi", x="days post-infection")

