library(readr)
navice <- read_delim("D:/Postdoc/theoretical/NAVICE Ehux Sytox VLP wrangled.csv", 
                     "\t", escape_double = FALSE, trim_ws = TRUE)
View(navice)

require(dplyr)
navice <- navice %>%
  mutate(ID = case_when(`Infection Designation` == "Early Infection" ~ "EI",
                        `Infection Designation` == "Late Infection" ~ "LI",
                        `Infection Designation` == "Post Bloom" ~ "PI"))
require(ggplot2)
require(viridis)
source("theme_Publication.R")
source("resizewin.R")
resize.win (12,9)

navice$Ehuxperml <- navice$`Av Ehux ml`
navice$EhVperml <- navice$`EhV copies mL` 
navice$virhostrat <- navice$EhVperml/navice$Ehuxperml

ggplot(data=navice, aes(x=as.factor(`Cast #`), y=`Depth (m)`)) +
  geom_point(aes(color=Ehuxperml, size=EhVperml, shape=ID)) +
  scale_colour_viridis(option="magma") +
  scale_x_discrete(position = "top") +
  scale_y_reverse(lim=c(50,0)) +
  scale_size_continuous(range = c(4,10)) +
  theme (axis.text = element_text(size=15), axis.title = element_text(size=15), legend.text=element_text(size=15)) 

ggplot(data=navice, aes(x=ID, y=`Depth (m)`)) +
  geom_point(aes(color=Ehuxperml, size=EhVperml)) +
  scale_colour_viridis(option="magma") +
  scale_x_discrete(position = "top") +
  scale_y_reverse(lim=c(50,0)) + facet_grid(~`Cast #`)

ggplot(data=navice, aes(x=as.factor(`Cast #`), y=`Depth (m)`)) +
  geom_point(aes(color=virhostrat, size=`Av Sytox %`, shape=ID)) +
  scale_colour_viridis(option="magma") +
  scale_x_discrete(position = "top") +
  scale_y_reverse(lim=c(50,0)) +
  scale_size_continuous(range = c(4,10)) +
  theme (axis.text = element_text(size=15), axis.title = element_text(size=15), legend.text=element_text(size=15)) 
