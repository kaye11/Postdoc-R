library(readxl)
ehv201 <- read_excel("D:/Postdoc/Experiments/190724 EhV 201/summary.xlsx")
View(ehv201)

require(ggplot2)
require(Rmisc)
require(data.table)
require (plotly)
source("theme_Publication.R")
require(reshape2)
source("resizewin.R")
require(dplyr)
resize.win (12,9)
require(viridis)

ehv201$strain <- as.factor (ehv201$strain)
ehv201$time <- ehv201$day*24
ehv201$dead <- ehv201$countperml*ehv201$sytox
ehv201$sytox <- ehv201$sytox*100

#divide cellcount measurements by 10^6
ehv201$countpermldiv <- ehv201$countperml/10^6

#divide dead cells by 10^4
ehv201$deadpermldiv <- ehv201$dead/10^4

ehv201.long <- melt (data=ehv201, id.vars=c("strain", "time", "day", "rep"), variable.name="parameter")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7", "#0072B2",  "#F0E442")

resize.win(6,6)
ggplot(data=ehv201.long %>% 
         filter(parameter %in% c("sytox")),
       aes(x=day, y=value, colour=strain)) +
  geom_point(size=5) + 
  geom_smooth(method = 'loess', aes(colour=strain, fill=strain)) + 
  scale_x_continuous(breaks=c(seq(0,22,4))) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual (values=cbPalette)  +
  theme_Publication() + 
  labs(y="% sytox stained", x= "days post-infection") 

ggplot(data=ehv201.long %>% 
         filter(parameter %in% c("countperml")),
       aes(x=time/24, y=log10(value), colour=strain)) +
  geom_point(size=5) + 
  geom_smooth(method = 'loess', aes(colour=strain, fill=strain)) + 
  scale_x_continuous(breaks=c(seq(0,22,4))) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual (values=cbPalette)  +
  theme_Publication() +   labs(y= "log10 E.huxleyi" , x= "hours post-infection") +
  theme_Publication()

#export data
setwd("D:/R program")
require(openxlsx)
write.xlsx(ehv201.long, file = "Postdoc-R/Exported Tables/0724 ehv201.xlsx")