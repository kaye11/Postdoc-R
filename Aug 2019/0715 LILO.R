library(readxl)
lilo <- read_excel("D:/Postdoc/Experiments/190715 LILO/summary.xlsx")
View(lilo)

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

lilo$strain <- as.factor (lilo$strain)
lilo$treatment <- as.factor (lilo$treatment)
lilo$time <- lilo$day*24
lilo$dead <- lilo$countperml*lilo$sytox
lilo$sytox <- lilo$sytox*100

#divide cellcount measurements by 10^6
lilo$countpermldiv <- lilo$countperml/10^6

#divide dead cells by 10^4
lilo$deadpermldiv <- lilo$dead/10^4

lilo.long <- melt (data=lilo, id.vars=c("strain", "treatment", "time", "day", "rep", "hourfor", "hour"), variable.name="parameter")

lilo.long$dayhr <- as.factor(paste (lilo.long$day, lilo.long$hour, sep="-"))

cbPalette <- c("#0072B2", "#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7", "#F0E442")

resize.win (6,6)
#LILO
ggplot(data=lilo.long %>% 
         filter(parameter %in% c("sytox")) %>% 
         filter(day %in% c("2")),
       aes(x=hourfor, y=value, colour=treatment)) +
  geom_point(size=3) + 
  geom_smooth(method = 'loess', aes(colour=treatment, fill=treatment)) + 
  scale_color_manual(values=cbPalette) +
  scale_fill_manual (values=cbPalette)  +
  theme_Publication() + 
  labs(y="% sytox stained", x= "48 hrs post-infection") 

ggplot(data=lilo.long %>% 
         filter(parameter %in% c("countperml")) %>% 
         filter(day %in% c("2")),
       aes(x=hourfor, y=log10(value), colour=treatment)) +
  geom_point(size=3) + 
  geom_smooth(method = 'loess', aes(colour=treatment, fill=treatment)) + 
  scale_color_manual(values=cbPalette) +
  scale_fill_manual (values=cbPalette)  +
  theme_Publication() + 
  labs(y= "log10 E.huxleyi", x= "48 hrs post-infection") 

#MOIs
ggplot(data=lilo.long %>% 
         filter(parameter %in% c("sytox")) %>% 
         filter(hour %in% c("2pm")),
       aes(x=day, y=value, colour=treatment)) +
  geom_point(size=3) + 
  geom_smooth(method = 'loess', aes(colour=treatment, fill=treatment)) + 
  scale_color_manual(values=cbPalette) +
  scale_fill_manual (values=cbPalette)  +
  theme_Publication() + 
  labs(y="% sytox stained", x= "days post-infection") 

ggplot(data=lilo.long %>% 
         filter(parameter %in% c("countperml")) %>% 
         filter(hour %in% c("2pm")),
       aes(x=day, y=log10(value), colour=treatment)) +
  geom_point(size=3) + 
  geom_smooth(method = 'loess', aes(colour=treatment, fill=treatment)) + 
  scale_color_manual(values=cbPalette) +
  scale_fill_manual (values=cbPalette)  +
  theme_Publication() + 
  labs(y="log10 E. huxleyi", x= "days post-infection") 
