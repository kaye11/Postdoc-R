
library(readxl)
influxdata <- read_excel("D:/Postdoc/Experiments/190318 Infection Seventh/26-Mar-2019 seventh edited reruns.xlsx")

require(ggplot2)
require(Rmisc)
require (plotly)
source("theme_Publication.R")
require(reshape2)
source("resizewin.R")
require(dplyr)
resize.win (12,9)
#run source code
source("influxsource.R")
require(tidyr)

influxdata$time <- case_when(
  influxdata$samplenumber <13 ~ "0",
  influxdata$samplenumber >12 & influxdata$samplenumber <25 ~ "24",
  influxdata$samplenumber >24 & influxdata$samplenumber <37 ~ "48",
  influxdata$samplenumber >36 & influxdata$samplenumber <49 ~ "72", 
  influxdata$samplenumber >48 ~ "96",
  TRUE ~ as.character(influxdata$samplenumber)
)

#change flowrate
flowrate <- (16.2+17.4+18+18)/4
influxdata$count <- (influxdata$cellcount/(flowrate))*50*1000 #divide the flowrate by 2 if samplerun is 30 sec, do not divide by 2 if sample run is 60 sec

treatments <- c ("still control 1", "still control 2", "still control 3", 
                 "turbulent control 1", "turbulent control 2", "turbulent control 3", 
                 "still infected 1", "still infected 2", "still infected 3", 
                 "turbulent infected 1", "turbulent infected 2", "turbulent infected 3")

#reorder variables
influxdata <- influxdata%>%
  arrange(factor(cell, c("EhV", "Bacteria")))

influxdata$treatment <- rep_len (treatments, length.out = 120) #change according to sample number

library(tidyr)
influxdata <- influxdata %>% separate(treatment, c("group1", "group2", "rep"))
influxdata$maingroup <- as.factor(paste(influxdata$group1, influxdata$group2, sep="-" ))

#divide cellcount measurements by 10^7
influxdata$countpermldiv <- influxdata$count/10^8

##code for plots next
sum.all <- summarySE(influxdata, measurevar = "countpermldiv", 
                     groupvars = c("maingroup", "group1", "group2", "time", "cell"))

influxdata$time <- as.numeric (influxdata$time)

ggplotly(ggplot(data=influxdata, aes(x=time, y=countpermldiv, colour=group2)) +geom_boxplot() + 
           facet_grid(cell~group1, scales="free")+ geom_point()+ theme_bw())

sum.all$time <- as.numeric(sum.all$time)

ggplot(data=sum.all, aes(x=time, y=countpermldiv, colour=group2)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=countpermldiv-se, ymax=countpermldiv+se, width=0.5)) + 
  facet_grid(cell~group1, scales="free")+ theme_bw()

#combined
bacteria.both <- ggplot(data=influxdata %>% 
                          filter(cell %in% c("Bacteria")), 
                        aes(x=time, y=countpermldiv, colour=maingroup, shape=maingroup, linetype=maingroup)) +
  geom_point(size=7, aes(colour=maingroup, shape=maingroup)) + 
  geom_smooth(method = 'loess', aes(colour=maingroup, fill=maingroup), alpha=0.2, size=1) + 
  labs(y= expression("bacteria per mL"~ scriptstyle(x)~"10"^~8), x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +
  #facet_grid(~group1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none")


virus.both <-ggplot(data=influxdata %>% 
                      filter(cell %in% c("EhV")), 
                    aes(x=time, y=countpermldiv, colour=maingroup, shape=maingroup, linetype=maingroup)) +
  geom_point(size=7, aes(colour=maingroup, shape=maingroup)) + 
  geom_smooth(method = 'loess', aes(colour=maingroup, fill=maingroup), alpha=0.2, size=1) + 
  labs(y= expression("EhV per mL"~ scriptstyle(x)~"10"^~8), x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +
  #facet_grid(~group1) +
  theme(strip.text = element_blank(), legend.title=element_blank())


resize.win(10,15)
grid.newpage()
grid.draw(rbind(ggplotGrob(bacteria.both), ggplotGrob(virus.both),  size = "last"))

#notcombined
bacteria <- ggplot(data=sum.all %>% 
                     filter(cell %in% c("Bacteria")), aes(x=time, y=countpermldiv, colour=group2)) +
  geom_point(size=5) + 
  geom_errorbar(aes(ymin=countpermldiv-se, ymax=countpermldiv+se, width=2)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2), alpha=0.2) + 
  labs(y= expression("bacteria per mL"~ scriptstyle(x)~"10"^~8), x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_color_manual (values = c(control="lightcoral", viralparticles="seagreen3", infected="steelblue2")) +
  theme_Publication() + 
  facet_grid(~group1) +
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), legend.position="none", 
        axis.title.y = element_text(angle=90,vjust = 5))

virus <- ggplot(data=sum.all %>% 
                  filter(cell %in% c("EhV")), aes(x=time, y=countpermldiv, colour=group2)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=countpermldiv-se, ymax=countpermldiv+se, width=2)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2), alpha=0.2) + 
  labs(y= expression("EhV per mL"~ scriptstyle(x)~"10"^~8), x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_color_manual (values = c(control="lightcoral", viralparticles="seagreen3", infected="steelblue2"))+
  theme_Publication() + 
  facet_grid(~group1) +
  theme(legend.title=element_blank(), strip.text=element_blank())

resize.win(12,15)
grid.newpage()
grid.draw(rbind(ggplotGrob(bacteria), ggplotGrob(virus),  size = "last"))

#export data
setwd("D:/R program")
require(openxlsx)
write.xlsx(sum.all, file = "Postdoc-R/Exported Tables/SeventhExp_summary_virbac.xlsx")
write.xlsx(influxdata, file = "Postdoc-R/Exported Tables/SeventhExp_virbac.xlsx")
