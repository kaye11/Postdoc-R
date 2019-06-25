
library(readxl)
influxdata <- read_excel("D:/Postdoc/Experiments/190211 Infection Sixth/21-Feb-2019 influx_edited out reruns.xls")
View(influxdata)

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
  influxdata$samplenumber <13  ~ "96",
  influxdata$samplenumber >12 & influxdata$samplenumber <25 ~ "24",
  influxdata$samplenumber >24 & influxdata$samplenumber <37 ~ "48",
  influxdata$samplenumber >36 ~ "72", 
  TRUE ~ as.character(influxdata$samplenumber)
)

#change flowrate
flowrate <- (16.8+17.4)/2
influxdata$count <- (influxdata$cellcount/(flowrate))*50*1000 #divide the flowrate by 2 if samplerun is 30 sec, do not divide by 2 if sample run is 60 sec

#rename influxdata as influxdata1
influxdata1 <- influxdata

influxdata <- read_excel("D:/Postdoc/Experiments/190211 Infection Sixth/22-Feb-2019 influx.xls")

#run source code
source("influxsource.R")

influxdata$time <- case_when(
  influxdata$samplenumber <13  ~ "96",
  influxdata$samplenumber >12 ~ "120", 
  TRUE ~ as.character(influxdata$samplenumber)
)

#change flowrate
flowrate <- (13.4+14.2)/2
influxdata$count <- (influxdata$cellcount/(flowrate))*50*1000 #divide the flowrate by 2 if samplerun is 30 sec, do not divide by 2 if sample run is 60 sec

#merge the two documents
influxdata <- rbind(influxdata1, influxdata)

treatments <- c ("still control 1", "still control 2", "still control 3", 
                 "turbulent control 1", "turbulent control 2", "turbulent control 3", 
                 "still infected 1", "still infected 2", "still infected 3", 
                 "turbulent infected 1", "turbulent infected 2", "turbulent infected 3")

#reorder variables
influxdata <- influxdata%>%
  arrange(factor(cell, c("EhV", "Bacteria")))

influxdata$treatment <- rep_len (treatments, length.out = 144) #change according to sample number

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
bacteria.both <- ggplot(data=sum.all %>% 
                          filter(cell %in% c("Bacteria")), 
                        aes(x=time, y=countpermldiv, colour=maingroup, shape=maingroup, linetype=maingroup)) +
  geom_point(size=5) + 
  geom_errorbar(aes(ymin=countpermldiv-se, ymax=countpermldiv+se, width=5)) + 
  geom_smooth(method="loess") + 
  labs(y= expression("bacteria per mL"~ scriptstyle(x)~"10"^~8), x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_color_manual(values = rep(c("#e41a1c", "#e41a1c", "#377eb8", "#377eb8", "#4daf4a", "#4daf4a"), times = 2)) +
  scale_linetype_manual(values = rep(c("solid", "longdash"), times = 6)) + 
  scale_shape_manual(values = rep(16:17, 3)) + 
  theme_Publication() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none")

virus.both <-ggplot(data=sum.all %>% 
                      filter(cell %in% c("EhV")), 
                    aes(x=time, y=countpermldiv, colour=maingroup, shape=maingroup, linetype=maingroup)) +
  geom_point(size=5) + 
  geom_errorbar(aes(ymin=countpermldiv-se, ymax=countpermldiv+se, width=5)) + 
  geom_smooth(method="loess") + 
  labs(y= expression("EhV per mL"~ scriptstyle(x)~"10"^~8), x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_color_manual(values = rep(c("#e41a1c", "#e41a1c", "#377eb8", "#377eb8", "#4daf4a", "#4daf4a"), times = 2)) +
  scale_linetype_manual(values = rep(c("solid", "longdash"), times = 6)) + 
  scale_shape_manual(values = rep(16:17, 3)) + 
  theme_Publication() +
  theme(legend.key.width=unit(3,"line"), legend.title = element_blank())

resize.win(12,15)
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
write.xlsx(sum.all, file = "Postdoc-R/Exported Tables/SixthExp_summary_virbac.xlsx")
write.xlsx(influxdata, file = "Postdoc-R/Exported Tables/SixthExp_virbac.xlsx")
