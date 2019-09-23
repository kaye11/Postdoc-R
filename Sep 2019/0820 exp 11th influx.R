
library(readxl)
influxdata <- read_excel("D:/Postdoc/Experiments/190820 EhV203 Eleventh/20190904 11th exp.xls")
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
source("influxsource2.R")
require(tidyr)

influxdata$day <- case_when(
  influxdata$samplenumber ==1  ~ "t72 before",
  influxdata$samplenumber >1 & influxdata$samplenumber <18 ~ "0",
  influxdata$samplenumber ==18  ~ "t72 before",
  influxdata$samplenumber >18 & influxdata$samplenumber <35 ~ "1",
  influxdata$samplenumber ==35  ~ "t72 before",
  influxdata$samplenumber >35 ~ "2", 
  TRUE ~ as.character(influxdata$samplenumber)
)

##change flowrate
id1 <- influxdata %>% filter(samplenumber<18 ) %>% mutate(flowrate = (43.2+41.2)/2)
id2 <- influxdata %>% filter(samplenumber>17 ) %>% mutate(flowrate = (28.4+29.2+29.8)/3)

##call other data
influxdata <- read_excel("D:/Postdoc/Experiments/190820 EhV203 Eleventh/20190905 11th.xls")

source("influxsource2.R")

influxdata$day <- case_when(
  influxdata$samplenumber <17  ~ "3",
  influxdata$samplenumber >16 & influxdata$samplenumber <33 ~ "4",
  influxdata$samplenumber >32 & influxdata$samplenumber <49 ~ "5",
  influxdata$samplenumber >48 & influxdata$samplenumber <65 ~ "6",
  influxdata$samplenumber >64 & influxdata$samplenumber <81 ~ "7",
  influxdata$samplenumber >80 & influxdata$samplenumber <84 ~ "t72 190823",
  influxdata$samplenumber >83 & influxdata$samplenumber <86 ~ "sh",
  influxdata$samplenumber >85 & influxdata$samplenumber <88 ~ "nsh",
  influxdata$samplenumber >87 ~ "frozen inf", 
  TRUE ~ as.character(influxdata$samplenumber)
)

#change flowrate
influxdata$flowrate <- (30.2+30.4+30.4+30.2+30.6+30.8+31+30.4)/8

#rbind
influxdata <- rbind(id1, id2, influxdata)
influxdata$count <- (influxdata$cellcount/(influxdata$flowrate))*50*1000 #divide the flowrate by 2 if samplerun is 30 sec, do not divide by 2 if sample run is 60 sec

#filterdata
otherdata <- influxdata %>% 
  filter(day %in% c("frozen inf", "nsh", "sh", "t72 190823", "t72 before"))

#delete data
influxdata <- influxdata %>% 
  filter(!(day %in% c("frozen inf", "nsh", "sh", "t72 190823", "t72 before")))

treatments <- c ("still control 1 0", "still control 2 0", "still control 3 0", "still control 4 0",
                 "turbulent control 1 350", "turbulent control 2 350", "turbulent control 3 600", 
                 "turbulent control 4 600","still infected 1 0", "still infected 2 0", "still infected 3 0", 
                 "still infected 4 0", "turbulent infected 1 350", "turbulent infected 2 350", 
                 "turbulent infected 3 600", "turbulent infected 4 600")

#reorder variables
influxdata <- influxdata%>%
  arrange(factor(cell, c("EhV", "Bacteria")))

influxdata$treatment <- rep_len (treatments, length.out = 256) #change according to sample number

library(tidyr)
influxdata <- influxdata %>% separate(treatment, c("group1", "group2", "rep", "rpm"))
influxdata$maingroup <- as.factor(paste(influxdata$group1, influxdata$group2, influxdata$rpm, sep="-" ))

#divide count measurements by 10^7
influxdata$countpermldiv <- influxdata$count/10^7

##code for plots next
sum.all <- summarySE(influxdata, measurevar = "countpermldiv", 
                     groupvars = c("maingroup", "group1", "group2", "day", "cell"))

sum.all.others <- summarySE(otherdata, measurevar = "count", 
                     groupvars = c("day", "cell"))

influxdata$day <- as.numeric (influxdata$day)

ggplotly(ggplot(data=influxdata, aes(x=day, y=countpermldiv, colour=maingroup)) +geom_boxplot() + 
           facet_grid(cell~maingroup, scales="free")+ geom_point()+ theme_bw())

sum.all$day <- as.numeric(sum.all$day)

ggplot(data=sum.all, aes(x=day, y=countpermldiv, colour=maingroup)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=countpermldiv-se, ymax=countpermldiv+se, width=0.5)) + 
  facet_grid(cell~maingroup, scales="free")+ theme_bw()

##what I wanted
resize.win(8,8)
ggplot(data=influxdata %>% 
         filter(cell=="EhV" & group2=="infected"), aes(x=day, y=log10(count), linetype=maingroup)) +
  geom_point(size=7, aes(colour=maingroup, shape=maingroup)) + 
  geom_smooth(method = 'loess', aes(colour=maingroup, fill=maingroup), alpha=0.2, size=1.5) + 
  labs (y= expression("log10 (EhV"~ "mL"^~-1~")"), x="days post-infection") +
  scale_shape_manual (values= c(15, 16, 17)) +
  scale_color_manual(values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_fill_manual (values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_linetype_manual(values = c("longdash", "longdash", "longdash")) +
  theme_Publication() +   theme(legend.position ="bottom", legend.title = element_blank())

