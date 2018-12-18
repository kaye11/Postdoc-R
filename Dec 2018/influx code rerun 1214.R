library(readxl)
influxdata <- read_excel("D:/Postdoc/Experiments/181127 Infection Third/181214 rerun.xls")

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
  influxdata$samplenumber <7  ~ "48",
  influxdata$samplenumber >6 & influxdata$samplenumber <15 ~ "72",
  influxdata$samplenumber >14 ~ "96", 
  TRUE ~ as.character(influxdata$samplenumber)
)

#change flowrate
flowrate <- 20.8
influxdata$count <- (influxdata$cellcount/(flowrate))*50*1000 #divide the flowrate by 2 if samplerun is 30 sec, do not divide by 2 if sample run is 60 sec

#manual
treatments <- c ("still infected 1", "still infected 2", "still infected 3", 
                 "turbulent infected 1", "turbulent infected 2", "turbulent infected 3", "still control 1",
                 "still infected 1", "still infected 2", "still infected 3", 
                 "turbulent infected 1", "turbulent infected 2", "turbulent infected 3", "still control 3", 
                 "still infected 1", "still infected 2", "still infected 3", 
                 "turbulent infected 1", "turbulent infected 2", "turbulent infected 3")

#reorder variables
influxdata <- influxdata%>%
  arrange(factor(cell, c("EhV", "Bacteria")))

influxdata$treatment <- treatments

library(tidyr)
influxdata <- influxdata %>% separate(treatment, c("group1", "group2", "rep"))
influxdata$maingroup <- as.factor(paste(influxdata$group1, influxdata$group2, sep="-" ))

#divide cellcount measurements by 10^7
influxdata$countpermldiv <- influxdata$count/10^7

#export data
setwd("D:/R program")
require(openxlsx)
write.xlsx(influxdata, file = "Postdoc-R/Exported Tables/ThirdExp_virbac_rerun 1214.xlsx")

#####
