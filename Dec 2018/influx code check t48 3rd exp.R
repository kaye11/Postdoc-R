#import data and name it as influxdata
library(readxl)
influxdata <- read_excel("D:/Postdoc/Experiments/181127 Infection Third/181206 run si1-ti2 48.xls")

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

influxdata$time <- "48"

#change flowrate
flowrate <- 19.45
influxdata$count <- (influxdata$cellcount/(flowrate))*50*1000 #divide the flowrate by 2 if samplerun is 30 sec, do not divide by 2 if sample run is 60 sec
#reorder variables
influxdata <- influxdata%>%
  arrange(factor(cell, c("EhV", "Bacteria")))
influxdata$countpermldiv <- influxdata

#export data
setwd("D:/R program")
require(openxlsx)
write.xlsx(influxdata, file = "Postdoc-R/Exported Tables/ThirdExp_virbac_run2_samples.xlsx")

#####
library(readxl)
influxdata <- read_excel("Postdoc-R/Exported Tables/ThirdExp_virbac_run2.xlsx")


