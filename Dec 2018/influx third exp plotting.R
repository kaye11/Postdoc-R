
require(ggplot2)
require(Rmisc)
require (plotly)
source("theme_Publication.R")
require(reshape2)
source("resizewin.R")
require(dplyr)
resize.win (12,9)
#run source code
require(tidyr)

library(readxl)
influxdata <- read_excel("Postdoc-R/Exported Tables/ThirdExp_virbac_plus rerun complete.xlsx")

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
