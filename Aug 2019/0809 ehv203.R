library(readxl)
ehv203 <- read_excel("D:/Postdoc/Experiments/190809 EhV 203 recheck/summary.xlsx")
View(ehv203)

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

ehv203$strain <- as.factor (ehv203$strain)
ehv203$time <- ehv203$day*24
ehv203$dead <- ehv203$countperml*ehv203$sytox
ehv203$sytox <- ehv203$sytox*100

#divide cellcount measurements by 10^6
ehv203$countpermldiv <- ehv203$countperml/10^6

#divide dead cells by 10^4
ehv203$deadpermldiv <- ehv203$dead/10^4

ehv203.long <- melt (data=ehv203, id.vars=c("strain", "time", "day", "rep"), variable.name="parameter")

#export data
setwd("D:/R program")
require(openxlsx)
write.xlsx(ehv203.long, file = "Postdoc-R/Exported Tables/0809 ehv203.xlsx")
