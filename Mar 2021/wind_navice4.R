##calculate dissipation rates from NAVICE wind speeds. wind speeds downloaded from BCO-DMO. 

## ------------------------------------------------------------------------
####downloaded data processing
source("inspack.R")

multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T, sep="", stringsAsFactors=FALSE)})
}

mydata=multmerge("d:/Postdoc/theoretical/NA-VICE/wind")
mergeddata <- ldply(mydata, data.frame)

##just get date, wind_speed_c
winddata <- mergeddata %>% select (date, wind_speed_c, wind_speed_r_port,	wind_speed_r_stbd,	wind_speed_c_port,	wind_speed_c_stbd)
write.table (winddata, "Postdoc-R/Exported Tables/winddata_mar2021.csv", sep=";", col.names=T, row.names=F)

#get stations, cast, through date
casts <- read.csv("D:/R program/Postdoc-R/CSV Files/casts.csv")
casts$date <- as.Date (casts$date)
winddata$date <- as.Date(winddata$date)
windmas <- inner_join(winddata, casts, by = "date")

## ------------------------------------------------------------------------
######calculate dissipation rates using the mean of the wind speeds

#constants
K= 0.4 #von Karman's constant
Cd <- 1.15*(10)^-3 #drag coefficient
Den_air <-  (1.212)*10^-3 #g/m3 at sea level and 18C
Den_CH2O= 1.025 #g/m3 density seawater at 18C #these two will even out so no need to convert

#calculate dissipation rates from 0 to 30 m (mixed layer depth)
winddata.sum <- summarySE (windmas, measurevar = "wind_speed_c", groupvars = c ("date", "cast", "Station", "Infection"))
winddata.sum <- winddata.sum[rep(seq_len(nrow(winddata.sum)), 301), ]
winddata.sum$depth <- rep(c(seq(-150, 0, 0.5)), each=21) 
winddata.sum$U <- sqrt(Cd*((winddata.sum$wind_speed_c)^2)*((Den_air/Den_CH2O)))
winddata.sum$Uc <- ((15.24/10)^0.143)*winddata.sum$U
winddata.sum$disrate <- (winddata.sum$U^3)/(K*(abs(winddata.sum$depth)))

write.table(winddata.sum, "Postdoc-R/Exported Tables/winddata_sum_mar2021.csv", sep=";", col.names=T, row.names=F)

## ------------------------------------------------------------------------
#####join wind data with navice biological dataset (wrangled). dataset has Ehux, Li, and Vi concentration
library(readxl)
#join disrate data into a wrangled data set

navice_long <- read_excel("D:/Postdoc/theoretical/NA-VICE/NAVICE_long_wrangledinexcel_formerge_mar2021.xlsx")
navice_long$date <- as.Date (navice_long$date)
winddata.sum$date <- as.Date(winddata.sum$date)
navice_long_alldata <- left_join(navice_long, winddata.sum %>% select (date, cast, Station, Infection, depth, disrate))

write.table(navice_long_alldata, "Postdoc-R/Exported Tables/navice_alldata_foranalysis_mar2021.csv", sep=";", col.names=T, row.names=F)


