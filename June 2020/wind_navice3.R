library(dplyr)
library(plyr)

multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T, sep="", stringsAsFactors=FALSE)})
}

mydata=multmerge("d:/Postdoc/theoretical/NA-VICE/wind")

mergeddata <- ldply(mydata, data.frame)

alidata <-  mergeddata %>% select (lon, lat, date, ISO_DateTime_UTC, wind_speed_c)
write.table (alidata, "Postdoc-R/Exported Tables/foralidata.csv", sep=";", col.names=T, row.names=F)


##just get date, wind_speed_c
source("inspack.R")
winddata <- mergeddata %>% select (date, wind_speed_c, wind_speed_r_port,	wind_speed_r_stbd,	wind_speed_c_port,	wind_speed_c_stbd)

write.table (winddata, "Postdoc-R/Exported Tables/winddata.csv", sep=";", col.names=T, row.names=F)

winddata <- read.csv("D:/R program/Postdoc-R/Exported Tables/winddata.csv", sep=";")

winddata_navice <- winddata %>% filter (date %in% c ( "2012/07/01", "2012/07/02", "2012/07/03", "2012/07/04", "2012/07/05", "2012/07/06", "2012/07/07", "2012/07/08", "2012/07/09", "2012/07/10", "2012/07/12", "2012/06/24", "2012/06/25", "2012/06/27", "2012/06/30"))

write.table (winddata_navice, "Postdoc-R/Exported Tables/winddata_navice.csv", sep=";", col.names=T, row.names=F)


winddata.raw.sum <- summarySE (winddata, measurevar = "wind_speed_c", groupvars = c ("date"))

ggplot(winddata, aes(x=date, y=wind_speed_c)) + geom_boxplot()

#get stations, cast, through date
casts <- read.csv("D:/R program/Postdoc-R/CSV Files/casts.csv")

windmas <- left_join(winddata, casts, by = "date")

ggplot(windmas, aes(x=date, y=wind_speed_c)) + geom_boxplot()

windmas <- windmas %>% filter (!(Station=="NA"))

winddata.sum <- summarySE (windmas, measurevar = "wind_speed_c", groupvars = c ("date", "cast", "Station", "Infection"))
winddata.sum2 <- winddata.sum

#use all data
windmas.long <- windmas[rep(seq_len(nrow(windmas)), 301), ]
windmas.long$depth <- rep(c(seq(-150, 0, 0.5)), each=31680)

#constants
K= 0.4 #von Karman's constant
Cd <- 1.15*(10)^-3 #drag coefficient
Den_air <-  (1.212)*10^-3 #g/m3 at sea level and 18C
Den_CH2O= 1.025 #g/m3 density seawater at 18C #these two will even out so no need to convert
windmas.long$U <- sqrt(Cd*((windmas.long$wind_speed_c)^2)*((Den_air/Den_CH2O)))
windmas.long$Uc <- ((15.24/10)^0.143)*windmas.long$U
windmas.long$disrate <- (windmas.long$U^3)/(K*(abs(windmas.long$depth)))

#this plot is chaotic, dont run. using all data is such a bad idea
#ggplot(windmas, aes(x=log10(E), y=depth, color=as.factor(cast))) + geom_smooth() + theme_clean() + labs (x="Dissipation rate", y="Depth")

#get windmas for 0-30m
wind_navice <- windmas.long %>% filter (depth %in% (-30:-1))

ggplot(wind_navice, aes(x=as.factor(depth), y=disrate)) + geom_boxplot()

#use the means
winddata.sum <- winddata.sum2
winddata.sum <- winddata.sum[rep(seq_len(nrow(winddata.sum)), 301), ]
winddata.sum$depth <- rep(c(seq(-150, 0, 0.5)), each=21) 

#constants
winddata.sum$U <- sqrt(Cd*((winddata.sum$wind_speed_c)^2)*((Den_air/Den_CH2O)))
winddata.sum$Uc <- ((15.24/10)^0.143)*winddata.sum$U
winddata.sum$disrate <- (winddata.sum$U^3)/(K*(abs(winddata.sum$depth)))

ggplot(winddata.sum, aes(x=log10(disrate), y=depth, color=as.factor(date), shape=Infection)) + geom_point(size=1) + geom_line() + theme_clean() + labs (x="Dissipation rate", y="Depth")

winddata.sum$StatInf <- as.factor (paste(winddata.sum$Station, winddata.sum$Infection, sep="-"))
ggplot(winddata.sum, aes(x=log10(disrate), y=depth, color=as.factor(date))) + geom_line(size=1.5) +  theme_clean() + labs (x="Dissipation rate", y="Depth") + facet_grid(~Infection)
ggplot(winddata.sum %>% filter (depth>=-10), aes(x=log10(disrate), y=depth, color=as.factor(cast))) + geom_line(size=1.5) +  theme_clean() + labs (x="Dissipation rate", y="Depth") + facet_grid(Infection~.)
ggplot(winddata.sum %>% filter (depth>=-20), aes(x=log10(disrate), y=depth, color=as.factor(cast), shape=Station)) + geom_line(size=1) + geom_point(size=2) + theme_clean() + labs (x="Dissipation rate", y="Depth") + facet_grid(Infection~.)

##make a pdf
resize.win(6,8)
ggplot(winddata.sum %>% filter (depth>=-30), aes(x=log10(disrate), color=as.factor(depth))) + geom_density() +theme_Publication2() + labs (x="log10 dissipation rate") + theme (legend.position = "none")

ggplot(winddata.sum %>% filter (disrate>1e-7) %>% filter (depth>=-30))+ geom_density(aes(x=log10(disrate), fill=Infection, color=Infection)) + facet_grid (Infection~.)+theme_Publication2() + labs (x="log10 dissipation rate") + theme (legend.position = "none")

##wind data

#save summary
write.table(winddata.sum, "Postdoc-R/Exported Tables/winddata_sum_200629.csv", sep=";", col.names=T, row.names=F)

##join wind data
library(readxl)
navice <- read_excel("D:/Postdoc/theoretical/NA-VICE/NAVICE Ehux Sytox VLP wrangled_KB_200629.xlsx")
navice$date <- as.Date (navice$date)
winddata.sum$date <- as.Date(winddata.sum$date)
navice_alldata <- left_join(navice, winddata.sum %>% select (date, cast, Station, Infection, depth, wind_speed_c, disrate))

write.table(navice_alldata, "Postdoc-R/Exported Tables/navice_alldata_18C_200629.csv", sep=";", col.names=T, row.names=F)

#join disrate data into a wrangled data set

navice_long <- read_excel("D:/Postdoc/theoretical/NA-VICE/NAVICE_long_wrangledinexcel_avebetaDS_18C_formerge_200629.xlsx")
navice_long$date <- as.Date (navice_long$date)
navice_long_alldata <- left_join(navice_long, winddata.sum %>% select (date, cast, Station, Infection, depth, disrate))

write.table(navice_long_alldata, "Postdoc-R/Exported Tables/navice_alldata_foranalysis_200629.csv", sep=";", col.names=T, row.names=F)


