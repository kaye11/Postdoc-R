library(dplyr)
library(plyr)

multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T, sep="", stringsAsFactors=FALSE)})
}

mydata=multmerge("d:/Postdoc/theoretical/NA-VICE/wind")

mergeddata <- ldply(mydata, data.frame)

##just get date, wind_speed_c
source("inspack.R")
winddata <- mergeddata %>% select (date, wind_speed_c, wind_speed_r_port,	wind_speed_r_stbd,	wind_speed_c_port,	wind_speed_c_stbd)

write.table (winddata, "Postdoc-R/Exported Tables/winddata.csv", sep=";", col.names=T, row.names=F)
winddata.raw.sum <- summarySE (winddata, measurevar = "wind_speed_c", groupvars = c ("date"))

#get stations, cast, through date
casts <- read.csv("D:/R program/Postdoc-R/CSV Files/casts.csv")

windmas <- left_join(winddata, casts, by = "date")
winddata.sum <- summarySE (windmas, measurevar = "wind_speed_c", groupvars = c ("date", "cast", "Station", "Infection"))
winddata.sum2 <- winddata.sum

#use all data
windmas <- windmas[rep(seq_len(nrow(windmas)), 301), ]
windmas$depth <- rep(c(seq(-150, 0, 0.5)), each=31680)

#constants
K= 0.4 #von Karman's constant
Cd <- 1.15*(10)^-3 #drag coefficient
Den_air <-  (1.225)*10^-3 #g/m3 at sea level and 15C
Den_CH2O= 1.025 #g/m3 density seawater at 18C #these two will even out so no need to convert
windmas$U <- sqrt(Cd*((windmas$wind_speed_c)^2)*((Den_air/Den_CH2O)))
windmas$Uc <- ((10/15.24)^0.143)*windmas$U
windmas$disrate <- (windmas$U^3)/(K*(abs(windmas$depth)))

#this plot is chaotic, dont run. using all data is such a bad idea
#ggplot(windmas, aes(x=log10(E), y=depth, color=as.factor(cast))) + geom_smooth() + theme_clean() + labs (x="Dissipation rate", y="Depth")

#use the means
winddata.sum <- winddata.sum2
winddata.sum <- winddata.sum[rep(seq_len(nrow(winddata.sum)), 301), ]
winddata.sum$depth <- rep(c(seq(-150, 0, 0.5)), each=21) #problem here

#constants
winddata.sum$U <- sqrt(Cd*((winddata.sum$wind_speed_c)^2)*((Den_air/Den_CH2O)))
winddata.sum$Uc <- ((10/15.24)^0.143)*winddata.sum$U
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


#save summary
write.table(winddata.sum2, "Postdoc-R/Exported Tables/winddata_sum2.csv", sep=";", col.names=T, row.names=F)

