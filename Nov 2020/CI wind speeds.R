
source ("inspack.R")
windspeed <- read.csv("D:/R program/Postdoc-R/Exported Tables/foralidata.csv", sep=";")

#get data
CI <- windspeed %>% filter (date=="2012/07/13")

ggplot(CI, aes (log10(disrate))) + geom_histogram()

#convert to dissipation rates
#constants
K= 0.4 #von Karman's constant
Cd <- 1.15*(10)^-3 #drag coefficient
Den_air <-  (1.212)*10^-3 #g/m3 at sea level and 18C
Den_CH2O= 1.025 #g/m3 density seawater at 18C #these two will even out so no need to convert
CI$U <- sqrt(Cd*((CI$wind_speed_c)^2)*((Den_air/Den_CH2O)))
CI$Uc <- ((15.24/10)^0.143)*CI$U
CI$disrate <- (CI$U^3)/(K*(abs(30)))
