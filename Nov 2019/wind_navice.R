library(dplyr)
library(plyr)

multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T, sep="")})
}

mydata=multmerge("d:/Postdoc/theoretical/NA-VICE/wind")

mergeddata <- ldply(mydata, data.frame)

##just get date, wind_speed_c
source("inspack.R")
winddata <- mergeddata %>% select (date, wind_speed_c)
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

ggplot(winddata.sum, aes(x=log10(disrate), y=depth, color=as.factor(cast), shape=Infection)) + geom_point(size=1) + geom_line() + theme_clean() + labs (x="Dissipation rate", y="Depth")

winddata.sum$StatInf <- as.factor (paste(winddata.sum$Station, winddata.sum$Infection, sep="-"))
ggplot(winddata.sum, aes(x=log10(disrate), y=depth, color=as.factor(cast))) + geom_line(size=1.5) +  theme_clean() + labs (x="Dissipation rate", y="Depth") + facet_grid(~Infection)
ggplot(winddata.sum %>% filter (depth>=-10), aes(x=log10(disrate), y=depth, color=as.factor(cast))) + geom_line(size=1.5) +  theme_clean() + labs (x="Dissipation rate", y="Depth") + facet_grid(Infection~.)
ggplot(winddata.sum %>% filter (depth>=-20), aes(x=log10(disrate), y=depth, color=as.factor(cast), shape=Station)) + geom_line(size=1) + geom_point(size=2) + theme_clean() + labs (x="Dissipation rate", y="Depth") + facet_grid(Infection~.)

r#save summary
write.table(winddata.sum2, "Postdoc-R/Exported Tables/winddata_sum2.csv", sep=";", col.names=T, row.names=F)

##get na-vice data
#navice <- read.csv("D:/Postdoc/theoretical/NA-VICE/NAVICE Ehux Sytox VLP wrangled_KB_191119.csv")
#has_data <- function(x) { sum(!is.na(x)) > 0 }
#navice <- navice %>% select_if(has_data)
library(readxl)
navice <- read_excel("D:/Postdoc/theoretical/NA-VICE/NAVICE Ehux Sytox VLP wrangled_KB_191119.xlsx")
navice$date <- as.Date (navice$date)
winddata.sum$date <- as.Date(winddata.sum$date)
navice_alldata <- left_join(navice, winddata.sum %>% select (date, cast, Station, Infection, depth, disrate))

write.table(navice_alldata, "Postdoc-R/Exported Tables/navice_alldata.csv", sep=";", col.names=T, row.names=F)

#1. calculate encounters, add beta kernels
BM_beta_Nc <- 4.534116e-09
BM_beta_Cc_Mc <- 5.674263e-09
BM_beta_Cc_Hc <- 5.674263e-09
BM_beta_Cc <- 5.674263e-09
BM_beta_Li_Mc <- 3.395688e-09
BM_beta_Li_Hc <- 3.395688e-09
BM_beta_Li <- 3.395688e-09

DS_beta_Nc <- 3.703283e-07
DS_beta_Cc_Mc <- 2.561877e-06
DS_beta_Cc_Hc <- 4.647220e-06
DS_beta_Cc <- 4.647220e-06
DS_beta_Li_Mc <- 1.673026e-06
DS_beta_Li_Hc <- 2.702580e-06
DS_beta_Li <- 2.702580e-06

Reh_calc= 2.3E-6 #in m radius Ehux
Reh_naked= 1.8E-6 #in m radius Ehux
Reh_lith = 1.3E-6 #in m radius
v= 1.099*(10)^-6 #m2/s kinematic viscosity in 18C
Rehv= 90*(10)^-9 #in m radius virus

navice_alldata$beta_turb_Nc <- (4.2*pi*((navice_alldata$disrate/(v*100^2))^0.5)*(((Reh_naked+Rehv)*100)^3))*86400 
navice_alldata$beta_turb_Cc <- (4.2*pi*((navice_alldata$disrate/(v*100^2))^0.5)*(((Reh_calc+Rehv)*100)^3))*86400 
navice_alldata$beta_turb_Li <- (4.2*pi*((navice_alldata$disrate/(v*100^2))^0.5)*(((Reh_lith+Rehv)*100)^3))*86400 

navice_alldata$beta_all_Nc <- BM_beta_Nc + DS_beta_Nc + navice_alldata$beta_turb_Nc
navice_alldata$beta_all_Cc <- BM_beta_Cc + DS_beta_Cc + navice_alldata$beta_turb_Cc
navice_alldata$beta_all_Li <- BM_beta_Li + DS_beta_Li + navice_alldata$beta_turb_Li

navice_alldata$StatInf <- as.factor (paste(navice_alldata$Station, navice_alldata$Infection, sep="-"))

ggplot(navice_alldata, aes(x=as.factor(Infection), y=log10(EhVperml_SYBR), color="extracellular")) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) +
  geom_boxplot(data=navice_alldata, aes(x=as.factor(Infection), y=log10(`EhV copies mL`), color="intracellular")) + geom_point(data=navice_alldata, aes(x=as.factor(Infection), y=log10(`EhV copies mL`), color="intracellular"), position = position_jitterdodge(), size=2) + 
  labs (y="EhV per ml", x= "infection stage") + theme_Publication2()

ggplot(navice_alldata, aes(x=as.factor(Infection), y=log10(lithsperml), color=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2)  +
  labs (y="log10 liths per mL", x= "infection stage") + theme_Publication2()

ggplot(navice_alldata, aes(x=as.factor(Infection), y=log10(`Av Ehux ml`), color=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2)  +
  labs (y="log10 Ehux per mL", x= "infection stage") + theme_Publication2()


#PLOT BETAS
ggplot(navice_alldata, aes(x=log10(disrate), y=log10(beta_all_Nc), color="Nc")) +geom_line() + geom_point (size=2) +
  geom_line(data=navice_alldata, aes(x=log10(disrate), y=log10(beta_all_Cc), color="Cc")) + geom_point(data=navice_alldata, aes(x=log10(disrate), y=log10(beta_all_Cc), color="Cc"), size=2) + 
  geom_line(data=navice_alldata, aes(x=log10(disrate), y=log10(beta_all_Li), color="Li")) + geom_point(data=navice_alldata, aes(x=log10(disrate), y=log10(beta_all_Li), color="Li"), size=2) + 
  labs (y = expression(beta~("Encounters"~cm^3~day^-1)), x= "dissipation rate") + theme_Publication2() + facet_grid(~Infection) + theme(legend.title = element_blank())

#get proportion of cells
ggplot(navice_alldata, aes(x=as.factor(cast), y=`Av Ehux ml`, color=depth)) +geom_boxplot()
ggplot(navice_alldata, aes(x=as.factor(depth), y=`Av Ehux ml`, color=Infection)) +geom_point() + geom_boxplot() 

navice_alldata$count_Nc <- 0.1*navice_alldata$`Av Ehux ml`
navice_alldata$count_Cc <- 0.9*navice_alldata$`Av Ehux ml`

ggplot(navice_alldata, aes(x=as.factor(Infection), y=log10(count_Nc), color="Nc count")) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2)  +
  geom_boxplot(data=navice_alldata, aes(x=as.factor(Infection), y=log10(count_Cc), color="Cc count")) + geom_point(data=navice_alldata, aes(x=as.factor(Infection), y=log10(count_Cc), color="Cc count"), position = position_jitterdodge(), size=2) +
  labs (y="log10 Ehux per mL", x= "infection stage") + theme_Publication2()

#calculate future encounters, focus on early infections
navice_alldata$E_Nc <- navice_alldata$beta_all_Nc*navice_alldata$EhVperml_SYBR
navice_alldata$E_Cc <- navice_alldata$beta_all_Cc*navice_alldata$EhVperml_SYBR
navice_alldata$E_Li <- navice_alldata$beta_all_Li*navice_alldata$EhVperml_SYBR

ggplot(navice_alldata, aes(x=as.factor(Infection), y=log10(E_Nc), color="Nc")) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) +
  geom_boxplot(data=navice_alldata, aes(x=as.factor(Infection), y=log10(E_Cc), color="Cc")) + geom_point(data=navice_alldata, aes(x=as.factor(Infection), y=log10(E_Cc), color="Cc"), position = position_jitterdodge(), size=2) + 
  geom_boxplot(data=navice_alldata, aes(x=as.factor(Infection), y=log10(E_Li), color="Li")) + geom_point(data=navice_alldata, aes(x=as.factor(Infection), y=log10(E_Li), color="Li"), position = position_jitterdodge(), size=2) + 
  labs (y="viral encounters per particle", x= "infection stage") + theme_Publication2()

#fast and slow viruses counts and betas of NC, Cc, and Li
navice_alldata$E_Nc_fast <- navice_alldata$beta_all_Nc*navice_alldata$EhVperml_SYBR*navice_alldata$`%fastEhV`
navice_alldata$E_Nc_slow <- navice_alldata$beta_all_Nc*navice_alldata$EhVperml_SYBR*navice_alldata$`%slowEhV`
navice_alldata$E_Cc_fast <- navice_alldata$beta_all_Cc*navice_alldata$EhVperml_SYBR*navice_alldata$`%fastEhV`
navice_alldata$E_Cc_slow <- navice_alldata$beta_all_Cc*navice_alldata$EhVperml_SYBR*navice_alldata$`%slowEhV`
navice_alldata$E_Li_fast <- navice_alldata$beta_all_Li*navice_alldata$EhVperml_SYBR*navice_alldata$`%fastEhV`
navice_alldata$E_Li_slow <- navice_alldata$beta_all_Li*navice_alldata$EhVperml_SYBR*navice_alldata$`%slowEhV`

##fast and slow viruses encounter rates
navice_alldata.trim2 <- navice_alldata %>% filter (!(Infection %in% c("Late Infection", "Post Bloom")))

ggplot(navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(EhVperml_SYBR*`%fastEhV`), color="EhV fast count")) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2)  +
  geom_boxplot(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(EhVperml_SYBR*`%slowEhV`), color="EhV slow count")) + geom_point(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(EhVperml_SYBR*`%slowEhV`), color="EhV slow count"), position = position_jitterdodge(), size=2) +
  labs (y="log10 EhVs", x= "infection stage") + theme_Publication2()


ggplot(navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Nc_slow), color="Nc")) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) +
  geom_boxplot(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Cc_slow), color="Cc")) + geom_point(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Cc_slow), color="Cc"), position = position_jitterdodge(), size=2) + 
  geom_boxplot(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Li_slow), color="Li")) + geom_point(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Li_slow), color="Li"), position = position_jitterdodge(), size=2) + 
  labs (y="slow viruses encounters per particle", x= "infection stage") + theme_Publication2() + theme (legend.title = element_blank(), axis.title.x = element_blank()) + ylim (0,2)

ggplot(navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Nc_fast), color="Nc")) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) +
  geom_boxplot(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Cc_fast), color="Cc")) + geom_point(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Cc_fast), color="Cc"), position = position_jitterdodge(), size=2) + 
  geom_boxplot(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Li_fast), color="Li")) + geom_point(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Li_fast), color="Li"), position = position_jitterdodge(), size=2) + 
  labs (y="fast viruses encounters per particle", x= "infection stage") + theme_Publication2() + theme (legend.title = element_blank(), axis.title.x = element_blank()) + ylim (0,2)

##adsorption
#viruses have same adsorption across the board
#reservoirs differ. calculated adsorptivity of reservoirs using the lith as a baseline (ads=1)
#lith=1, Cc=0.4, Nc=0.12
navice_alldata.trim2$E_Nc_fast_ads <- navice_alldata.trim2$E_Nc_fast*0.12
navice_alldata.trim2$E_Nc_slow_ads <- navice_alldata.trim2$E_Nc_slow*0.12
navice_alldata.trim2$E_Cc_fast_ads <- navice_alldata.trim2$E_Cc_fast*0.04
navice_alldata.trim2$E_Cc_slow_ads <- navice_alldata.trim2$E_Cc_slow*0.04
navice_alldata.trim2$E_Li_fast_ads <- navice_alldata.trim2$E_Li_fast*1
navice_alldata.trim2$E_Li_slow_ads <- navice_alldata.trim2$E_Li_slow*1

ggplot(navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Nc_slow_ads), color="Nc")) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) +
  geom_boxplot(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Cc_slow_ads), color="Cc")) + geom_point(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Cc_slow_ads), color="Cc"), position = position_jitterdodge(), size=2) + 
  geom_boxplot(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Li_slow_ads), color="Li")) + geom_point(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Li_slow_ads), color="Li"), position = position_jitterdodge(), size=2) + 
  labs (y="slow viruses encounters+ads", x= "infection stage") + theme_Publication2() + theme (legend.title = element_blank(), axis.title.x = element_blank()) + ylim (-1,2)

ggplot(navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Nc_fast_ads), color="Nc")) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) +
  geom_boxplot(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Cc_fast_ads), color="Cc")) + geom_point(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Cc_fast_ads), color="Cc"), position = position_jitterdodge(), size=2) + 
  geom_boxplot(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Li_fast_ads), color="Li")) + geom_point(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Li_fast_ads), color="Li"), position = position_jitterdodge(), size=2) + 
  labs (y="fast viruses encounters+ads", x= "infection stage") + theme_Publication2() + theme (legend.title = element_blank(), axis.title.x = element_blank()) + ylim (-1,2)


###infection with fast and slow
#fast viruses have higher infectivity (0.3), slow viruses have lower infectivity (0.06) from Jozef's paper
navice_alldata.trim2$E_Nc_fast_inf_ads <- navice_alldata.trim2$E_Nc_fast_ads*0.3
navice_alldata.trim2$E_Nc_slow_inf_ads <- navice_alldata.trim2$E_Nc_slow_ads*0.06
navice_alldata.trim2$E_Cc_fast_inf_ads <- navice_alldata.trim2$E_Cc_fast_ads*0.3
navice_alldata.trim2$E_Cc_slow_inf_ads <- navice_alldata.trim2$E_Cc_slow_ads*0.06

ggplot(navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Nc_slow_inf_ads), color="Nc")) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) +
  geom_boxplot(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Cc_slow_inf_ads), color="Cc")) + geom_point(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Cc_slow_inf_ads), color="Cc"), position = position_jitterdodge(), size=2) + theme_Publication2() + theme (legend.title = element_blank(), axis.title.x = element_blank()) + labs (y="log10 successful infection (slow viruses)") + ylim (-2,0)

ggplot(navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Nc_fast_inf_ads), color="Nc")) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) +
  geom_boxplot(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Cc_fast_inf_ads), color="Cc")) + geom_point(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Cc_fast_inf_ads), color="Cc"), position = position_jitterdodge(), size=2) + theme_Publication2() + theme (legend.title = element_blank(), axis.title.x = element_blank()) + labs (y="log10 successful infection (fast viruses)") + ylim (-2,0)

##consider calcified and naked cells
#calcified cells are less susceptible, infection will go lower (prob=0.5)
#naked cells are more susceptible, infection will go higher (prob=1)
#infection with naked and slow
navice_alldata.trim2$E_Nc_fast_inf_ads_Nc <- navice_alldata.trim2$E_Nc_fast_inf_ads*1 #naked cell 
navice_alldata.trim2$E_Nc_slow_inf_ads_Nc <- navice_alldata.trim2$E_Nc_slow_inf_ads*1
navice_alldata.trim2$E_Cc_fast_inf_ads_Cc <- navice_alldata.trim2$E_Cc_fast_inf_ads*0.5
navice_alldata.trim2$E_Cc_slow_inf_ads_Cc <- navice_alldata.trim2$E_Cc_fast_inf_ads*0.5

ggplot(navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Nc_slow_inf_ads_Nc), color="Nc")) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) +
  geom_boxplot(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Cc_slow_inf_ads_Cc), color="Cc")) + geom_point(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Cc_slow_inf_ads_Cc), color="Cc"), position = position_jitterdodge(), size=2) + theme_Publication2() + theme (legend.title = element_blank(), axis.title.x = element_blank()) + labs (y="log10 successful infection (slow viruses)") + ylim (-2,0)

ggplot(navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Nc_fast_inf_ads_Nc), color="Nc")) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) +
  geom_boxplot(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Cc_fast_inf_ads_Cc), color="Cc")) + geom_point(data=navice_alldata.trim2, aes(x=as.factor(Infection), y=log10(E_Cc_fast_inf_ads_Cc), color="Cc"), position = position_jitterdodge(), size=2) + theme_Publication2() + theme (legend.title = element_blank(), axis.title.x = element_blank()) + labs (y="log10 successful infection (fast viruses)") + ylim (-2,0)

#THIS IS A VERY SLOPPY CODE. MELT EVERYTHING TOMORROW AND RE-DO THIS CODE. 