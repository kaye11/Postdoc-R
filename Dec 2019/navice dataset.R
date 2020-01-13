#open wind_navice workspace first

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

navice_alldata_backup <- navice_alldata %>% select (1:2, 4, 6:13, 16, 19)

#make it a long df
navice_alldata <- navice_alldata_backup
navice_alldata <- navice_alldata[rep(seq_len(nrow(navice_alldata)), 3), ]
navice_alldata$entities <- rep(c("Nc", "Cc", "Li"), each=127)
navice_alldata$rad <- rep(c(1.8E-6, 2.3E-6, 1.3E-6), each=127)
navice_alldata$beta_BM <- rep (c(BM_beta_Nc, BM_beta_Cc, BM_beta_Li), each=127)
navice_alldata$beta_DS <- rep (c(DS_beta_Nc, DS_beta_Cc, DS_beta_Li), each=127)

#for calculating betas
navice_alldata$beta_turb <- (4.2*pi*((navice_alldata$disrate/(v*100^2))^0.5)*(((navice_alldata$rad+Rehv)*100)^3))*86400 
navice_alldata$beta_all <- navice_alldata$beta_BM + navice_alldata$beta_DS + 
  navice_alldata$beta_turb

#change dates and reorder levels
navice_alldata$date <-format(navice_alldata$date , format="%m-%d")
navice_alldata$date2 = factor(navice_alldata$date, levels=c("06-30","07-01", "07-02", "07-03", "07-04", "07-05", "07-07", "07-08", "07-09","07-10", "07-06","07-12","06-24", "06-25", "06-27"))
                             
ggplot(navice_alldata, aes(x=as.factor(date2), y=log10(EhVperml_SYBR), color="extracellular", shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) +
  geom_boxplot(data=navice_alldata, aes(x=as.factor(date2), y=log10(`EhV copies mL`), color="intracellular")) + geom_point(data=navice_alldata, aes(x=as.factor(date2), y=log10(`EhV copies mL`), color="intracellular", shape=Infection), position = position_jitterdodge(), size=2) + 
  labs (y="EhV per ml", x= "date") + theme_Publication2() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(navice_alldata, aes(x=as.factor(date2), y=log10(`EhV copies mL`), color=Infection, shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + 
  labs (y="EhV per ml", x= "date") + theme_Publication2() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(navice_alldata, aes(x=as.factor(date2), y=log10(lithsperml), color=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2)  +
  labs (y="log10 liths per mL", x= "infection stage") + theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(navice_alldata, aes(x=as.factor(date), y=log10(`Av Ehux ml`), color=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2)  +
  labs (y="log10 Ehux per mL", x= "infection stage") + theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#PLOT BETAS
ggplot(navice_alldata, aes(x=date2, y=log10(beta_all), color=entities, shape=Infection))+ geom_line() + labs (y = expression(beta~("Encounters"~cm^3~day^-1)), x= "date") + theme_Publication2() + theme(legend.title = element_blank())

#encounter rates
navice_alldata$encounters <- navice_alldata$beta_all*navice_alldata$`EhV copies mL`

ggplot(navice_alldata, aes(x=date2, y=log10(encounters), color=entities, shape=Infection))+ geom_boxplot()+ labs (y = expression(beta~("Encounters"~cm^3~day^-1)), x= "date") + theme_Publication2() + theme(legend.title = element_blank())

#overlay betas and en
ylim.prim <- c(0, 4000)   # in this example, Ehux
ylim.sec <- c(0,5)    # in this example, beta
b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])

ggplot(navice_alldata, aes(x=as.factor(date), y=log10(`Av Ehux ml`), color=Infection)) +geom_boxplot() + geom_point(aes(y = log10(beta_all), shape = entities)) + scale_y_continuous("log10 Ehux", sec.axis = sec_axis(~ (. - a)/b, name = "encounter rates")) 

#you were trying secondary axes


#get proportion of cells
ggplot(navice_alldata, aes(x=as.factor(date), y=`Av Ehux ml`, color=depth)) +geom_boxplot()
ggplot(navice_alldata, aes(x=as.factor(date2), y=`Av Ehux ml`, color=Infection)) +geom_point() + geom_boxplot() 

###YOU STOPPED HERE
navice_alldata <- navice_alldata[rep(seq_len(nrow(navice_alldata)), 2), ]
navice_alldata$cellprop <- rep(c(0.1, 0.9, NA), each=381)
navice_alldata$countEhuxprop <- navice_alldata$cellprop*navice_alldata$`Av Ehux ml`

ggplot(navice_alldata, aes(x=as.factor(date), y=log10(count_Nc), color="Nc count", shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2)  +
  geom_boxplot(data=navice_alldata, aes(x=as.factor(date), y=log10(count_Cc), color="Cc count")) + geom_point(data=navice_alldata, aes(x=as.factor(date), y=log10(count_Cc), color="Cc count", shape=Infection), position = position_jitterdodge(), size=2) +
  labs (y="log10 Ehux per mL", x= "infection stage") + theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#calculate future encounters, focus on early infections
navice_alldata$E_Nc <- navice_alldata$beta_all_Nc*navice_alldata$EhVperml_SYBR
navice_alldata$E_Cc <- navice_alldata$beta_all_Cc*navice_alldata$EhVperml_SYBR
navice_alldata$E_Li <- navice_alldata$beta_all_Li*navice_alldata$EhVperml_SYBR

ggplot(navice_alldata, aes(x=as.factor(date), y=log10(E_Nc), color="Nc", shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2.5) +
  geom_boxplot(data=navice_alldata, aes(x=as.factor(date), y=log10(E_Cc), color="Cc")) + geom_point(data=navice_alldata, aes(x=as.factor(date), y=log10(E_Cc), color="Cc", shape=Infection), position = position_jitterdodge(), size=2.5) + 
  geom_boxplot(data=navice_alldata, aes(x=as.factor(date), y=log10(E_Li), color="Li")) + geom_point(data=navice_alldata, aes(x=as.factor(date), y=log10(E_Li), color="Li", shape=Infection), position = position_jitterdodge(), size=2.5) + 
  labs (y="viral encounters per particle", x= "infection stage") + theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#fast and slow viruses counts and betas of NC, Cc, and Li
navice_alldata$E_Nc_fast <- navice_alldata$beta_all_Nc*navice_alldata$EhVperml_SYBR*navice_alldata$`%fastEhV`
navice_alldata$E_Nc_slow <- navice_alldata$beta_all_Nc*navice_alldata$EhVperml_SYBR*navice_alldata$`%slowEhV`
navice_alldata$E_Cc_fast <- navice_alldata$beta_all_Cc*navice_alldata$EhVperml_SYBR*navice_alldata$`%fastEhV`
navice_alldata$E_Cc_slow <- navice_alldata$beta_all_Cc*navice_alldata$EhVperml_SYBR*navice_alldata$`%slowEhV`
navice_alldata$E_Li_fast <- navice_alldata$beta_all_Li*navice_alldata$EhVperml_SYBR*navice_alldata$`%fastEhV`
navice_alldata$E_Li_slow <- navice_alldata$beta_all_Li*navice_alldata$EhVperml_SYBR*navice_alldata$`%slowEhV`

##fast and slow viruses encounter rates
navice_alldata.trim2 <- navice_alldata %>% filter (!(Infection %in% c("Late Infection", "Post Bloom")))

ggplot(navice_alldata.trim2, aes(x=as.factor(date), y=log10(EhVperml_SYBR*`%fastEhV`), color="EhV fast count", shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2.5)  +
  geom_boxplot(data=navice_alldata.trim2, aes(x=as.factor(date), y=log10(EhVperml_SYBR*`%slowEhV`), color="EhV slow count")) + geom_point(data=navice_alldata.trim2, aes(x=as.factor(date), y=log10(EhVperml_SYBR*`%slowEhV`), color="EhV slow count", shape=Infection), position = position_jitterdodge(), size=2.5) +
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