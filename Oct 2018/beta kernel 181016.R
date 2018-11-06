
#values needed 

K= 1.38064852*(10)^-23 #m2 kg/ s2 K boltzmann constant
mu= 1.126*(10)^-3 #kg/m s dynamic viscosity in 18C
v= 1.099*(10)^-6 #m2/s kinematic viscosity in 18C
Reh_calc= 2.3E-6 #in m radius Ehux
Reh_naked= 1.8E-6 #in m radius Ehux
Rehv= 90E-9 #in m radius virus
Temp = 18+273.15 #temp in kelvin, here assuming 18C
Den_OcM = 1.05 #g/cm3 density organic cell matter
Den_CH2O= 1.025 #g/cm3 density seawater at 18C
hostnum <- (10)^3
virnum <- (10)^4

require (ggplot2)
require(plotly)
require(grid)
require(ggthemes)
require (dplyr)
require(plyr)
source ("theme_Publication.R")
source("resizewin.R")
resize.win(12,9)
grid.newpage()


#Brownian motion (BM)
#1. determine diffusivity (D)
D_TK_host_calc <- ((K*Temp)/(6*pi*mu*Reh_calc))*(10)^4 #in cm2/s diffusity for a spherical host
D_TK_host_naked <- ((K*Temp)/(6*pi*mu*Reh_naked))*(10)^4 #in cm2/s diffusity for a spherical host
D_TK_virus <- ((K*Temp)/(6*pi*mu*Rehv))*(10)^4 #in cm2/s diffusity for a spherical virus

#2. calculate beta (beta)
beta_BM_Burd_calc <- ((2/3)*(K*Temp/mu)*((Reh_calc+Rehv)^2/(Reh_calc*Rehv)))*((10)^4)*86400
beta_BM_Burd_naked <- ((2/3)*(K*Temp/mu)*((Reh_naked+Rehv)^2/(Reh_naked*Rehv)))*((10)^4)*86400

#beta_BM_TK_calc <- ((4*pi)*(D_TK_host_calc+D_TK_virus)*((Reh_calc+Rehv)*100))*86400 #Brownian motion TK formula, in cm3/day
#beta_BM_TK_naked <- ((4*pi)*(D_TK_host_naked+D_TK_virus)*((Reh_naked+Rehv)*100))*86400 #Brownian motion TK formula, in cm3/day

#3. calculate encounters (E)
E_BM_HV_calc <- beta_BM_Burd_calc*hostnum*virnum #E calculated with Virus and Host (10:1 MOI)
E_BM_V_calc <- beta_BM_Burd_calc*virnum #E calculated with Virus 
E_BM_HV_naked <- beta_BM_Burd_naked*hostnum*virnum #E calculated with Virus and Host (10:1 MOI)
E_BM_V_naked <- beta_BM_Burd_naked*virnum #E calculated with Virus
E_BM_H_calc <- beta_BM_Burd_calc*hostnum #E calculated with Virus and Host (10:1 MOI)


#Differential settling (DS)
#1. read in PIC data
library(readr) #always use readr not baseR

PIC <- read_csv("Postdoc/CSV Files/PIC.csv")

PIC$Strain <- as.factor(PIC$Strain)
PIC$Replicate <- as.factor(PIC$Replicate)

#certain changes in data.table API made calculating inside the list data.table to not work

#2. calculate PIC
PIC$PIC <- PIC$TC-PIC$AC
PIC$PICpercell <- (PIC$PIC/PIC$Cellcount)*(10)^-3#in g
PIC$PICpercellpg <- PIC$PICpercell*1e12

ggplotly(ggplot(data=PIC, aes(x=Strain, y=PICpercellpg)) + geom_boxplot()+geom_point(size=2) +theme_Publication())

#3. calculate density of cells (den)
PIC <- mutate(PIC, group = ifelse(PICpercellpg < 2 , "naked", "calcified"))

ggplotly(ggplot(data=PIC, aes(x=group, y=PICpercellpg)) + geom_boxplot()+geom_point(size=2) +theme_Publication())

PIC <- mutate(PIC, rad = ifelse(group == "naked" ,  1.8E-6,  2.3E-6)) #in m

PIC$volume <- (4/3)*pi*(PIC$rad*100)^3 #in cm3
PIC$Den_cell <- PIC$PICpercell/PIC$volume #g/cm3
PIC$Den_celltotal <- PIC$Den_cell+Den_OcM

ggplotly(ggplot(data=PIC, aes(x=Strain, y=Den_celltotal, color=group)) + geom_boxplot()+geom_point(size=2) 
         +theme_Publication())

#some strains that are "naked" have PIC<2. I chose to ignore this since in the lm model I do not use
#strain as a factor, rather data is treated as a whole (e.g., no grouping)

PIC$SinkVel <- ((2*((PIC$rad*100)^2)*(981)*(PIC$Den_celltotal-Den_CH2O))/(9*(mu*10)))*864 #meter per day
#g is converted to per day, 864 is the one that converts cm/s to m/day

#plot sinking velocity vs calcification

ggplot(data=PIC, aes(x=PICpercellpg, y=SinkVel, color=Strain)) + geom_point(size=5)+theme_Publication()+
  labs(y = expression("Sinking velocity"~("m"~day^-1)), x = expression("PIC"~cell^-1)) 

Ehv_SinkVel <- ((2*((Rehv*100)^2)*(981)*(Den_CH2O-Den_CH2O))/9*(mu*10))*864 #equals to 0

PIC$beta <- pi*(((PIC$rad+Rehv)*100)^2)*(abs(PIC$SinkVel/864)) #in encounters cm3/s

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

Sinkvelbeta.plot<- ggplot(data=PIC, aes(x=SinkVel, y=beta, color=Strain)) + geom_point(size=5)+theme_Publication()+
  labs(x = expression("Sinking velocity"~("m"~day^-1)), y = expression(beta~("Encounters"~cm^3~s^-1))) +
  scale_y_continuous(label=scientific_10) #+stat_smooth(method="lm", aes(group=1))

Sinkvelbeta.plot

ggplotly(Sinkvelbeta.plot)

ggplotly(ggplot(data=PIC, aes(x=Strain, y=SinkVel)) + geom_boxplot()+theme_Publication())

ggplot(data=PIC, aes(x=PICpercellpg, y=beta, color=Strain)) + geom_point(size=5)+theme_Publication()+
  labs(y = expression(beta~("Encounters"~cm^3~s^-1)), x = expression("PIC"~cell^-1))  +
  scale_y_continuous(label=scientific_10)

PIC_reg <- lm(SinkVel~PICpercellpg, data=PIC) #essentially perfect fit: summary may be unreliable haha
summary(PIC_reg)
plot(residuals.lm(PIC_reg))
layout(matrix(1:4,2,2))
plot(PIC_reg)

coef(PIC_reg)
# coef(PIC_reg)
#(Intercept) PICpercellpg 
#0.01698132   0.01778757

cor(PIC$PICpercellpg, PIC$SinkVel)
#cor=0.9993518

Beta_reg <- lm(beta~PICpercellpg, data=PIC)
plot(residuals.lm(PIC_reg))
coef(Beta_reg)
#coef(Beta_reg)
#(Intercept) PICpercellpg 
#3.071474e-12 3.704551e-12 

#encounters are in encounters cm3/ s then converted to encounters/cm3 s
PIC$E_DS_HV <- (PIC$beta*(10)^4*(10)^3)  #E calculated with Virus and Host (10:1 MOI)
PIC$E_DS_V <- (PIC$beta*(10)^4)#E calculated with Virus

E_DS_HV_reg <- lm(E_DS_HV~PICpercellpg, data=PIC)
E_DS_V_reg <- lm(E_DS_V~PICpercellpg, data=PIC)
plot(residuals.lm(E_DS_HV_reg))
plot(residuals.lm(E_DS_V_reg))

PIC$group2 <- case_when(
  PIC$PICpercellpg <2  ~ "naked_bouyant",
  PIC$PICpercellpg >2 & PIC$PICpercellpg < 4 ~ "naked/calcified uncertain",
  PIC$PICpercellpg >4 & PIC$PICpercellpg < 10 ~ "moderately calcified",
  PIC$PICpercellpg >10 ~ "strongly calcified", 
  TRUE ~ as.character(PIC$PICpercellpg)
)

breaks <- 10^(-10:10)

ggplot(data=PIC, aes(x=SinkVel, y=E_DS_HV, color=Strain, shape=group)) + geom_point(size=5)+theme_Publication()+
  scale_y_log10(breaks = breaks, label=scientific_10)

ggplot(data=PIC, aes(x=SinkVel, y=E_DS_V, color=Strain, shape=group)) + geom_point(size=5)+theme_Publication()+
  scale_y_log10(breaks = breaks, label=scientific_10)


summary_DS <- ddply(PIC, .(Strain), summarize,  PICpercellpg=mean(PICpercellpg), Den_celltotal = mean (Den_celltotal),
                    SinkVel=mean(SinkVel),beta=mean(beta), E_DS_V= mean(E_DS_V), E_DS_HV=mean(E_DS_HV))

summary_DS_bygroup <- ddply(PIC, .(group2), summarize,  PICpercellpg=mean(PICpercellpg), Den_celltotal = mean (Den_celltotal),
                            SinkVel=mean(SinkVel),beta=mean(beta), E_DS_V= mean(E_DS_V), E_DS_HV=mean(E_DS_HV))

require(openxlsx)
openxlsx::write.xlsx(summary_DS, file = "Postdoc/Exported Tables/summary_DS.xlsx")
openxlsx::write.xlsx(summary_DS_bygroup, file = "Postdoc/Exported Tables/summary_DS_bygroup.xlsx")

#make a prediction based on PIC values
#make new dataframe depending on experimental PIC values

require(truncnorm)
require(Rmisc)
summary(PIC$PICpercellpg)
summarySE(data=PIC, measurevar="PICpercellpg")
PIC_newdata <- as.data.frame(rtruncnorm(n=1000, a=-2, b=22, mean=4.7, sd=6.15))

#rename column. rename function in plyr 
PIC_newdata <- rename (PIC_newdata, c ("rtruncnorm(n = 1000, a = -2, b = 22, mean = 4.7, sd = 6.15)" = "PICpercellpg"))

PIC_newdata <- mutate(PIC_newdata, group = ifelse(PICpercellpg < 2 , "naked", "calcified"))
ggplotly(ggplot(data=PIC_newdata, aes(x=group, y=PICpercellpg)) + geom_boxplot()+geom_point(size=2) +theme_Publication())

PIC_newdata$group2 <- case_when(
  PIC_newdata$PICpercellpg <2  ~ "naked_bouyant",
  PIC_newdata$PICpercellpg >2 & PIC_newdata$PICpercellpg < 4 ~ "naked/calcified uncertain",
  PIC_newdata$PICpercellpg >4 & PIC_newdata$PICpercellpg < 10 ~ "moderately calcified",
  PIC_newdata$PICpercellpg >10 ~ "strongly calcified", 
  TRUE ~ as.character(PIC_newdata$PICpercellpg)
)

ggplotly(ggplot(data=PIC_newdata, aes(x=group2, y=PICpercellpg)) + geom_boxplot()+geom_point(size=2) +theme_Publication())

PIC_newdata$SinkVel.pred <- predict(PIC_reg, data.frame(PIC_newdata))
PIC_newdata_reg <- lm(SinkVel.pred~PICpercellpg, data=PIC_newdata) 
coef(PIC_newdata_reg)
#same coef as PIC_reg

plot(resid(PIC_newdata_reg))

ggplot(data=PIC_newdata, aes(x=PICpercellpg, y=SinkVel.pred)) +geom_point(size=2) +theme_Publication()

PIC_newdata$beta.pred <- predict(Beta_reg, data.frame(PIC_newdata))

PIC_newdata$E_DS_V.pred <- predict(E_DS_V_reg, data.frame(PIC_newdata))
PIC_newdata$E_DS_HV.pred <- predict(E_DS_HV_reg, data.frame(PIC_newdata))

ggplot(data=PIC_newdata, aes(x=PICpercellpg, y=E_DS_V.pred)) +geom_point(size=5, aes(color=group2))+geom_smooth() +theme_Publication() + scale_y_log10(breaks = breaks, label=scientific_10)

ggplot(data=PIC_newdata, aes(x=PICpercellpg, y=E_DS_HV.pred)) +geom_smooth() +theme_Publication() +
  scale_y_log10(breaks = breaks, label=scientific_10)

summary_DS_newdata_bygroup2 <- ddply(PIC_newdata, .(group2), summarize,  PICpercellpg=mean(PICpercellpg), SinkVel.pred=mean(SinkVel.pred),beta.prep= mean (beta.pred), E_DS_V.pred= mean(E_DS_V.pred), E_DS_HV.pred=mean(E_DS_HV.pred))


#turbulence - not final yet

#TURBULENCE
#disrate is cm2/s3

#make data frame
turb<- as.data.frame(10^(-8:-2))
turb <- rename (turb, c("10^(-8:-2)"="disrate"))

turb$Kol <- ((v^3/turb$disrate)^0.25)*100 #Kolmogorov length scale in cm
#everything is below 1 cm, use eqn 2 in TK 

#radius of naked and calcified
Reh_calc <- 2.3E-6 #in m
Reh_naked <- 1.8E-6 #in m

turb$beta_TK2_calc <- (4.2*pi*(turb$disrate^(0.5))*((Reh_calc+Rehv)^3))*1000000 #this has kinematic viscosity assumed and for less than kolmogorov scale

turb$beta_TK2_naked <- (4.2*pi*(turb$disrate^(0.5))*((Reh_naked+Rehv)^3))*1000000 #this has kinematic viscosity assumed and for less than kolmogorov scale

turb$beta_TK_calc <- (1.37*pi*(turb$disrate^(1/3))*((Reh_calc+Rehv)^(7/3)))*1000000 #this has kinematic viscosity assumed and for more than kolmogorov scale

turb$beta_TK_naked <- (1.37*pi*(turb$disrate^(1/3))*((Reh_naked+Rehv)^(7/3)))*1000000 #this has kinematic viscosity assumed and for more than kolmogorov scale

turb$beta_Burd_calc <- (1.3*((turb$disrate/v)^0.5)*((Reh_calc+Rehv)^3))*1000000 #in encounters cm3/s
turb$beta_Burd_naked <- (1.3*((turb$disrate/v)^0.5)*((Reh_naked+Rehv)^3))*1000000 #in encounters cm3/s

turb$beta_Heidi_calc <- (0.42*pi*((turb$disrate/(v*100^2))^0.5)*(((Reh_calc+Rehv)*100)^3))*86400


#check encounters

#use TK, in cm3 s
turb$E_turb_HV_naked <- (turb$beta_TK_naked*(10)^4*(10)^3)   #E calculated with Virus and Host (10:1 MOI)
turb$E_turb_V_naked <- (turb$beta_TK_naked*(10)^4) #E calculated with virus only

turb$E_turb_HV_calc <- (turb$beta_TK_calc*(10)^4*(10)^3)  #E calculated with Virus and Host (10:1 MOI)
turb$E_turb_V_calc <- (turb$beta_TK_calc*(10)^4) #E calculated with virus only

breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

ggplot(data = turb, aes(x = disrate, y = E_turb_HV_calc)) + geom_point(size =5) +
  scale_x_log10(breaks = breaks, label=scientific_10) +
  scale_y_log10(limits=c(1e-3, 1e0), label=scientific_10) +
  annotation_logticks()
  