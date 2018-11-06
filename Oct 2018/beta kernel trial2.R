
#always set this as wd or your source files wouldn't run
setwd("D:/R program/")

####FOR EXPONENTS USE (n)^n RATHER THAN E
#just for trial, shift completed script chunks to the Rnotebook but make initial scripts here 

##calculating beta

#values needed 

K= 1.38064852*(10)^-23 #m2 kg/ s2 K boltzmann constant
mu= 1.126*(10)^-3 #kg/m s dynamic viscosity in 18C
v= 1.099*(10)^-6 #m2/s kinematic viscosity in 18C
Reh= 2E-6 #in m radius Ehux
Rehv= 90E-9 #in m radius virus
Temp = 18+273.15 #temp in kelvin, here assuming 18C
Den_OcM = 1.05 #g/cm3 density organic cell matter
Den_CH2O= 1.025 #g/cm3 density seawater at 18C
Eh_vol = ((4/3)*pi*(Reh)^3)*1000000 #in cm3


#CJ is CJ, TK is Kiorboe, Burd is Burd (formula source)

#BROWNIAN MOTION
BM_Beta_Burd <- ((2/3)*(K*Temp/mu)*((Reh+Rehv)^2/(Reh*Rehv)))*(10)^4 #in cm2/s  diffusivity for both partners
D_CJ_virus <- ((K*Temp)/(3*pi*mu*Rehv*2))*(10)^4 #in cm2/s diffusity for a spherical virus
D_CJ_host <- ((K*Temp)/(3*pi*mu*Reh*2))*(10)^4 #in cm2/s diffusity for a spherical host

D_TK_virus <- ((K*Temp)/(6*pi*mu*Rehv))*(10)^4 #in cm2/s diffusity for a spherical virus
D_TK_host <- ((K*Temp)/(6*pi*mu*Reh))*(10)^4 #in cm2/s diffusity for a spherical host

#TK and CJ have same formula

Beta_BM_TK <- ((4*pi)*(D_TK_host+D_TK_virus)*((Reh+Rehv)*(10)^2)) #Brownian motion TK formula, in cm3/s

#encounters are in encounters /cm3 s then converted to encounters/cm3 day
E_BM_TK_HV <- (Beta_BM_TK*(10)^4*(10)^3)/86400 #E calculated with Virus and Host (10:1 MOI)
E_BM_TK_V <- (Beta_BM_TK*(10)^4)/86400 #E calculated with Virus 
E_BM_Burd_HV <- (BM_Beta_Burd*(10)^4*(10)^3)/86400  #E calculated with Virus and Host (10:1 MOI)
E_BM_Burd_V <- (BM_Beta_Burd*(10)^4)/86400  #E calculated with Virus

#converting it produced different results with HF

#DIFFERENTIAL SETTLING

##calculate cell density
#read in PIC data
library(readr) #always use readr not baseR

PIC <- read_csv("Postdoc/CSV Files/PIC.csv")

PIC$Strain <- as.factor(PIC$Strain)
PIC$Replicate <- as.factor(PIC$Replicate)

#certain changes in data.table API made calculating inside the list data.table to not work
#defining new variables and using that new variable for calculating another variable does not work anymore huhu sad
#FYI took me one hour to figure it out. great! so now just do it the old fashioned way

PIC$PIC <- PIC$TC-PIC$AC
PIC$PICpercell <- (PIC$PIC/PIC$Cellcount)*(10)^-3#in g
PIC$PICpercellpg <- PIC$PICpercell*1e12

require (ggplot2)
require(plotly)
require(grid)
require(ggthemes)
source ("theme_Publication.R")
source("resizewin.R")

ggplotly(ggplot(data=PIC, aes(x=Strain, y=PICpercellpg)) + geom_boxplot()+geom_point(size=2) +theme_Publication())

require (dplyr)
PIC <- mutate(PIC, group = ifelse(PICpercellpg < 1 , "naked", "calcified"))

ggplotly(ggplot(data=PIC, aes(x=group, y=PICpercellpg)) + geom_boxplot()+geom_point(size=2) +theme_Publication())

PIC <- mutate(PIC, rad = ifelse(group == "naked" ,  2E-6,  2.3E-6)) #in m

PIC$volume <- (4/3)*pi*(PIC$rad*100)^3 #in cm3
PIC$Den_cell <- PIC$PICpercell/PIC$volume #g/m3
PIC$Den_celltotal <- PIC$Den_cell+Den_OcM

ggplotly(ggplot(data=PIC, aes(x=Strain, y=Den_celltotal, color=group)) + geom_boxplot()+geom_point(size=2) 
         +theme_Publication())

#PIC <- mutate(PIC, group2 = ifelse(Den_celltotal < 1.025 , "bouyant", "sink")) ---> not a good grouping variable

#use Stokes equation to calculate sinking velocity
#Sinkvel_Paasche <- ((2*(2E-4)*981*(1.19-1.027))/9*(mu*10))*864 #meter per day
#Stokes calculator use a different formula, see notes on thinking notebook

#Now run for all cells
#Same formula used by Burd and TK
PIC$SinkVel <- ((2*((PIC$rad)^2)*(9.8*(86400)^2)*(PIC$Den_celltotal-Den_CH2O))/9*(mu*10))*864 #meter per day

#plot sinking velocity vs calcification

resize.win(12,9)
grid.newpage()
#text <- element_text(size = 20, color="black") #change the size of the axes

ggplot(data=PIC, aes(x=PICpercellpg, y=SinkVel, color=Strain)) + geom_point(size=5)+theme_Publication()+
  labs(y = expression("Sinking velocity"~("m"~day^-1)), x = expression("PIC"~cell^-1)) 

PIC_reg <- lm(SinkVel~PICpercellpg, data=PIC) #essentially perfect fit: summary may be unreliable haha
summary(PIC_reg)
plot(residuals.lm(PIC_reg))
layout(matrix(1:4,2,2))
plot(PIC_reg)

cor(PIC$PICpercellpg, PIC$SinkVel)
#cor=0.9997279

##make new dataframe depending on experimental PIC values
a <- sample (PIC$PICpercellpg) #just makes the same length as your df
b <- sample (PIC$PICpercellpg)
c <- sample (PIC$PICpercellpg)

new_PIC = as.data.frame (list(a, b, c))
#how to merge the lists


#compute beta kernels
#assume density of a viral particle is same as seawater

Ehv_SinkVel <- ((2*((2E-6)^2)*(9.8*(86400)^2)*(Den_CH2O-Den_CH2O))/9*(mu*10))*864 #equals to 0

#beta_Paasche <- (pi*(Reh+Rehv)^2*abs(Sinkvel_Paasche-Ehv_SinkVel))*1000000 #in encounters cm3/s

PIC$beta <- (pi*(Reh+Rehv)^2*abs(PIC$SinkVel-Ehv_SinkVel))*1000000 #in encounters cm3/s

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

Sinkvelbeta.plot<- ggplot(data=PIC, aes(x=SinkVel, y=beta, color=Strain)) + geom_point(size=5)+theme_Publication()+
  labs(x = expression("Sinking velocity"~("m"~day^-1)), y = expression(beta~("Encounters"~cm^3~s^-1))) +
  scale_y_continuous(label=scientific_10) #+stat_smooth(method="lm", aes(group=1))

Sinkvelbeta.plot

require(plotly)

ggplotly(Sinkvelbeta.plot)

ggplotly(ggplot(data=PIC, aes(x=Strain, y=SinkVel)) + geom_boxplot()+theme_Publication())

ggplot(data=PIC, aes(x=PICpercellpg, y=beta, color=Strain)) + geom_point(size=5)+theme_Publication()+
  labs(y = expression(beta~("Encounters"~cm^3~s^-1)), x = expression("PIC"~cell^-1))  +
  scale_y_continuous(label=scientific_10)

Beta_reg <- lm(beta~SinkVel, data=PIC)
layout(matrix(1:4,2,2))
plot(Beta_reg)



require (Rmisc)

#sinkvel.sum <- summarySE (data=PIC, measurevar="SinkVel", groupvars="Strain")
#pic.sum <-  summarySE(data=PIC, measurevar="PICpercellpg", groupvars="Strain")
#beta.sum <- summarySE(data=PIC, measurevar="beta", groupvars="Strain")

#encounters are in encounters /cm3 s then converted to encounters/cm3 day
E_Paache_DS_V <- (beta_Paasche*(10)^4)/86400
PIC$E_DS_HV <- (PIC$beta*(10)^4*(10)^3)/86400  #E calculated with Virus and Host (10:1 MOI)
PIC$E_DS_V <- (PIC$beta*(10)^4)/86400  #E calculated with Virus

#encounter.sum <- summarySE(data=PIC, measurevar="E_DS_virus", groupvars="Strain") 
#encounter.sum_V <- summarySE(data=PIC, measurevar="E_DS", groupvars="Strain")

ggplotly(ggplot(data=PIC, aes(x=Strain, y=E_DS_V)) + geom_boxplot()+theme_Publication()+
           scale_y_continuous(label=scientific_10))

ggplot(data=PIC, aes(x=SinkVel, y=E_DS_V, color=Strain, shape=group)) + geom_point(size=5)+theme_Publication()+
  scale_y_continuous(label=scientific_10)

summary_DS <- ddply(PIC, .(Strain), summarize,  PICpercellpg=mean(PICpercellpg), Den_celltotal = mean (Den_celltotal),
                    SinkVel=mean(SinkVel),beta=mean(beta), E_DS_V= mean(E_DS_V), E_DS_HV=mean(E_DS_HV))

require(openxlsx)
openxlsx::write.xlsx(summary_DS, file = "Postdoc/Exported Tables/summary_DS.xlsx")

ggplot(data=summary_DS, aes(x=SinkVel, y=E_DS_V, color=Strain)) + geom_point(size=5)+theme_Publication()+
  labs(y = expression(E~("Encounters"~cm^-3~s^-1)), x =  expression("Sinking velocity"~("m"~day^-1))) +
  scale_y_continuous(label=scientific_10)

ggplotly (ggplot(data=summary_DS, aes(x=SinkVel, y=E_DS_V, color=Strain)) + geom_point(size=5)+theme_Publication()+
            labs(y = expression(E~("Encounters"~cm^-3~s^-1)), x =  expression("Sinking velocity"~("m"~day^-1))) +
            scale_y_continuous(label=scientific_10))

#one 374n has higher encounter rate and beta kernel because of its negative density 
#formula for beta kernel reports abs values, check this more closely later 
#TO DOOOOOO!

#make a prediction based on PIC values



#TURBULENCE

turb<- as.data.frame(10^(-8:-3)) #seq dont work for numbers with super(powers) lol

turb$disrate <- turb$`10^(-8:-3)`

turb$beta_TK <- (1.37*pi*(turb$disrate^(1/3))*((Reh+Rehv)^(7/3)))*1000000 #this has kinematic viscosity assumed and for more than kolmogorov scale

turb$beta_TK_long <- (1.37*pi*(turb$disrate^(1/3))*((Reh+Rehv)^(1/3))*(Reh+Rehv)^2)*1000000 #this has kinematic viscosity assumed and for more than kolmogorov scale long version

#same results beta_TK and beta_TK_long

turb$beta_TK2 <- (4.2*pi*(turb$disrate^(0.5))*((Reh+Rehv)^3))*1000000 #this has kinematic viscosity assumed and for less than kolmogorov scale

turb$beta_TK_book <- (1.3*(turb$disrate^(0.5))*((Reh+Rehv)^3)*((7.5*(Reh+Rehv)^2)/(1+(2*(Reh/Rehv)))^2))*1000000 

#this has kinematic viscosity assumed and for more than 1 cm

turb$beta_Burd <- (1.3*((turb$disrate/v)^0.5)*((Reh+Rehv)^3))*1000000 #in encounters cm3/s

#check Kolmogorov scale for each disrate

turb$Kol <- ((v^3/turb$disrate)^0.25)*1000 #Kolmogorov length scale in cm

turb$E_turb_HV <- turb$beta_TK*(10)^4*(10)^3  #E calculated with Virus and Host (10:1 MOI)
turb$E_turb_V <- turb$beta_TK*(10)^4 #E calculated with virus only

turb$shear_beta <- (4/3*(turb$disrate/v)^0.5*(Reh+Rehv)^3)
                    