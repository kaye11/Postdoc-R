## ------------------------------------------------------------------------
setwd("D:/R program")
source("inspack.R")
#values needed 

K= 1.38064852*(10)^-23 #m2 kg/ s2 K boltzmann constant
mu= 1.126*(10)^-3 #kg/m s dynamic viscosity in 18C
v= 1.099*(10)^-6 #m2/s kinematic viscosity in 18C
Reh_calc= 2.3E-6 #in m radius Ehux
Reh_naked= 1.8E-6 #in m radius Ehux
Reh_lith = 1.3E-6 #in m radius
Rehv= 90*(10)^-9 #in m radius virus
Temp = 18+273.15 #temp in kelvin, here assuming 18C
Den_OcM = 1.05 #g/cm3 density organic cell matter
Den_CH2O= 1.025 #g/cm3 density seawater at 18C
CcNum <- 0.9*((10)^3)
NcNum <- 0.1*((10)^3)
ViNum <- (CcNum+NcNum)*10
LiNum <- CcNum*10


## ------------------------------------------------------------------------
#Brownian motion (BM)
#1. make a data frame
BM <- data.frame (group=as.factor(c("Nc", "Cc", "Cc", "Li")), group2 =c("Nc", "Cc-Mc", "Cc-Oc", "Li"), rad= c(Reh_naked, Reh_calc, Reh_calc, Reh_lith)) 

#2. calculate beta (beta)
BM$beta_BM <- ((2*(K*(10)^4)*Temp*(((BM$rad+Rehv)*100)^2))/((3*mu*10)*(BM$rad*Rehv*1e4)))*86400 #cm3/d

# go back to this later
#3. calculate encounters (E)
#BM$E_BM <- BM$beta_BM*ViNum

BM


## ------------------------------------------------------------------------
#Differential settling (DS)
#1. read in PIC data
library(readr) #always use readr not baseR

setwd("D:/R program")
PIC <- read_csv("Postdoc-R/CSV Files/PIC.csv")

PIC$Strain <- as.factor(PIC$Strain)
PIC$Replicate <- as.factor(PIC$Replicate)

#2. calculate PIC for cell
PIC$PIC <- PIC$TC-PIC$AC
PIC$PICpercell <- (PIC$PIC/PIC$Cellcount)*(10)^-3#in g
PIC$PICpercellpg <- PIC$PICpercell*1e12

ggplotly(ggplot(data=PIC, aes(x=Strain, y=PICpercellpg)) + geom_boxplot()+geom_point(size=2) +theme_Publication())

#3a. calculate density of cells (den_cell)
PIC <- mutate(PIC, group = ifelse(PICpercellpg < 4 , "Nc", "Cc"))

ggplotly(ggplot(data=PIC, aes(x=group, y=PICpercellpg)) + geom_boxplot()+geom_point(size=2, aes(color=Strain))+ theme_Publication())

PIC <- mutate(PIC, rad = ifelse(group == "Nc" , 1.8E-6,  2.3E-6)) #in m

PIC$volume <- (4/3)*pi*(PIC$rad*100)^3 #in cm3
PIC$Den_cell <- PIC$PICpercell/PIC$volume #g/cm3
PIC$Den_celltotal <- PIC$Den_cell+Den_OcM

#remove strains 621, 623, 625
PIC <- PIC %>% filter (! Strain %in% c("621", "623", "655"))

ggplotly(ggplot(data=PIC, aes(x=Strain, y=Den_celltotal, color=group)) + geom_boxplot()+geom_point(size=2) 
         +theme_Publication())
#some strains that are "naked" have PIC<2. I chose to ignore this since in the lm model I do not use
#strain as a factor, rather data is treated as a whole (e.g., no grouping)



## ------------------------------------------------------------------------
#assume grouping
PIC$group2 <- case_when(
  PIC$PICpercellpg <2  ~ "Nc",
  PIC$PICpercellpg >2 & PIC$PICpercellpg < 4 ~ "Nc/Cc uncertain",
  PIC$PICpercellpg >4 & PIC$PICpercellpg < 10 ~ "Cc-Mc",
  PIC$PICpercellpg >10 ~ "Cc-Oc", 
  TRUE ~ as.character(PIC$PICpercellpg)
)

PIC$group2 <- factor (PIC$group2,levels= c("Nc", "Nc/Cc uncertain", "Cc-Mc", "Cc-Oc"),
                      labels =  c("Nc", "Nc/Cc uncertain", "Cc-Mc", "Cc-Oc"))

#remove Nc/Cc uncertain
PIC <- PIC %>% filter (! group2 %in% c("Nc/Cc uncertain"))


#3b. calculate PIC and density for lith
PIC$Denlith <- case_when(
  PIC$group2 == "Nc"  ~ 1.025, 
  PIC$group2 == "Cc-Mc" ~ 2.6,
  PIC$group2 == "Cc-Oc" ~ 2.6)

ggplotly(ggplot(data=PIC, aes(x=group, y=Denlith, color=group)) + geom_boxplot()+geom_point(size=2) 
         +theme_Publication())



## ------------------------------------------------------------------------
#4. calculate sinking velocity of cells,liths, and viruses
PIC$SinkVel <- ((2*((PIC$rad*100)^2)*(981)*(PIC$Den_celltotal-Den_CH2O))/(9*(mu*10)))*864 #meter per day
PIC$SinkVel_lith <- ((2*((Reh_lith*100)^2)*(981)*(PIC$Denlith-Den_CH2O))/(9*(mu*10)))*864 #meter per day
Ehv_SinkVel <- ((2*((Rehv*100)^2)*(981)*(Den_CH2O-Den_CH2O))/(9*(mu*10)))*864  #equals to 0
#g is converted to per day, 864 is the one that converts cm/s to m/day
#plot sinking velocity vs calcification

#for paper
resize.win(6,6)
ggplot(data=PIC, aes(x=PICpercellpg, y=SinkVel, color=Strain, shape=group2)) + geom_point(size=5)+theme_Publication2()+
  labs(y = expression("sinking velocity "~("m"~day^-1)), x = expression("PIC pg"~cell^-1)) + 
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) 


## ------------------------------------------------------------------------
#5. calculate betas for DS
PIC$beta_DS <-(pi*(((PIC$rad+Rehv)*100)^2)*(abs((PIC$SinkVel-Ehv_SinkVel)/864)))*86400 #in encounters cm3/day
PIC$beta_DS_lith <- (pi*(((Reh_lith+Rehv)*100)^2)*(abs((PIC$SinkVel_lith-Ehv_SinkVel)/864)))*86400 #in encounters cm3/day

ggplot(data=PIC, aes(x=PICpercellpg, y=beta_DS, color=Strain,  shape=group2)) + geom_point(size=5)+theme_Publication()+
  labs(y = expression(beta~("Encounters"~cm^3~day^-1)), x = expression("PIC pg"~cell^-1))  +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=2),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) + annotation_logticks(sides="l")+
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) 

## ------------------------------------------------------------------------
#6. calculate encounters
PIC$E_DS <- (PIC$beta_DS*ViNum) #E calculated with Virus for cell
PIC$E_DS_lith <- (PIC$beta_DS_lith*ViNum) #E calculated with Virus for lith

ggplotly(ggplot(data=PIC, aes(x=PICpercellpg, y=log10(E_DS))) +geom_point(size=5, aes(shape=group2, color=Strain)) +
           theme_Publication() + geom_smooth())

ggplot(data=PIC, aes(x=PICpercellpg, y=E_DS, color=Strain,  shape=group2)) + geom_point(size=5)+theme_Publication()+
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=2),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) + annotation_logticks(sides="l") +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) +
  labs(y = expression("viral encounters " ~ day^-1~cell^-1), x = expression("PIC pg"~cell^-1))


## ------------------------------------------------------------------------
#7. summaries
library(plyr)
summary_DS <- ddply(PIC, .(Strain), summarize,  PICpercellpg=mean(PICpercellpg),
                    Den_celltotal = mean (Den_celltotal),SinkVel=mean(SinkVel),beta_DS=mean(beta_DS), E_DS= mean(E_DS),
                    Denlith = mean (Denlith), SinkVel_lith=mean (SinkVel_lith), 
                    beta_DS_lith=mean (beta_DS_lith), E_DS_lith=mean (E_DS_lith))
summary_DS_bygroup2 <- ddply(PIC, .(group2), summarize,  PICpercellpg=mean(PICpercellpg), 
                             Den_celltotal = mean (Den_celltotal), SinkVel=mean(SinkVel),beta_DS=mean(beta_DS), E_DS= mean(E_DS),
                             Denlith = mean (Denlith), SinkVel_lith=mean (SinkVel_lith), 
                             beta_DS_lith=mean (beta_DS_lith), E_DS_lith=mean (E_DS_lith))

summary_DS_bygroup <- ddply(PIC, .(group), 
                            summarize,  PICpercellpg=mean(PICpercellpg), 
                            Den_celltotal = mean (Den_celltotal), SinkVel=mean(SinkVel),beta_DS=mean(beta_DS), E_DS= mean(E_DS),
                            Denlith = mean (Denlith), SinkVel_lith=mean (SinkVel_lith), 
                            beta_DS_lith=mean (beta_DS_lith), E_DS_lith=mean (E_DS_lith))
summary_DS
summary_DS_bygroup2
summary_DS_bygroup
setwd("D:/R program")
require(openxlsx)
write.xlsx(summary_DS, file = "Postdoc-R/Exported Tables/summary_DS_master5.xlsx")
write.xlsx(summary_DS_bygroup, file = "Postdoc-R/Exported Tables/summary_DS_bygroup_master5.xlsx")
write.xlsx(summary_DS_bygroup2, file = "Postdoc-R/Exported Tables/summary_DS_bygroup2_master5.xlsx")


## ------------------------------------------------------------------------
#8. regressions
#8a. PIC and beta_DS of cells 
PIC_beta_reg <- lm (beta_DS ~ PICpercellpg, data=PIC)
summary(PIC_beta_reg)
plot(residuals.lm(PIC_beta_reg))
coef(PIC_beta_reg)
cor(PIC$PICpercellpg, PIC$beta_DS)

#8d other regressions, change this
PIC_Den_celltotal_reg <- lm(Den_celltotal ~ PICpercellpg, data=PIC)
PIC_SinkVel_reg <- lm (SinkVel ~ PICpercellpg, data=PIC)
cor(PIC$PICpercellpg, PIC$SinkVel)

plot(residuals.lm(PIC_Den_celltotal_reg))
plot(residuals.lm(PIC_SinkVel_reg))



## ------------------------------------------------------------------------
#9. make a prediction based on PIC values
#9a. for cells
require(truncnorm)
require(Rmisc)
summary(PIC$PICpercellpg)
summarySE(data=PIC, measurevar="PICpercellpg")

#make new dataframe
PIC_newdata <- as.data.frame(rtruncnorm(n=10000, a=-0.4, b=20.14, mean=6.09, sd=6.83))
#rename column. rename function in plyr 
library(plyr)
PIC_newdata <- plyr::rename (PIC_newdata, c ("rtruncnorm(n = 10000, a = -0.4, b = 20.14, mean = 6.09, sd = 6.83)" = "PICpercellpg"))
PIC_newdata <- mutate(PIC_newdata, group = ifelse(PICpercellpg < 4 , "Nc", "Cc"))
ggplotly(ggplot(data=PIC_newdata, aes(x=group, y=PICpercellpg)) + geom_boxplot()+geom_point(size=2) +
           theme_Publication())


## ------------------------------------------------------------------------
PIC_newdata$group2 <- case_when(
  PIC_newdata$PICpercellpg <2  ~ "Nc",
  PIC_newdata$PICpercellpg >2 & PIC_newdata$PICpercellpg < 4 ~ "Nc/Cc uncertain",
  PIC_newdata$PICpercellpg >4 & PIC_newdata$PICpercellpg < 10 ~ "Cc-Mc",
  PIC_newdata$PICpercellpg >10 ~ "Cc-Oc", 
  TRUE ~ as.character(PIC_newdata$PICpercellpg)
)

PIC_newdata$group2 <- factor (PIC_newdata$group2,levels= c("Nc", "Nc/Cc uncertain", "Cc-Mc", "Cc-Oc"), labels =  c("Nc", "Nc/Cc uncertain", "Cc-Mc", "Cc-Oc"))

#remove Nc/Cc uncertain
PIC_newdata <- PIC_newdata %>% filter (! group2 %in% c("Nc/Cc uncertain"))

#calculate new stuff

PIC_newdata <- mutate(PIC_newdata, rad = ifelse(group == "Nc" , 1.8E-6,  2.3E-6)) #in m

PIC_newdata$volume <- (4/3)*pi*(PIC_newdata$rad*100)^3 #in cm3
PIC_newdata$Den_cell <- (PIC_newdata$PICpercell*1e-12)/PIC_newdata$volume #g/cm3
PIC_newdata$Den_celltotal <- PIC_newdata$Den_cell+Den_OcM

ggplotly(ggplot(data=PIC, aes(x=Strain, y=Den_celltotal, color=group)) + geom_boxplot()+geom_point(size=2) 
         +theme_Publication())


#3b. calculate PIC and density for lith
PIC_newdata$Denlith <- case_when(
  PIC_newdata$group2 == "Nc"  ~ 1.025, 
  PIC_newdata$group2 == "Nc/Cc uncertain" ~ 1.025,
  PIC_newdata$group2 == "Cc-Mc" ~ 2.6,
  PIC_newdata$group2 == "Cc-Oc" ~ 2.6)

ggplotly(ggplot(data=PIC_newdata, aes(x=group, y=Denlith, color=group)) + geom_boxplot()+geom_point(size=2) 
         +theme_Publication())

#4. calculate sinking velocity of cells,liths, and viruses
PIC_newdata$SinkVel <- ((2*((PIC_newdata$rad*100)^2)*(981)*(PIC_newdata$Den_celltotal-Den_CH2O))/(9*(mu*10)))*864 #meter per day
PIC_newdata$SinkVel_lith <- ((2*((Reh_lith*100)^2)*(981)*(PIC_newdata$Denlith-Den_CH2O))/(9*(mu*10)))*864 #meter per day
Ehv_SinkVel <- ((2*((Rehv*100)^2)*(981)*(Den_CH2O-Den_CH2O))/(9*(mu*10)))*864  #equals to 0
#g is converted to per day, 864 is the one that converts cm/s to m/day
#plot sinking velocity vs calcification


ggplot(data=PIC_newdata, aes(x=PICpercellpg, y=SinkVel, color=group2, shape=group2)) + geom_point(size=5)+theme_Publication()+
  labs(y = expression("Sinking velocity"~("m"~day^-1)), x = expression("PIC pg"~cell^-1)) +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) 

## ------------------------------------------------------------------------

#5. calculate betas for DS
PIC_newdata$beta_DS <-(pi*(((PIC_newdata$rad+Rehv)*100)^2)*(abs((PIC_newdata$SinkVel-Ehv_SinkVel)/864)))*86400 #in encounters cm3/day
PIC_newdata$beta_DS_lith <- (pi*(((Reh_lith+Rehv)*100)^2)*(abs((PIC_newdata$SinkVel_lith-Ehv_SinkVel)/864)))*86400 #in encounters cm3/day

ggplot(data=PIC_newdata %>% filter (!(group2 %in% c("Nc/Cc uncertain"))), aes(x=PICpercellpg , y=beta_DS, color=group2,  shape=group2)) + geom_point(size=5)+theme_Publication()+
  labs(y = expression(beta~("Encounters"~cm^3~day^-1)), x = expression("PIC pg"~cell^-1))  +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=2),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) + annotation_logticks(sides="l")+
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) 


#6. calculate encounters
PIC_newdata$E_DS <- (PIC_newdata$beta_DS*ViNum) #E calculated with Virus for cell
PIC_newdata$E_DS_lith <- (PIC_newdata$beta_DS_lith*ViNum) #E calculated with Virus for lith

ggplotly(ggplot(data=PIC_newdata, aes(x=PICpercellpg, y=log10(E_DS))) +geom_point(size=5, aes(shape=group2, color=group2)) +
           theme_Publication() + geom_smooth())
#remove strains 621, 623, 625

ggplot(data=PIC_newdata , aes(x=PICpercellpg, y=E_DS, color=group2,  shape=group2)) + geom_point(size=5)+theme_Publication()+
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=2),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  annotation_logticks(sides="l") + theme_Publication2() + geom_smooth(aes(group=1), color="black", method="loess") +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) +
  labs(y = expression("viral encounters " ~ day^-1~cell^-1), x = expression("PIC pg"~cell^-1)) 


## ------------------------------------------------------------------------
#7. calculate encounters based on newdata

summary_DS_bygroup<-ddply(PIC %>% filter (!(group2 %in% c("naked/calcified uncertain"))), .(group), summarize,  PICpercellpg=mean(PICpercellpg), Den_celltotal = mean (Den_celltotal),
                          SinkVel=mean(SinkVel),beta_DS=mean(beta_DS), E_DS= mean(E_DS), 
                          Denlith = mean (Denlith), SinkVel_lith=mean (SinkVel_lith), 
                          beta_DS_lith=mean (beta_DS_lith), E_DS_lith=mean (E_DS_lith))
summary_DS_bygroup

#remove naked/calcified uncertain

summary_DS_bygroup_pred <-ddply(PIC_newdata, .(group), summarize,  PICpercellpg=mean(PICpercellpg), 
                                Den_celltotal = mean (Den_celltotal),
                                SinkVel=mean(SinkVel),beta_DS=mean(beta_DS), E_DS= mean(E_DS), 
                                Denlith = mean (Denlith), SinkVel_lith=mean (SinkVel_lith), 
                                beta_DS_lith=mean (beta_DS_lith), E_DS_lith=mean (E_DS_lith))
summary_DS_bygroup_pred

summary_DS_bygroup2_pred <-ddply(PIC_newdata, .(group2), summarize,  PICpercellpg=mean(PICpercellpg), 
                                 Den_celltotal = mean (Den_celltotal),
                                 SinkVel=mean(SinkVel),beta_DS=mean(beta_DS), E_DS= mean(E_DS), 
                                 Denlith = mean (Denlith), SinkVel_lith=mean (SinkVel_lith), 
                                 beta_DS_lith=mean (beta_DS_lith), E_DS_lith=mean (E_DS_lith))
summary_DS_bygroup2_pred

summarySE (PIC_newdata %>% filter (!(group2 %in% c("naked/calcified uncertain"))), 
           measurevar = "PICpercellpg", groupvars = c("group"))


setwd("D:/R program")
write.xlsx(summary_DS_bygroup_pred, file = "Postdoc-R/Exported Tables/summary_DS_bygroup_pred_master4.xlsx")


## ------------------------------------------------------------------------
#arrange the summary
DS_pred <- data.frame (group=as.factor(c("Nc", "Cc", "Cc", "Li")), group2 =c("Nc", "Cc-Mc", "Cc-Oc", "Li"), PIC = c(0.8423303, 6.9241984,  13.5265157, NA), Den = c(1.084481, 1.185862, 1.315408, 2.6), SinkVel = c(0.03223688, 0.14234442, 0.25697799, 0.4452451), beta_DS = c(3.617650e-07, 2.554384e-06, 4.611493e-06, 2.702580e-06))

DS_pred


## ------------------------------------------------------------------------
#Turbulence
#1. make new dataframe
#disrate is cm2/s3
#make data frame
turb <- expand.grid(list (disrate = (c (1 %o% 10^(seq(-8,-2, 0.5)))), group2 =c("Nc", "Cc-Mc", "Cc-Oc", "Li")))
turb$group <- factor (turb$group,levels= c("Nc", "Cc-Mc", "Cc-Oc", "Li"), labels = c("Nc", "Cc", "Cc", "Li"))

turb$rad <- case_when(
  turb$group =="Nc" ~ 1.8E-6,
  turb$group =="Cc" ~ 2.3E-6,
  turb$group =="Li" ~ 1.3E-6,
  TRUE ~ as.numeric(turb$group)
)
turb$disrate <- as.numeric(as.character(turb$disrate))

#2. calculate beta and encounters
turb$beta_turb <- (4.2*pi*((turb$disrate/(v*100^2))^0.5)*(((turb$rad+Rehv)*100)^3))*86400 
turb$E_turb <- (turb$beta_turb*ViNum) #E calculated with virus only

turb$group <- factor (turb$group,levels= c("Nc", "Cc", "Li"), labels = c("Nc", "Cc", "Li"))

ggplot(data = turb, aes(x = disrate, y = beta_turb, color=group)) + geom_point(size =5) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=4),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=4),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks() +
  theme_Publication() +
  labs(y = expression(beta~("predicted encounters " ~cm^3~day^-1)), 
       x = expression("dissipation rate "~(m^2~s^-3))) +
  theme(legend.title = element_blank())

ggplot(data = turb, aes(x = disrate, y = E_turb, color=group)) + geom_point(size =5) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=4),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=7),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks()+
  theme_Publication()+
  labs(y = expression("viral encounters " ~ cm^-3~day^-1),x = expression("dissipation rate "~(m^2~s^-3))) +
  theme(legend.title = element_blank())


## ------------------------------------------------------------------------
all <- Reduce(function(x,y) merge(x,y,by="group2",all=TRUE) ,
              list(BM %>% select (group2, beta_BM), DS_pred %>% select (group2, beta_DS), 
                   turb %>% select (group2, disrate, beta_turb)))
all$beta_all <- all$beta_BM + all$beta_DS + all$beta_turb
all$E_all_low <- all$beta_all*(10^4)
all$E_all_high <- all$beta_all*(10^6)
all$group <- factor (all$group,levels= c("Nc", "Cc-Mc", "Cc-Oc", "Li"), labels = c("Nc", "Cc", "Cc", "Li"))
#with virus and reservoir
all$E_all_low_resvi <- all$beta_all*(10^4)*(10^3)
all$E_all_high_resvi <- all$beta_all*(10^6)*(10^5)

#make summary so that you can have CI


#plotting
resize.win(8,5)
ggplot(data=all, aes(x=disrate,y = E_all_low , color=group, fill=group)) + 
  geom_smooth(size=1, position=position_jitter(w=0.02, h=0), aes(linetype="field", fill=group))+
  geom_smooth(data = all, aes(y= E_all_high, color=group,fill=group, linetype="lab")) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=2),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=7),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks()+
  theme_Publication2() +
  theme(legend.title = element_blank(), legend.key.width=unit(1,"cm"))+
  labs(y = expression("viral encounters " ~day^-1~cell^-1), x = expression("dissipation rate "~(m^2~s^-3))) +
  theme(legend.title = element_blank()) 

