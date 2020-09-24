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
Den_calcite <- 2.6 #in g/cm3 as Jakob et al
CcNum <- 1*((10)^3)
NcNum <- 0.1*((10)^3)
ViNum <- (CcNum)*30 #NAVICE
LiNum <- (CcNum)*25 #satellite data Toby


## ------------------------------------------------------------------------
#Brownian motion (BM)
#1. make a data frame
BM <- data.frame (group=as.factor(c("Nc", "Cc", "Li")), rad= c(Reh_naked, Reh_calc, Reh_lith)) 

#2. calculate beta (beta)
BM$beta_BM <- ((2*(K*(10)^7)*Temp*(((BM$rad+Rehv)*100)^2))/((3*mu*10)*(BM$rad*Rehv*1e4)))*86400 #cm3/d
BM$beta_BM2 <- ((2*(K*Temp*((BM$rad+Rehv)^2)))/(3*mu*(BM$rad*Rehv)))*86400*10^6 #cm3/d
#same

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
PIC <- mutate(PIC, PICpercell = ifelse(PICpercell<0, 0,  PICpercell)) #in m
PIC$PICpercellpg <- PIC$PICpercell*1e12


ggplot(data=PIC, aes(x=Strain, y=PICpercellpg)) + geom_boxplot()+geom_point(size=2) +theme_Publication()

#3a. calculate density of cells (den_cell)
PIC <- mutate(PIC, group = ifelse(PICpercellpg < 4 , "Nc", "Cc"))

ggplot(data=PIC, aes(x=Strain, y=PICpercellpg, color=group)) + geom_boxplot()+geom_point(size=2)+ 
  theme_Publication()

PIC <- mutate(PIC, rad = ifelse(group == "Nc" , Reh_naked,  Reh_calc)) #in m

####calculate variables for  calcite
#mass is PICpercell
#density is Den_calcite
#calculate volume
PIC$volume_calc <- PIC$PICpercell/Den_calcite #in cm3

####calculate variables for OM
#density is Den_OCM
#calculate volume
PIC$volume_OM <- (4/3)*pi*(PIC$rad*100)^3 #rad in cm
#calculate mass
PIC$mass_OM <- Den_OcM*(PIC$volume_OM) #in g

#calculate density
PIC$Den_celltotal <- (PIC$PICpercell+PIC$mass_OM)/(PIC$volume_calc+PIC$volume_OM)

#remove strains 621, 623, 655
PIC <- PIC %>% filter (! Strain %in% c("629", "639", "655"))

ggplot(data=PIC, aes(x=Strain, y=Den_celltotal, color=group)) + geom_boxplot()+geom_point(size=2) + theme_Publication()

PIC$group <- as.factor(PIC$group)

## ------------------------------------------------------------------------

#3b. calculate PIC and density for lith
PIC$Denlith <- case_when(
  PIC$group == "Nc"  ~ 1.025, 
  PIC$group == "Cc" ~ 2.6)

## ------------------------------------------------------------------------
#4. calculate sinking velocity of cells,liths, and viruses
PIC$SinkVel <- ((2*((PIC$rad*100)^2)*(981)*(PIC$Den_celltotal-Den_CH2O))/(9*(mu*10)))*864 #meter per day
PIC$SinkVel_lith <- ((2*((Reh_lith*100)^2)*(981)*(PIC$Denlith-Den_CH2O))/(9*(mu*10)))*864 #meter per day
Ehv_SinkVel <- ((2*((Rehv*100)^2)*(981)*(Den_CH2O-Den_CH2O))/(9*(mu*10)))*864  #equals to 0
#g is converted to per day, 864 is the one that converts cm/s to m/day
#plot sinking velocity vs calcification

#for paper
resize.win(6,6)
ggplot(data=PIC, aes(x=PICpercellpg, y=SinkVel, color=Strain, shape=group)) + geom_point(size=5)+theme_Publication2()+
  labs(y = expression("sinking velocity "~("m"~day^-1)), x = expression("PIC pg"~cell^-1)) + 
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) 

## ------------------------------------------------------------------------
#5. calculate betas for DS
PIC$beta_DS <-(pi*(((PIC$rad+Rehv)*100)^2)*(abs((PIC$SinkVel-Ehv_SinkVel)/864)))*86400 #in encounters cm3/day
PIC$beta_DS2 <-(pi*((PIC$rad+Rehv)^2)*(abs((PIC$SinkVel-Ehv_SinkVel))))*10^6 #in encounters cm3/day
#same

PIC$beta_DS_lith <- (pi*(((Reh_lith+Rehv)*100)^2)*(abs((PIC$SinkVel_lith-Ehv_SinkVel)/864)))*86400 #in encounters cm3/day

ggplot(data=PIC, aes(x=PICpercellpg, y=beta_DS, color=Strain,  shape=group)) + geom_point(size=5)+theme_Publication()+
  labs(y = expression(beta~("Encounters"~cm^3~day^-1)), x = expression("PIC pg"~cell^-1))  +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=2),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) + annotation_logticks(sides="l")+
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) 

## ------------------------------------------------------------------------
#6. calculate encounters
PIC$E_DS <- (PIC$beta_DS*ViNum) #E calculated with Virus for cell
PIC$E_DS_lith <- (PIC$beta_DS_lith*ViNum) #E calculated with Virus for lith

ggplot(data=PIC, aes(x=PICpercellpg, y=log10(E_DS))) +geom_point(size=5, aes(shape=group, color=Strain)) +
           theme_Publication() + geom_smooth()

ggplot(data=PIC, aes(x=PICpercellpg, y=E_DS, color=Strain,  shape=group)) + geom_point(size=5)+theme_Publication()+
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
summary_DS_bygroup <- ddply(PIC, .(group), summarize,  PICpercellpg=mean(PICpercellpg), 
                             Den_celltotal = mean (Den_celltotal), SinkVel=mean(SinkVel),beta_DS=mean(beta_DS), E_DS= mean(E_DS),
                             Denlith = mean (Denlith), SinkVel_lith=mean (SinkVel_lith), 
                             beta_DS_lith=mean (beta_DS_lith), E_DS_lith=mean (E_DS_lith))
summary_DS
summary_DS_bygroup
setwd("D:/R program")
require(openxlsx)
write.xlsx(summary_DS, file = "Postdoc-R/Exported Tables/summary_DS_master6.xlsx")
write.xlsx(summary_DS_bygroup, file = "Postdoc-R/Exported Tables/summary_DS_bygroup_master6.xlsx")


## ------------------------------------------------------------------------
#8. make a prediction based on PIC values
#8a. for cells
#make equally spaced numbers for PIC
PIC_newdata <- as.data.frame(seq(0, 21, 0.1))

#rename column. rename function in plyr 
library(plyr)
PIC_newdata <- plyr::rename (PIC_newdata, c ("seq(0, 21, 0.1)" = "PICpercellpg"))
PIC_newdata <- mutate(PIC_newdata, group = ifelse(PICpercellpg < 4 , "Nc", "Cc"))
ggplot(data=PIC_newdata, aes(x=group, y=PICpercellpg)) + geom_boxplot()+geom_point(size=2) +
           theme_Publication()

#calculate new stuff

PIC_newdata <- mutate(PIC_newdata, rad = ifelse(group == "Nc" , Reh_naked,  Reh_calc)) #in m

####calculate variables for  calcite
#mass is PICpercell
#density is Den_calcite
#calculate volume
PIC_newdata$volume_calc <- (PIC_newdata$PICpercell*1e-12)/Den_calcite #in cm3

####calculate variables for OM
#density is Den_OCM
#calculate volume
PIC_newdata$volume_OM <- (4/3)*pi*(PIC_newdata$rad*100)^3 #rad in cm
#calculate mass
PIC_newdata$mass_OM <- Den_OcM*(PIC_newdata$volume_OM) #in g

#calculate density
PIC_newdata$Den_celltotal <- ((PIC_newdata$PICpercell*1e-12)+PIC_newdata$mass_OM)/(PIC_newdata$volume_calc+PIC_newdata$volume_OM)

#8b. calculate PIC and density for lith
PIC_newdata$Denlith <- case_when(
  PIC_newdata$group == "Nc"  ~ 1.025, 
  PIC_newdata$group == "Cc" ~ 2.6)

ggplot(data=PIC_newdata, aes(x=group, y=Denlith, color=group)) + geom_boxplot()+geom_point(size=2) 
         +theme_Publication()

#8c. calculate sinking velocity of cells,liths, and viruses
PIC_newdata$SinkVel <- ((2*((PIC_newdata$rad*100)^2)*(981)*(PIC_newdata$Den_celltotal-Den_CH2O))/(9*(mu*10)))*864 #meter per day
PIC_newdata$SinkVel_lith <- ((2*((Reh_lith*100)^2)*(981)*(PIC_newdata$Denlith-Den_CH2O))/(9*(mu*10)))*864 #meter per day
Ehv_SinkVel <- ((2*((Rehv*100)^2)*(981)*(Den_CH2O-Den_CH2O))/(9*(mu*10)))*864  #equals to 0
#g is converted to per day, 864 is the one that converts cm/s to m/day
#plot sinking velocity vs calcification

ggplot(data=PIC_newdata, aes(x=PICpercellpg, y=SinkVel, color=group, shape=group)) + geom_point(size=5)+theme_Publication()+
  labs(y = expression("Sinking velocity"~("m"~day^-1)), x = expression("PIC pg"~cell^-1)) +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) 

#8d. calculate betas for DS
PIC_newdata$beta_DS <-(pi*(((PIC_newdata$rad+Rehv)*100)^2)*(abs((PIC_newdata$SinkVel-Ehv_SinkVel)/864)))*86400 #in encounters cm3/day
PIC_newdata$beta_DS_lith <- (pi*(((Reh_lith+Rehv)*100)^2)*(abs((PIC_newdata$SinkVel_lith-Ehv_SinkVel)/864)))*86400 #in encounters cm3/day

ggplot(data=PIC_newdata, aes(x=PICpercellpg , y=beta_DS, color=group,  shape=group)) + geom_point(size=5)+
  theme_Publication()+
  labs(y = expression(beta~("Encounters"~cm^3~day^-1)), x = expression("PIC pg"~cell^-1))  +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=2),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) + annotation_logticks(sides="l")+
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) 


#8e. calculate encounters
PIC_newdata$E_DS <- (PIC_newdata$beta_DS*ViNum) #E calculated with Virus for cell
PIC_newdata$E_DS_lith <- (PIC_newdata$beta_DS_lith*ViNum) #E calculated with Virus for lith

ggplot(data=PIC_newdata, aes(x=PICpercellpg, y=log10(E_DS))) +geom_point(size=5, aes(shape=group, color=group)) +
           theme_Publication() + geom_smooth()

ggplot(data=PIC_newdata , aes(x=PICpercellpg, y=E_DS, color=group,  shape=group)) + geom_point(size=5)+theme_Publication()+
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=2),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  annotation_logticks(sides="l") + theme_Publication2() + geom_smooth(aes(group=1), color="black", method="loess") +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) +
  labs(y = expression("viral encounters " ~ day^-1~cell^-1), x = expression("PIC pg"~cell^-1)) 


#8f. summaries


summary_DS_bygroup_pred <-ddply(PIC_newdata, .(group), summarize,  PICpercellpg=mean(PICpercellpg), 
                                Den_celltotal = mean (Den_celltotal),
                                SinkVel=mean(SinkVel),beta_DS=mean(beta_DS), E_DS= mean(E_DS), 
                                Denlith = mean (Denlith), SinkVel_lith=mean (SinkVel_lith), 
                                beta_DS_lith=mean (beta_DS_lith), E_DS_lith=mean (E_DS_lith),
                                Den_celltotal_median = median(Den_celltotal), Sinkvel_median=median (SinkVel))
summary_DS_bygroup_pred

library (plotrix)
cols <- c("Den_celltotal", "SinkVel", "beta_DS")
sum_all_newdata <- PIC_newdata %>% group_by(group) %>% 
  summarise_at(.vars = cols,
               funs(mean, sd, std.error))

sum_all <- PIC %>% group_by(group) %>% 
  summarise_at(.vars = cols,
               funs(mean, sd, std.error))


setwd("D:/R program")
write.xlsx(summary_DS_bygroup_pred, file = "Postdoc-R/Exported Tables/summary_DS_bygroup_pred_master6.xlsx")
write.table(PIC_newdata, "Postdoc-R/Exported Tables/PIC_newdata.csv", sep=";", col.names=T, row.names=F)
write.table(PIC, "Postdoc-R/Exported Tables/PIC.csv", sep=";", col.names=T, row.names=F)

## ------------------------------------------------------------------------
#arrange the summary
DS_pred <- read.csv("D:/R program/Postdoc-R/CSV Files/summary_DS_bygroup_pred_master6_edited.csv")
DS_pred


## ------------------------------------------------------------------------
#9. Turbulence
#9a. make new dataframe
#disrate is cm2/s3
#make data frame
turb <- expand.grid(list (disrate = (c (1 %o% 10^(seq(-8,-2, 0.5)))), group =c("Nc", "Cc", "Li")))

turb$rad <- case_when(
  turb$group =="Nc" ~ Reh_naked,
  turb$group =="Cc" ~ Reh_calc,
  turb$group =="Li" ~ Reh_lith,
  TRUE ~ as.numeric(turb$group)
)
turb$disrate <- as.numeric(as.character(turb$disrate))

#9b. calculate beta and encounters
turb$beta_turb <- (4.2*pi*((turb$disrate/(v))^0.5)*((turb$rad+Rehv)^3))*86400*10^6 
turb$beta_turb_orig <- (4.2*pi*((turb$disrate/(v*1000^2))^0.5)*(((turb$rad+Rehv)*100)^3))*86400
turb$beta_turb_orig_2 <- 4.2*pi*(((turb$disrate/v)^0.5)*86400)*(((turb$rad+Rehv)^3)*10^6)


turb$E_turb <- (turb$beta_turb*ViNum) #E calculated with virus only

ggplot(data = turb, aes(x = disrate, y = beta_turb, color=group)) + geom_line(size =2) +
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

ggplot(data = turb, aes(x = disrate, y = E_turb, color=group)) + geom_line() +
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

#9c. combine everything
all <- Reduce(function(x,y) merge(x,y,by="group",all=TRUE) ,
              list(BM %>% select (group, beta_BM), DS_pred %>% select (group, beta_DS), 
                   turb %>% select (group, disrate, beta_turb)))
all$beta_all <- all$beta_BM + all$beta_DS + all$beta_turb

all <- all %>%
  mutate (hostlow = case_when (group=="Cc" ~ CcNum,
                            group=="Nc" ~ NcNum,
                            group=="Li" ~ LiNum)) %>%
  mutate (hosthigh = case_when (group=="Cc" ~ CcNum*10^2,
                        group=="Nc" ~ NcNum*10^2,
                        group=="Li" ~ LiNum*10^2))

all$E_all_low <- all$beta_all*ViNum
all$E_all_high <- all$beta_all*ViNum*(10^2)
#with virus and reservoir
all$E_all_low_resvi <- all$beta_all*all$hostlow*ViNum

all$E_all_high_resvi <- all$beta_all*all$hosthigh*ViNum*(10^2)

#9d. plotting
resize.win(8,5)
ggplot(data=all, aes(x=disrate,y = E_all_low_resvi , color=group, fill=group)) + 
  geom_smooth(size=1, position=position_jitter(w=0.02, h=0), aes(linetype="field", fill=group))+
  geom_smooth(data = all, aes(y= E_all_high_resvi, color=group,fill=group, linetype="lab")) +
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

