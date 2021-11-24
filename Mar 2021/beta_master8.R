## ------------------------------------------------------------------------
source("inspack.R")

#values needed 

K= 1.38064852*(10)^-23 #m2 kg/ s2 K boltzmann constant
mu= 1.126*(10)^-3 #kg/m s dynamic viscosity in 18C
v= 1.099*(10)^-6 #m2/s kinematic viscosity in 18C
Reh_calc= 2.3E-6 #in m radius Ehux
Reh_lith = 1.3E-6 #in m radius
Rehv= 90*(10)^-9 #in m radius virus, from castberg et al 2002
Temp = 18+273.15 #temp in kelvin, here assuming 18C
Den_OcM = 1.05 #g/cm3 density organic cell matter
Den_CH2O= 1.025 #g/cm3 density seawater at 18C
Den_calcite <- 2.6 #in g/cm3 as Jakob et al
CcNum <- 1*((10)^3)
#NcNum <- 0.1*((10)^3)
ViNum <- (CcNum)*7 #NAVICE
LiNum <- (CcNum)*26 #satellite data Toby


## ------------------------------------------------------------------------
#Brownian motion (BM)
#####1. make a data frame
BM <- data.frame (group=as.factor(c("Cc", "Li")), rad= c(Reh_calc, Reh_lith)) 

#####2. calculate beta (beta)
BM$beta_BM <- ((2*(K*Temp*((BM$rad+Rehv)^2)))/(3*mu*(BM$rad*Rehv)))*86400*10^6 #cm3/d

write.table(BM, "Postdoc-R/Exported Tables/beta_master8_BM.csv", sep=";", col.names=T, row.names=F)

## ------------------------------------------------------------------------
#Differential settling (DS)
#####1. read in PIC data
library(readr) #always use readr not baseR

setwd("D:/R program")
PIC <- read_csv("Postdoc-R/CSV Files/PIC.csv")

PIC$Strain <- as.factor(PIC$Strain)
PIC$Replicate <- as.factor(PIC$Replicate)

#####2. calculate PIC for cell
PIC$PIC <- PIC$TC-PIC$AC
PIC$PICpercell <- (PIC$PIC/PIC$Cellcount)*(10)^-3#in g
PIC <- mutate(PIC, PICpercell = ifelse(PICpercell<0, 0,  PICpercell)) #in m
PIC$PICpercellpg <- PIC$PICpercell*1e12

ggplot(data=PIC, aes(x=Strain, y=PICpercellpg)) + geom_boxplot()+geom_point(size=2) +theme_Publication()

#####3a. calculate density of cells (den_cell)
PIC <- mutate(PIC, group = ifelse(PICpercellpg < 4 , "Nc", "Cc"))

ggplot(data=PIC, aes(x=Strain, y=PICpercellpg, color=group)) + geom_boxplot()+geom_point(size=2)+ 
  theme_Publication()

#delete data for naked strains
PIC <- PIC %>% filter (!(group=="Nc"))

PIC$rad <- Reh_calc

#calculate variables for  calcite, #mass is PICpercell, #density is Den_calcite, #calculate volume
PIC$volume_calc <- PIC$PICpercell/Den_calcite #in cm3

#calculate variables for OM, #density is Den_OCM, #calculate volume
PIC$volume_OM <- (4/3)*pi*(PIC$rad*100)^3 #rad in cm
#calculate mass
PIC$mass_OM <- Den_OcM*(PIC$volume_OM) #in g

#calculate density
PIC$density <- (PIC$PICpercell+PIC$mass_OM)/(PIC$volume_calc+PIC$volume_OM)

ggplot(data=PIC, aes(x=Strain, y=density)) + geom_boxplot()+geom_point(size=2) + theme_Publication()

#####3b. put lith on the table and clean up table
lith <- data.frame (group="Li", rad= Reh_lith, density=Den_calcite)

DS <- full_join((PIC %>% select(group, PICpercellpg, Strain, rad, density)), lith)

#####4. calculate sinking velocity of cells,liths, and viruses
DS$SinkVel <- ((2*((DS$rad*100)^2)*(981)*(DS$density-Den_CH2O))/(9*(mu*10)))*864 #meter per day
Ehv_SinkVel <- ((2*((Rehv*100)^2)*(981)*(Den_CH2O-Den_CH2O))/(9*(mu*10)))*864  #equals to 0

#plot sinking velocity vs calcification
#for paper
resize.win(6,6)
ggplot(data=DS %>% filter (group=="Cc"), aes(x=PICpercellpg, y=SinkVel, shape=Strain)) + geom_point(size=5)+
  theme_Publication2()+
  labs(y = expression("sinking velocity "~("m"~day^-1)), x = expression("PIC pg"~cell^-1)) + 
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) 

#####5. calculate betas for DS
DS$beta_DS <-(pi*((DS$rad+Rehv)^2)*(abs((DS$SinkVel-Ehv_SinkVel))))*10^6 #in encounters cm3/day

ggplot(data=DS, aes(x=PICpercellpg, y=beta_DS, color=Strain,  shape=group)) + geom_point(size=5)+theme_Publication()+
  labs(y = expression(beta~("Encounters"~cm^3~day^-1)), x = expression("PIC pg"~cell^-1))  +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=2),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) + annotation_logticks(sides="l")+
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) 

#####6. summaries
library(plyr)
summary.DS <- ddply(DS, .(group), summarize,  PICpercellpg=mean(PICpercellpg), 
                            density = mean (density),
                            SinkVel=mean(SinkVel), beta_DS=mean(beta_DS))


write.table(summary.DS, "Postdoc-R/Exported Tables/beta_master8_DS.csv", sep=";", col.names=T, row.names=F)

## ------------------------------------------------------------------------
#Turbulence

#####1. make new dataframe, disrate is cm2/s3
turb <- expand.grid(list (disrate = (c (1 %o% 10^(seq(-8,-2, 0.5)))), group =c("Cc", "Li")))

turb$rad <- case_when(
  turb$group =="Cc" ~ Reh_calc,
  turb$group =="Li" ~ Reh_lith,
  TRUE ~ as.numeric(turb$group)
)

turb$disrate <- as.numeric(as.character(turb$disrate))

#####2. calculate beta and encounters
turb$beta_turb <- (4.2*pi*((turb$disrate/(v))^0.5)*((turb$rad+Rehv)^3))*86400*10^6 

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

## ------------------------------------------------------------------------
#Combine all betas

all <- Reduce(function(x,y) merge(x,y,by="group",all=TRUE) ,
              list(BM %>% select (group, beta_BM), summary.DS %>% select (group, beta_DS), 
                   turb %>% select (group, disrate, beta_turb)))
all$beta_all <- all$beta_BM + all$beta_DS + all$beta_turb

all <- all %>%
  mutate (hostlow = case_when (group=="Cc" ~ CcNum, group=="Li" ~ LiNum)) %>%
  mutate (hosthigh = case_when (group=="Cc" ~ CcNum*10^2, group=="Li" ~ LiNum*10^2))

all$E_all_lowvir <- all$beta_all*ViNum
all$E_all_highvir <- all$beta_all*ViNum*(10^2)

#plotting
resize.win(8,5)
ggplot(data=all, aes(x=disrate,y = E_all_lowvir , color=group, fill=group)) + 
  geom_smooth(size=1, position=position_jitter(w=0.02, h=0), aes(linetype="field", fill=group))+
  geom_smooth(data = all, aes(y= E_all_highvir, color=group,fill=group, linetype="lab")) +
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

##save all data in one table
write.table(all, "Postdoc-R/Exported Tables/beta_master8_allbetas.csv", sep=";", col.names=T, row.names=F)


## ------------------------------------------------------------------------
#plots
#summaries
BM #BM summary
summary.DS #predicted data

#sinkvel vs PIC
resize.win (6,4)
ggplot(data=DS %>% filter (group=="Cc"), aes(x=PICpercellpg, y=SinkVel, color=group)) + geom_point(aes(size=density))+
  scale_size(range = c(1,10)) + theme_Publication()+
  labs(y = expression("sinking velocity "~("m"~d^-1)), x = expression("PIC pg"~cell^-1)) + 
  theme(legend.direction = "vertical", legend.box = "vertical", legend.position="right", legend.title = element_blank()) +
  scale_color_manual(values=c("#5ab4ac")) + expand_limits(x=0, y=0) +
  guides(colour = guide_legend(override.aes = list(size=5)))

#sinkvel vs beta sinking
resize.win (6.5,4.5)
ggplot(data=DS %>% filter (group=="Cc"), aes(x=SinkVel, y=log10(beta_DS), color="E. huxleyi", shape=Strain)) + 
  geom_point(size=4, stroke=2)+ theme_Publication()+
  geom_point (data=DS %>% filter (group=="Li"), aes(x=SinkVel, y=log10(beta_DS), color="coccolith"), size=6, shape=16) +
  labs(y = expression(log[10]~beta[S]~("encounters "~mL~d^-1)), x = expression("sinking velocity "~("m"~d^-1)))  +
  theme(legend.direction = "vertical", legend.box = "vertical", legend.position="right", legend.title = element_blank(),
        legend.key.size = unit(0.8, "cm")) +
  scale_color_manual(values=c("#d8b365", "#5ab4ac"))  + scale_shape_manual(values=c(0:5))

#dissipation rate and beta turb
resize.win(5,5)
turb$group <- factor (turb$group,labels= c("E. huxleyi", "coccolith"))
ggplot(data = turb, aes(x = log10(disrate), y = log10(beta_turb), color=group)) + geom_line(size =2.5) +
  theme_Publication() +
  labs(y = expression(log[10]~beta[T]~("encounters "~mL~d^-1)), 
       x = expression(log[10]~epsilon~(m^2~s^-3))) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#5ab4ac", "#d8b365")) + guides(colour = guide_legend(override.aes = list(size=5)))

#encounters all betas, viral encounters per day per cell
all$group <- reorder.factor (all$group, new.order = c("Cc", "Li"))
all$group2 <- factor (all$group,labels= c("E. huxleyi", "coccolith"))

resize.win(6.8,5)

ggplot(data=all, aes(x=log10(disrate),y = log10(E_all_highvir) , color=group2)) + 
  geom_line(size=2, position=position_jitter(w=0.02, h=0), aes(linetype="shelf/slope"))+
  geom_line(size=2, data = all, aes(y= log10(E_all_lowvir), color=group2,linetype="open ocean")) +
  theme_Publication2() +
  theme(legend.title = element_blank(), legend.key.width=unit(1,"cm"))+
  labs(y = expression(log[10]~"viral encounters " ~entity^-1~d^-1), x = expression(log[10]~epsilon~(m^2~s^-3))) +
  theme(legend.title = element_blank()) +
  scale_color_manual (values=c("#5ab4ac", "#d8b365")) + 
  scale_fill_manual (values=c("#5ab4ac", "#d8b365"))

