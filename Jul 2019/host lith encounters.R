## ------------------------------------------------------------------------
setwd("D:/R program")
#values needed 

K= 1.38064852*(10)^-23 #m2 kg/ s2 K boltzmann constant
mu= 1.126*(10)^-3 #kg/m s dynamic viscosity in 18C
v= 1.099*(10)^-6 #m2/s kinematic viscosity in 18C
Reh_naked= 1.8E-6 #in m radius Ehux
Reh_lith = 2E-6
Temp = 18+273.15 #temp in kelvin, here assuming 18C
Den_OcM = 1.05 #g/cm3 density organic cell matter
Den_CH2O= 1.025 #g/cm3 density seawater at 18C
hostnum <- (10)^3
lithnum <- hostnum*50

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

## ------------------------------------------------------------------------
#Brownian motion (BM)
#1. make a data frame

BM <- data.frame (group= c("lith"), rad= c(2E-6)) 

#2. calculate beta (beta)
BM$beta_BM_s <- (2*(K*(10)^4)*Temp*(((BM$rad+Reh_naked )*100)^2))/((3*mu*10)*(BM$rad*Reh_naked*1e4)) #m3/s
BM$beta_BM_d <- BM$beta_BM_s*86400 #to cm3/day

##differential settling
#1. read in PIC data
library(readxl)
PIC <- read_excel("Postdoc-R/Exported Tables/PIC_DS.xlsx")
View(PIC)

#2. filter naked cells as host
host <- PIC %>% filter(group == "naked") %>% select(Strain, Replicate, PICpercell, PICpercellpg, group, rad, Den_celltotal, SinkVel, group2, beta_s, beta_d)

lith <- PIC %>% filter(group %in% c("calcified")) %>% select(Strain, Replicate, perlith, perlithpg, group, rad, Denlith, SinkVel_lith, group2, beta_s_lith, beta_d_lith)

lith$rad <- 1.5E-6
host$particle <- "host"
lith$particle <- "lith"

library(dplyr)

oldnames_host = c("PICpercell","PICpercellpg", "Den_celltotal", "SinkVel", "beta_s", "beta_d")
oldnames_lith = c("perlith","perlithpg", "Denlith", "SinkVel_lith", "beta_s_lith", "beta_d_lith")
newnames = c("PICperpart","PICperpartpg", "Den", "SinkVel", "beta_DS_s", "beta_DS_d")

host <- host %>% rename_at(vars(oldnames_host), ~ newnames)
lith <- lith %>% rename_at(vars(oldnames_lith), ~ newnames)

DS_all <- rbind (host, lith)

DS_sum_sinkvel <- summarySE (DS_all, measurevar = "SinkVel", groupvars = c("particle"))
hostsinkvel <- 0.03666226

lith$beta_DS_s <- pi*(((lith$rad+Reh_naked)*100)^2)*(abs((lith$SinkVel-hostsinkvel)/864)) #in encounters cm3/s
lith$beta_DS_d <- lith$beta_DS_s*86400 #in cm3/day

#TURBULENCE
#disrate is cm2/s3

#make data frame

disrate <- rep_len (c (1 %o% 10^(seq(-8,-2, 0.5))), length.out=26)
host_part <- rep_len(c("host"), length.out=13)
lith_part <- rep_len(c("lith"), length.out=13)
particle <- c(host_part, lith_part)
turb <- as.data.frame(cbind(disrate, particle))

turb$rad <- case_when(
  turb$particle =="host" ~ 1.8E-6,
  turb$particle =="lith" ~ 2E-6,
  TRUE ~ as.numeric(turb$particle)
)

turb$disrate <- as.numeric(as.character(turb$disrate))

turb_lith <- subset (turb, turb$particle=="lith")
turb_host <- subset (turb, turb$particle=="host")

turb_lith$beta_T_s <- 4.2*pi*((turb_lith$disrate/(v*100^2))^0.5)*(((turb_lith$rad+Reh_naked)*100)^3)
turb_lith$beta_T_d <- turb_lith$beta_T_s*86400 

turb_host$beta_T_s <- 4.2*pi*((turb_host$disrate/(v*100^2))^0.5)*(((turb_host$rad+Reh_lith)*100)^3)
turb_host$beta_T_d <- turb_host$beta_T_s*86400 

#both turb_host and turb_lith are the same

library(Rmisc)
DS_sum <- summarySE (lith, measurevar = "beta_DS_d", groupvars = c("particle"))

beta_DS_d <- mean (lith$beta_DS_d)

all <- Reduce(function(x,y) merge(x,y,by="particle",all=TRUE), 
              list(DS_sum, turb))

beta_BM <- 8.248001e-10

turb_lith$beta_all <- beta_BM + beta_DS_d + turb_lith$beta_T_d

turb_lith$E <- turb_lith$beta_all*lithnum
turb_lith$logE <- log10 (turb_lith$E)

resize.win(9,6)
ggplot(data=turb_lith, 
       aes(x=disrate,y = E)) + 
  geom_line(size=1.5, position=position_jitter(w=0.02, h=0))+
  scale_linetype_manual (values=c("solid", "dotted")) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=2),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=7),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks()+
  theme_Publication() +
  theme(legend.title = element_blank(), legend.key.width=unit(1,"cm"))+
  labs(y = expression("lith encounters " ~day^-1~cell^-1), x = expression("dissipation rate "~(m^2~s^-3))) +
  theme(legend.title = element_blank(), legend.key.width=unit(3,"line"))+
  guides(linetype=guide_legend(ncol=1), colour=guide_legend(ncol=2,byrow=TRUE))

ggplot(data=turb_lith, 
       aes(x=disrate,y = logE)) + 
  geom_line(size=1.5, position=position_jitter(w=0.02, h=0))+
  scale_linetype_manual (values=c("solid", "dotted")) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=7),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks()+
  theme_Publication() +
  theme(legend.title = element_blank(), legend.key.width=unit(1,"cm"))+
  labs(y = expression("lith encounters " ~day^-1~cell^-1), x = expression("dissipation rate "~(m^2~s^-3))) +
  theme(legend.title = element_blank(), legend.key.width=unit(3,"line"))+
  guides(linetype=guide_legend(ncol=1), colour=guide_legend(ncol=2,byrow=TRUE))


