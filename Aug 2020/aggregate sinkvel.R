source("inspack.R")

##import dataset
library(readxl)

sinkvel <- read_excel("C:/Users/bondoc/OneDrive - Rutgers University/Pub Matl/Model/sinkvel2.xlsx")

Rehv= 90*(10)^-9 #in m radius virus
Ehv_SinkVel = 0
#all radius are m
#all densities are g/ml
#all sinkvel are m/day

#codes: Nc and Cc are cells, Ncaggl are 
sinkvel$beta_DS <-(pi*(((sinkvel$rad+Rehv)*100)^2)*(abs((sinkvel$SinkVel-Ehv_SinkVel)/864)))*86400 #in encounters cm3/day

##plotting
library(DescTools)
sinkvel$group <- reorder.factor(sinkvel$group, new.order = c("Nc", "Cc", "Ncaggl", "Ccaggl"))

sinkvel$group <- factor (sinkvel$group, labels= c("Nc",  "Cc", "Nc aggregate", "Cc aggregate"))

resize.win(7,7)

#sinking velocity
ggplot(data=sinkvel %>% filter (! (ref=="Iversen et al. 2010")), 
       aes(x=as.factor(group), y=log10(SinkVel))) + geom_boxplot() +
  geom_point(size=3, position=position_jitter(), aes(color=ref)) +
  labs (y = expression(log[10]~"sinking velocity "~("m"~d^-1)), color="reference") +
  geom_hline(yintercept = log10(1), linetype="dashed")  +
  scale_color_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#8da0cb','#a65628','#f781bf')) +
  theme_Publication2()+
  theme (axis.title.x = element_blank(), legend.position="bottom", legend.direction = "horizontal", legend.title = element_blank()) +
  guides (color=guide_legend(nrow=4))

##calculate beta_DS
sinkvel$beta_DS <-(pi*(((sinkvel$rad+Rehv)*100)^2)*(abs((sinkvel$SinkVel-Ehv_SinkVel)/864)))*86400 #in encounters cm3/day

ggplot(data=sinkvel %>% filter (! (ref=="Iversen et al. 2010")), 
       aes(x=as.factor(group), y=log10(beta_DS))) + geom_boxplot() +
  geom_point(size=3, position=position_jitter(), aes(color=ref)) +
  labs (y = expression(log[10]~beta[S]~("encounters "~mL~d^-1)), color="reference") +
  geom_hline(yintercept = log10(1), linetype="dashed")  +
  scale_color_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#8da0cb','#a65628','#f781bf')) +
  theme_Publication2()+
  theme (axis.title.x = element_blank(), legend.position="bottom", legend.direction = "horizontal", legend.title = element_blank()) +
  guides (color=guide_legend(nrow=4))

#ESD spherical diameter
ggplot(data=sinkvel %>% filter (! (ref=="Iversen et al. 2010")), 
       aes(x=as.factor(group), y=log10(2*(rad)))) + geom_boxplot() +
  geom_point(size=3, position=position_jitter(), aes(color=ref)) +
  labs (y = expression(log[10]~"ESD (Estimated spherical diameter [m])"), color="reference") +
  scale_color_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#8da0cb','#a65628','#f781bf')) +
  theme_Publication2()+
  theme (axis.title.x = element_blank(), legend.position="bottom", legend.direction = "horizontal", legend.title = element_blank()) +
  guides (color=guide_legend(nrow=4))

sinkvel <- sinkvel %>%
  mutate(porositymin1 = case_when(group %in% c("Cc", "Nc") ~ 1, 
                               group %in% c("Cc aggregate") ~ 0.041, #1-0.959 porosity from Engel et al. 2009
                              group %in% c("Nc aggregate") ~ 0.004)) #1-0.996 porosity 

sinkvel <- sinkvel %>%
  mutate(porosity = case_when(group %in% c("Cc", "Nc") ~ 1, 
                              group %in% c("Cc aggregate") ~ 0.959, 
                              group %in% c("Nc aggregate") ~ 0.996)) 

#consider the porosity as to sinkvel
sinkvel$excessden <- (-(sinkvel$porosity-1))*as.numeric(sinkvel$den)

#Nc is less compact
sinkvel$rad_por <- sinkvel$rad*sinkvel$porositymin1
sinkvel$beta_DS_radpor <-(pi*(((sinkvel$rad_por+Rehv)*100)^2)*(abs((sinkvel$SinkVel-Ehv_SinkVel)/864)))*86400 #in encounters cm3/day

ggplot(data=sinkvel %>% filter (! (ref=="Iversen et al. 2010")), 
       aes(x=as.factor(group), y=log10(beta_DS_radpor))) + geom_boxplot() +
  geom_point(size=3, position=position_jitter(), aes(color=ref)) +
  labs (y = expression(log[10]~beta[S]~("encounters "~mL~d^-1)~"adjusted"), color="reference") +
  geom_hline(yintercept = log10(1), linetype="dashed")  +
  scale_color_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#8da0cb','#a65628','#f781bf')) +
  theme_Publication2()+
  theme (axis.title.x = element_blank(), legend.position="bottom", legend.direction = "horizontal", legend.title = element_blank()) +
  guides (color=guide_legend(nrow=4))

#save as table to wrangle
write.table(sinkvel, "Postdoc-R/Exported Tables/sinkvel_aggregates.csv", sep=";", col.names=T, row.names=F)


sum_sinkvel.bygroup <- summarySE (data=sinkvel, groupvars= c("group"), measurevar = "SinkVel")
sum_beta.bygroup <- summarySE (data=sinkvel, groupvars= c("group"), measurevar = "beta_DS")

medmaxmin <- ddply(sinkvel, .(group), summarize, 
                   mean_SinkVel= mean (SinkVel), med_sinkvel=median(SinkVel), max_sinkvel=max(SinkVel), 
                   min_sinkvel=min(SinkVel), mean_beta_DS = mean (beta_DS), med_beta_DS=median(beta_DS), 
                   max_beta_DS=max(beta_DS), min_beta_DS=min(beta_DS))

bondoc.medmaxmin <- ddply(sinkvel %>% filter (ref=="this study"), .(group), summarize, 
                          mean_SinkVel= mean (SinkVel), med_sinkvel=median(SinkVel), max_sinkvel=max(SinkVel), 
                          min_sinkvel=min(SinkVel), mean_beta_DS = mean (beta_DS), med_beta_DS=median(beta_DS), 
                          max_beta_DS=max(beta_DS), min_beta_DS=min(beta_DS))

sinkvel_aggregates_edited <- read.csv("D:/R program/Postdoc-R/CSV Files/sinkvel_aggregates_edited.csv")

resize.win (9,7)

sinkvel_aggregates_edited$group <- reorder.factor(sinkvel_aggregates_edited$group, new.order = c("Nc", "Cc", "Nc aggregate", "Cc aggregate", "Nc aggregate adj", "Cc aggregate adj"))

sinkvel_aggregates_edited$group2 <- factor (sinkvel_aggregates_edited$group, labels= c("cells",  "cells", "lab aggregate", "lab aggregate", "lab aggregate", "lab aggregate"))

ggplot(data=sinkvel_aggregates_edited %>% filter (! (ref=="Iversen et al. 2010")), 
       aes(x=as.factor(group), y=log10(beta_DS))) + geom_boxplot() +
  geom_point(size=3, position=position_jitter(), aes(color=ref)) +
  labs (y = expression(log[10]~beta[S]~("encounters "~mL~d^-1)), color="reference") +
  geom_hline(yintercept = log10(1), linetype="dashed")  +
  scale_color_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#8da0cb','#a65628','#f781bf')) +
  theme_Publication2()+
  theme (axis.title.x = element_blank(), legend.position="bottom", legend.direction = "horizontal", legend.title = element_blank()) +
  guides (color=guide_legend(nrow=4))

ggplot(data=sinkvel_aggregates_edited %>% filter (!(group %in% c("Nc aggregate adj", "Cc aggregate adj"))) %>% filter (!(ref %in% c("Lecourt et al. 1996", "Bach et al. 2012", "Eppley et al. 1967", "Rosas-Navarro et al. 2018", "Milner et al. 2016"))),  
       aes(x=as.factor(group2), y=log10(beta_DS))) + geom_boxplot() +
  geom_point(size=3, position=position_jitter(), aes(color=ref)) +
  labs (y = expression(log[10]~beta[S]~("encounters "~mL~d^-1)), color="reference") +
  geom_hline(yintercept = log10(1), linetype="dashed")  +
  #scale_color_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#8da0cb','#a65628','#f781bf')) +
  theme_Publication2()+
  theme (axis.title.x = element_blank(), legend.position="bottom", legend.direction = "horizontal", legend.title = element_blank()) +
  guides (color=guide_legend(nrow=4))

#add 