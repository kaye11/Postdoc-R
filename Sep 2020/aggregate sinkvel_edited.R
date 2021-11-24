source("inspack.R")

##import dataset
library(readxl)

sinkvel <- read_excel("C:/Users/bondoc/OneDrive - Rutgers University/Pub Matl/Model/sinkvel2.xlsx")
field <- read_excel("C:/Users/bondoc/OneDrive - Rutgers University/Pub Matl/Model/sinkvel field paticles and aggregates.xlsx")

Rehv= 90*(10)^-9 #in m radius virus
Ehv_SinkVel = 0
#all radius are m
#all densities are g/ml
#all sinkvel are m/day

sinkvel_all <- full_join (sinkvel, field)

library(DescTools)
sinkvel_all$group2 <- factor (sinkvel_all$group, labels= c("cells", "lab aggregates", "field aggregates", "cells", "lab aggregates"))

#calculate betas
sinkvel_all$beta_DS <- (pi*((sinkvel_all$rad+Rehv)^2)*(abs((sinkvel_all$SinkVel-Ehv_SinkVel))))*10^6 

#filter out data
sinkvel_filter <- sinkvel_all %>% filter (!(ref %in% c("Lecourt et al. 1996", "Bach et al. 2012", "Eppley et al. 1967", "Rosas-Navarro et al. 2018", "Milner et al. 2016"))) %>% filter (!(rad=="NA"))

#sinking velocity

resize.win (9, 6)

#change shapes
sinkvel_filter <- sinkvel_filter %>% mutate(group3 = case_when(group2 == "cells" ~ "cells",
                                                               group2 == "field aggregates" ~ "aggregates", 
                                                               group2 == "lab aggregates" ~ "aggregates"))

sinkvel_filter$group3 <- reorder.factor (sinkvel_filter$group3, new.order = c("cells", 'aggregates'))                                                             
                                                               
ggplot(data=sinkvel_filter, aes(x=as.factor(group3), y=log10(SinkVel))) + geom_boxplot() +
  geom_point(size=5, position=position_jitter(), aes(color=ref, shape=group2)) +
  labs (y = expression(log[10]~"sinking velocity "~("m"~d^-1)), color="reference") +  
  geom_hline(yintercept = log10(1), linetype="dashed")  +
  scale_color_manual(values=c('#5e77dc', '#7395de', '#8ab2dc', '#a9ced0', '#ffde73', '#c2b9aa', '#bdab9d', '#b89e90', '#8fa03c')) + scale_shape_manual(values=c(19, 15, 17)) +
  theme_Publication2()+
  theme (axis.title.x = element_blank(), legend.position="right", legend.direction = "vertical", legend.title = element_blank())

ggplot(data=sinkvel_filter, aes(x=as.factor(group3), y=log10(beta_DS))) + geom_boxplot() +
  geom_point(size=5, position=position_jitter(), aes(color=ref, shape=group2)) +
  labs (y = expression(log[10]~beta[S]~("encounters "~mL~d^-1)), color="reference") +
  geom_hline(yintercept = log10(1), linetype="dashed")  +
  scale_color_manual(values=c('#5e77dc', '#7395de', '#8ab2dc', '#a9ced0', '#ffde73', '#c2b9aa', '#bdab9d', '#b89e90', '#8fa03c')) + scale_shape_manual(values=c(19, 15, 17)) +
  theme_Publication2()+
  theme (axis.title.x = element_blank(), legend.position="right", legend.direction = "vertical", legend.title = element_blank())

sum.sinkvel <- summarySE(data=sinkvel_filter, measurevar = "SinkVel", group=c("group2"))
sum.beta <- summarySE(data=sinkvel_filter, measurevar = "beta_DS", group=c("group2"))


sinkvel_filter$beta_adj <- (pi*(((sinkvel_filter$rad*0.1)+Rehv)^2)*(abs((sinkvel_filter$SinkVel-Ehv_SinkVel))))*10^6 
##note: not true for cells
