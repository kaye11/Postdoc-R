## ------------------------------------------------------------------------
setwd("D:/R program")
source ("inspack.R")
#values needed 

K= 1.38064852*(10)^-23 #m2 kg/ s2 K boltzmann constant
mu= 1.126*(10)^-3 #kg/m s dynamic viscosity in 18C
v= 1.099*(10)^-6 #m2/s kinematic viscosity in 18C
Reh_calc= 3E-6 #in m radius Ehux
Reh_naked= 2.5E-6 #in m radius Ehux
Reh_lith = 1.5E-6 #in m radius
Rehv= 90*(10)^-9 #in m radius virus, from Castberg et al 2002
Temp = 18+273.15 #temp in kelvin, here assuming 18C
Den_OcM = 1.05 #g/cm3 density organic cell matter
Den_CH2O= 1.025 #g/cm3 density seawater at 18C
Den_Li <- 2.6 #in g/cm3 as Jakob et al
Den_Nc <- 1.05
Den_Cc <- 1.19

CcNum <- 1*((10)^3)
NcNum <- 0.1*CcNum
ViNum <- (CcNum)*30
LiNum <- (CcNum)*50 #balch 1991 from cj's paper


## ------------------------------------------------------------------------
#Brownian motion (modata)
#1. make a data frame
require(utils)

modata <- expand.grid(group1= as.factor(c("Cc", "Nc", "Li", "Vi")), group2= as.factor(c("Cc", "Nc", "Li", "Vi")))

modata <- modata %>%
  mutate(rad1 = case_when(group1 == "Nc" ~ Reh_naked,
                         group1 == "Cc" ~ Reh_calc, 
                         group1 == "Li" ~ Reh_lith,
                         group1 == "Vi" ~ Rehv)) %>%
  mutate(rad2 = case_when(group2 == "Nc" ~ Reh_naked,
                          group2 == "Cc" ~ Reh_calc, 
                          group2 == "Li" ~ Reh_lith,
                          group2 == "Vi" ~ Rehv)) %>%
  mutate(count1 = case_when(group1 == "Nc" ~ NcNum,
                          group1 == "Cc" ~ CcNum, 
                          group1 == "Li" ~ LiNum,
                          group1 == "Vi" ~ ViNum))%>%
  mutate(count2 = case_when(group2 == "Nc" ~ NcNum,
                            group2 == "Cc" ~ CcNum, 
                            group2 == "Li" ~ LiNum,
                            group2 == "Vi" ~ ViNum))%>%
  mutate(den1 = case_when(group1 == "Nc" ~ Den_Nc,
                            group1 == "Cc" ~ Den_Cc, 
                            group1 == "Li" ~ Den_Li,
                            group1 == "Vi" ~ Den_CH2O))%>%
  mutate(den2 = case_when(group2 == "Nc" ~ Den_Nc,
                         group2 == "Cc" ~ Den_Cc, 
                         group2 == "Li" ~ Den_Li,
                         group2 == "Vi" ~ Den_CH2O))


#2. calculate beta (beta)
modata$beta_BM <-((2*(K*Temp*((modata$rad1+modata$rad2)^2)))/(3*mu*(modata$rad1*modata$rad2)))*86400*10^6 #cm3/day

#3. calculate encounters (E)
modata$E_BM <- modata$beta_BM*modata$count2

ggplot(modata, aes(group1, group2)) +
  geom_tile(aes(fill = log(E_BM)), colour = "grey50")+
  scale_fill_gradient(low = "white", high = "blue")

##arrange factors
modata$group1 <- factor(modata$group1, levels=c("Cc", "Nc", "Li", "Vi"))
modata$group2 <- factor(modata$group2, levels=c("Cc", "Nc", "Li", "Vi")) 

## ------------------------------------------------------------------------
##differential settling
#calc sinkvel
modata$SinkVel1 <- ((2*((modata$rad1*100)^2)*(981)*(modata$den1-Den_CH2O))/(9*(mu*10)))*864 #meter per day
modata$SinkVel2 <- ((2*((modata$rad2*100)^2)*(981)*(modata$den2-Den_CH2O))/(9*(mu*10)))*864 #meter per day
modata$beta_DS <- (pi*(((modata$rad1+modata$rad2)*100)^2)*(abs((modata$SinkVel1-modata$SinkVel2)/864)))*86400 #in encounters cm3/day
#beta will be the same for pairings in reverse

modata$E_DS <- modata$beta_DS*modata$count2

resize.win(6,6)
ggplot(modata, aes(group1, group2)) +
  geom_tile(aes(fill = log10(E_DS)), colour = "grey50") +
  scale_fill_viridis(option="cividis") +
  ylim(rev(levels(modata$group2))) + labs (fill="log10 encounters per day") + 
  theme_Publication() + theme(axis.title = element_blank(), legend.position = "bottom", legend.direction = "horizontal", legend.key.height=unit(1,"line"), legend.key.width = unit (2, "line"))


## ------------------------------------------------------------------------
##turbulence
#disrate is cm2/s3

#make data frame

modataext <- modata[rep(seq_len(nrow(modata)), 7), ]
modataext$disrate <- rep_len (c (1 %o% (10)^(seq(-8,-2, 1))), length.out=112)

modataext$beta_turb <- (4.2*pi*((modataext$disrate/(v))^0.5)*((modataext$rad1+modataext$rad2)^3))*86400*10^6 

modataext$E_turb <- modataext$beta_turb*modataext$count2


## ------------------------------------------------------------------------
## add them all

modataext$beta_all <- modataext$beta_BM + modataext$beta_DS + modataext$beta_turb

ggplot (data=modataext %>% filter (group1 %in% c ("Nc")) %>% filter (!(group2 %in% c("Nc"))), aes(x=log10(disrate),y = log10(beta_all), color=group2)) + geom_line() + geom_point ()

#compute encounters

modataext$E_all <- modataext$beta_all*modataext$count2 #doublechecked with Heidi's model already
modataext$E_allmix <- modataext$beta_all*modataext$count1*modataext$count2
modataext$disratef <- as.factor (modataext$disrate)

ggplot (data=modataext %>% filter (group1 %in% c ("Nc")) %>% filter (!(group2 %in% c("Nc"))), aes(x=log10(disrate),y = log10(E_all), color=group2)) + geom_line() + geom_point ()

ggplot(modataext, aes(x=group2, y=group1)) +
  geom_tile(aes(fill = log10(E_all)), colour = "grey50") + geom_text(size=2.8, color="white", aes(label=formatC(E_all, format = "e", digits = 1))) +
  scale_fill_viridis(option="cividis") + 
  ylim(rev(levels(modata$group2))) +
  theme_Publication() +
  theme(axis.title = element_blank(), legend.position = "bottom", legend.title= element_blank(), legend.direction = "horizontal", legend.key.height=unit(1.5,"line"), legend.key.width = unit (2.5, "line")) + facet_grid(~disrate)

##for cj
modataext$disratecode <- factor (modataext$disrate, levels = c("1e-08", "0.001"),
                                 labels = c("calm", "stormy"))
resize.win (9,6)
ggplot(modataext %>% filter (disratecode %in% c("calm", "stormy")), aes(x=group2, y=group1)) +
  geom_tile(aes(fill = log10(E_all)))  + labs (fill= expression(log[10]~"encounters "~"day"^-1)) +
  scale_fill_viridis(option="cividis") + 
  #geom_text(size=4, color="white", aes(label=formatC(E_all, format = "e", digits = 1))) +
  ylim(rev(levels(modata$group2))) +
  theme_Publication2() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "bottom", legend.direction = "horizontal", legend.key.height=unit(1.5,"line"), legend.key.width = unit (2.5, "line")) + facet_grid(~disratecode)

ggplot(modataext %>% filter (disratecode %in% c("calm", "stormy")), aes(x=group2, y=group1)) +
  geom_tile(aes(fill = (E_all)))  + 
  scale_fill_viridis(option="cividis") + 
  geom_text(size=4, color="white", aes(label=formatC(E_all, digits = 3))) +
  ylim(rev(levels(modata$group2))) +
  theme_Publication2() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "bottom", legend.direction = "horizontal", legend.key.height=unit(1.5,"line"), legend.key.width = unit (2.5, "line")) + facet_grid(~disratecode)

##days
modataext$days <- 1/(modataext$E_all)

ggplot(modataext %>% filter (disratecode %in% c("calm", "stormy")), aes(x=group2, y=group1)) +
  geom_tile(aes(fill = log10(days)))  + labs (fill= expression(log[10]~"days to encounter")) +
  scale_fill_viridis(option="cividis") + 
  #geom_text(size=4, color="white", aes(label=formatC(E_all, format = "e", digits = 1))) +
  ylim(rev(levels(modata$group2))) +
  theme_Publication2() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "bottom", legend.direction = "horizontal", legend.key.height=unit(1.5,"line"), legend.key.width = unit (2.5, "line")) + facet_grid(~disratecode)

sum <- modataext %>% filter (disratecode %in% c("calm", "stormy"))

write.table (sum, "Postdoc-R/Exported Tables/sum_cjpaper_paasche.csv", sep=";", col.names=T, row.names=F)
