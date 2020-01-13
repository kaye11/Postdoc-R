## ------------------------------------------------------------------------
setwd("D:/R program")
source ("inspack.R")
#values needed 

K= 1.38064852*(10)^-23 #m2 kg/ s2 K boltzmann constant
mu= 1.126*(10)^-3 #kg/m s dynamic viscosity in 18C
v= 1.099*(10)^-6 #m2/s kinemodataic viscosity in 18C
Reh_calc= 2.3E-6 #in m radius Ehux
Reh_naked= 1.8E-6 #in m radius Ehux
Reh_lith = 1.5E-6
Rehv= 90*(10)^-9 #in m radius virus
Temp = 18+273.15 #temp in kelvin, here assuming 18C
Den_OcM = 1.05 #g/cm3 density organic cell modatater
Den_CH2O= 1.025 #g/cm3 density seawater at 18C
host <- (10)^3
CcNum <- 0.9*host
NcNum <- 0.1*host
ViNum <- (CcNum+NcNum)*10
LiNum <- CcNum*50

## ------------------------------------------------------------------------
#Brownian motion (modata)
#1. make a data frame
require(utils)

modata <- expand.grid(group1= as.factor(c("Cc", "Nc", "Li", "Vi")), group2= as.factor(c("Cc", "Nc", "Li", "Vi")))

modata <- modata %>%
  mutate(rad1 = case_when(group1 == "Nc" ~ 1.8E-6,
                         group1 == "Cc" ~ 2.3E-6, 
                         group1 == "Li" ~ 1.25E-6,
                         group1 == "Vi" ~ 2E-9)) %>%
  mutate(rad2 = case_when(group2 == "Nc" ~ 1.8E-6,
                         group2 == "Cc" ~ 2.3E-6, 
                         group2 == "Li" ~ 1.25E-6,
                         group2 == "Vi" ~ 2E-9)) %>%
  mutate(count1 = case_when(group1 == "Nc" ~ NcNum,
                          group1 == "Cc" ~ CcNum, 
                          group1 == "Li" ~ LiNum,
                          group1 == "Vi" ~ ViNum))%>%
  mutate(count2 = case_when(group2 == "Nc" ~ NcNum,
                            group2 == "Cc" ~ CcNum, 
                            group2 == "Li" ~ LiNum,
                            group2 == "Vi" ~ ViNum))%>%
  mutate(den1 = case_when(group1 == "Nc" ~ 1.09,
                            group1 == "Cc" ~ 1.25, ##average of mod calc and strongly calc uses 
                            group1 == "Li" ~ 1.21,
                            group1 == "Vi" ~ Den_CH2O))%>%
  mutate(den2 = case_when(group2 == "Nc" ~ 1.09,
                         group2 == "Cc" ~ 1.25, ##average of mod calc and strongly calc uses 
                         group2 == "Li" ~ 1.21,
                         group2 == "Vi" ~ Den_CH2O))


#2. calculate beta (beta)
modata$beta_BM <- ((2*(K*(10)^4)*Temp*(((modata$rad1+modata$rad2)*100)^2))/((3*mu*10)*(modata$rad1*modata$rad2*1e4)))*86400 #cm3/s

#3. calculate encounters (E)
modata$E_BM <- modata$beta_BM*modata$count1

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

modata$E_DS <- modata$beta_DS*modata$count1

resize.win(6,6)
ggplot(modata, aes(group1, group2)) +
  geom_tile(aes(fill = log10(E_DS)), colour = "grey50") +
  scale_fill_viridis(option="cividis") +
  ylim(rev(levels(modata$group2))) + labs (title = 10^3, fill="log10 encounters per day") + 
  theme_Publication() + theme(axis.title = element_blank(), legend.position = "bottom", legend.direction = "horizontal", legend.key.height=unit(1,"line"), legend.key.width = unit (2, "line"))


## ------------------------------------------------------------------------
##turbulence
#disrate is cm2/s3

#make data frame

modataext <- modata[rep(seq_len(nrow(modata)), 7), ]
modataext$disrate <- rep_len (c (1 %o% (10)^(seq(-8,-2, 1))), length.out=112)

modataext$beta_turb <- (4.2*pi*((modataext$disrate/(v*100^2))^0.5)*(((modataext$rad1 +modataext$rad2)*100)^3))*86400 

modataext$E_turb <- modataext$beta_turb*modataext$count2


#random plotchecks
ggplot(modataext %>% filter (disrate %in% c ("1e-04")), aes(group1, group2)) +
  geom_tile(aes(fill = log10(beta_turb)), colour = "grey50") +
  scale_fill_viridis(option="cividis") +
  ylim(rev(levels(modataext$group2))) + labs (title = 10^-4, fill="log10 mL per day") + 
  theme_Publication() + theme(axis.title = element_blank(), legend.position = "right", legend.direction = "vertical", legend.key.height=unit(2.5,"line"), legend.key.width = unit (1.5, "line"))


ggplot (data=modataext %>% filter (group1 %in% c ("Nc")), aes(x=log10(disrate),y = log10(beta_turb), color=group2)) + geom_line() +geom_point()

ggplot (data=modataext, aes(x=group1,y = group2, color=log10(disrate), size=log10(beta_turb))) + geom_line() + geom_point(position = position_jitter())

## ------------------------------------------------------------------------
## add them all

modataext$beta_all <- modataext$beta_BM + modataext$beta_DS + modataext$beta_turb

ggplot (data=modataext %>% filter (group1 %in% c ("Nc")) %>% filter (!(group2 %in% c("Nc"))), aes(x=log10(disrate),y = log10(beta_all), color=group2)) + geom_line() + geom_point ()

#compute encounters

modataext$E_all <- modataext$beta_all*modataext$count2 #doublechecked with Heidi's model already
modataext$disratef <- as.factor (modataext$disrate)

ggplot (data=modataext %>% filter (group1 %in% c ("Nc")) %>% filter (!(group2 %in% c("Nc"))), aes(x=log10(disrate),y = log10(E_all), color=group2)) + geom_line() + geom_point ()

ggplot(modataext, aes(x=group2, y=group1)) +
  geom_tile(aes(fill = log10(E_all)), colour = "grey50") + geom_text(size=2.8, color="white", aes(label=formatC(E_all, format = "e", digits = 1))) +
  scale_fill_viridis(option="cividis") + 
  ylim(rev(levels(modata$group2))) +
  theme_Publication() +
  theme(axis.title = element_blank(), legend.position = "bottom", legend.title= element_blank(), legend.direction = "horizontal", legend.key.height=unit(1.5,"line"), legend.key.width = unit (2.5, "line")) + facet_grid(~disrate)


#export data
setwd("D:/R program")
require(openxlsx)
write.xlsx(modataext, file = "Postdoc-R/Exported Tables/encounters 10 to the 5 host_corden.xlsx")


##-------------STOP HERE---------------------------------------------------------##
#readin data
library(readxl)
three <- read_excel("Postdoc-R/Exported Tables/encounters 10 to the 3 host_corden.xlsx")
four <- read_excel("Postdoc-R/Exported Tables/encounters 10 to the 4 host_corden.xlsx")
five <- read_excel("Postdoc-R/Exported Tables/encounters 10 to the 5 host_corden.xlsx")

##arrange factors
three$group1 <- factor(three$group1, levels=c("Cc", "Nc", "Li", "Vi"))
three$group2 <- factor(three$group2, levels=c("Cc", "Nc", "Li", "Vi")) 
four$group1 <- factor(four$group1, levels=c("Cc", "Nc", "Li", "Vi"))
four$group2 <- factor(four$group2, levels=c("Cc", "Nc", "Li", "Vi")) 
five$group1 <- factor(five$group1, levels=c("Cc", "Nc", "Li", "Vi"))
five$group2 <- factor(five$group2, levels=c("Cc", "Nc", "Li", "Vi")) 

resize.win (12,8)
ggplot(four, aes(x=group2, y=group1)) +
  geom_tile(aes(fill = log10(E_all)), colour = "grey50") +
  scale_fill_viridis(option="cividis") +
  ylim(rev(levels(modata$group1))) + labs (fill="log10 encounters per day") + 
  theme_Publication() +
  theme(axis.title = element_blank(), legend.position = "bottom", legend.direction = "horizontal", legend.key.height=unit(1,"line"), legend.key.width = unit (2, "line")) +facet_wrap(~disrate, nrow=2)

three$host <- 10^3
four$host <- 10^4
five$host <- 10^5

comb <- rbind(three, four, five)

resize.win(16,8)
ggplot(comb %>% filter (host %in% c("1000", "1e+05")), aes(x=group2, y=group1)) +
  geom_tile(aes(fill = log10(E_all)), colour = "grey50") +
  scale_fill_viridis(option="cividis") +
  ylim(rev(levels(modata$group1))) + labs (fill="log10 encounters per day") + 
  theme_Publication() +
  theme(axis.title = element_blank(), legend.position = "bottom", legend.direction = "horizontal", legend.key.height=unit(1,"line"), legend.key.width = unit (2, "line")) +facet_grid(host~disrate)

#for visualization, with texts
ggplot(comb %>% filter (host %in% c("1000", "1e+05")) , aes(x=group2, y=group1)) +
  geom_tile(aes(fill = log10(E_all)), colour = "grey50") +  geom_text(size=2.8, color="white", aes(label=formatC(E_all, format = "e", digits = 1))) +
  scale_fill_viridis(option="cividis") +
  ylim(rev(levels(modata$group1))) + labs (fill="log10 encounters per day") + 
  theme_Publication() +
  theme(axis.title = element_blank(), legend.position = "bottom", legend.direction = "horizontal", legend.key.height=unit(1,"line"), legend.key.width = unit (2, "line")) +facet_grid(host~disrate)


#####____________________________________#####
### for lith concentration *10

three_lith10 <- three
four_lith10 <- four
five_lith10 <- five

comb.lith10 <- comb %>%
  mutate(count1 = case_when(host == "1000" & group1 == "Li" ~ 10^4,
                            host == "10000" & group1 == "Li" ~ 10^5,
                            host == "1e+05" & group1 == "Li" ~ 10^6))%>%
  mutate(count2 = case_when(host == "1000" & group2 == "Li" ~ 10^4,
                            host == "10000" & group2 == "Li" ~ 10^5,
                            host == "1e+05" & group2 == "Li" ~ 10^6))
comb.lith10 <- comb
comb.lith10$count1 [comb.lith10$count1=="45000"] <- "10000"
comb.lith10$count1 [comb.lith10$count1=="450000"] <- "100000"
comb.lith10$count1 [comb.lith10$count1=="4500000"] <- "1000000"
comb.lith10$count2 [comb.lith10$count2=="45000"] <- "10000"
comb.lith10$count2 [comb.lith10$count2=="450000"] <- "100000"
comb.lith10$count2 [comb.lith10$count2=="4500000"] <- "1000000"

comb.lith10$E_all <- comb.lith10$beta_all*as.numeric(comb.lith10$count2)

resize.win(12,6)
ggplot(comb.lith10 %>% filter (host %in% c("1000", "1e+05")), aes(x=group2, y=group1)) +
  geom_tile(aes(fill = log10(E_all)), colour = "grey50") +
  scale_fill_viridis(option="cividis") +
  ylim(rev(levels(modata$group1))) + labs (fill="log10 encounters per day") + 
  theme_Publication() +
  theme(axis.title = element_blank(), legend.position = "bottom", legend.direction = "horizontal", legend.key.height=unit(1,"line"), legend.key.width = unit (2, "line")) +facet_grid(host~disrate)

ggplot(comb.lith10 %>% filter (host %in% c("1000", "1e+05")) , aes(x=group2, y=group1)) +
  geom_tile(aes(fill = log10(E_all)), colour = "grey50") +  geom_text(size=2.5, color="white", aes(label=formatC(E_all, format = "e", digits = 1))) +
  scale_fill_viridis(option="cividis") +
  ylim(rev(levels(modata$group1))) + labs (fill="log10 encounters per day") + 
  theme_Publication() +
  theme(axis.title = element_blank(), legend.position = "bottom", legend.direction = "horizontal", legend.key.height=unit(1,"line"), legend.key.width = unit (2, "line")) +facet_grid(host~disrate)
