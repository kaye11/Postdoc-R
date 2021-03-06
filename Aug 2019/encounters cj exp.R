## ------------------------------------------------------------------------
setwd("D:/R program")
#values needed 

K= 1.38064852*(10)^-23 #m2 kg/ s2 K boltzmann constant
mu= 1.126*(10)^-3 #kg/m s dynamic viscosity in 18C
v= 1.099*(10)^-6 #m2/s kinemodataic viscosity in 18C
Rehv= 90*(10)^-9 #in m radius virus
Temp = 18+273.15 #temp in kelvin, here assuming 18C
Den_OcM = 1.05 #g/cm3 density organic cell modatater
Den_CH2O= 1.025 #g/cm3 density seawater at 18C

source("inspack.R")

#1. make a data frame
modata <- expand.grid (cell1=as.factor (c("Nc", "Li")), cell2=as.factor (c("Nc", "Li")), 
                       num1=c (1 %o% 10^(seq(3,7, 1))),  
                       num2=c (1 %o% 10^(seq(3,6, 1))))

modata <- modata %>%
  mutate(rad1 = case_when(cell1 == "Nc" ~ 1.8E-6,
                         cell1 == "Li" ~ 1.3E-6)) %>%
  mutate(rad2 = case_when(cell2 == "Nc" ~ 1.8E-6,
                          cell2 == "Li" ~ 1.3E-6)) %>%
  mutate(den1 = case_when(cell1 == "Nc" ~ 1.08,
                          cell1 == "Li" ~ 2))%>%
  mutate(den2 = case_when(cell2 == "Nc" ~ 1.08,
                          cell2 == "Li" ~ 2))

# ------------------------------------------------------------------------
##differential settling
#calc sinkvel
modata$SinkVel1 <- ((2*((modata$rad1*100)^2)*(981)*(modata$den1-Den_CH2O))/(9*(mu*10)))*864 #meter per day
modata$SinkVel2 <- ((2*((modata$rad2*100)^2)*(981)*(modata$den2-Den_CH2O))/(9*(mu*10)))*864 #meter per day
modata$beta_DS <- (pi*(((modata$rad1+modata$rad2)*100)^2)*(abs((modata$SinkVel1-modata$SinkVel2)/864)))*86400 #in encounters cm3/day

modata$num1f=formatC (modata$num1, format="e", digits=0)
modata$num2f=formatC (modata$num2, format="e", digits=0) 

modata$E_DS <- modata$beta_DS*modata$num1

resize.win(6,6)
ggplot(modata  %>% filter (cell1 %in% c ("Li")) %>% filter (cell2 %in% c ("Nc")), 
       aes(y=log10(num1), x=log10(num2))) +
  geom_tile(aes(fill = log10(E_DS)), colour = "grey50") +
  scale_fill_viridis(option="cividis") + 
  labs (title = "host:lith encounters", fill="encounters per day") + 
  xlab("Host density") + ylab("Lith density") +
  theme_Publication() + theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.height=unit(1,"line"), legend.key.width = unit (2, "line"))

#Brownian motion (BM)
modata$beta_BM <- ((2*(K*(10)^4)*Temp*(((modata$rad1+modata$rad2)*100)^2))/((3*mu*10)*(modata$rad1*modata$rad2*1e4)))*86400 #cm3/d

modata$BM_DS <- modata$beta_BM+modata$beta_DS
modata$E_BM_DS <- modata$BM_DS*modata$num1

ggplot(modata  %>% filter (cell1 %in% c ("Li")) %>% filter (cell2 %in% c ("Nc")), 
       aes(y=log10(num1), x=log10(num2))) +
  geom_tile(aes(fill = log10(E_BM_DS)), colour = "grey50") +
  scale_fill_viridis(option="cividis") + 
  labs (title = "host:lith encounters", fill="encounters per day") + 
  xlab("Host density") + ylab("Lith density") +
  theme_Publication() + theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.height=unit(1,"line"), legend.key.width = unit (2, "line"))
