## ------------------------------------------------------------------------
setwd("D:/R program")
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
hostnum <- (10)^4 #change this as needed
virnum <- hostnum*10
lithnum <- hostnum*50

require (ggplot2)
require(plotly)
require(grid)
require(ggthemes)
require(plyr)
require (dplyr)
require (viridis)
source ("theme_Publication.R")
source("resizewin.R")
resize.win(12,9)
grid.newpage()


## ------------------------------------------------------------------------
#Brownian motion (modata)
#1. make a data frame
require(utils)

modata <- expand.grid(group1= as.factor(c("Cc", "Nc", "Li", "Vi")), group2= as.factor(c("Cc", "Nc", "Li", "Vi")))

modata <- modata %>%
  mutate(rad1 = case_when(group1 == "Nc" ~ 1.8E-6,
                         group1 == "Cc" ~ 2.3E-6, 
                         group1 == "Li" ~ 1.5E-6,
                         group1 == "Vi" ~ 2E-9)) %>%
  mutate(rad2 = case_when(group2 == "Nc" ~ 1.8E-6,
                         group2 == "Cc" ~ 2.3E-6, 
                         group2 == "Li" ~ 1.5E-6,
                         group2 == "Vi" ~ 2E-9)) %>%
  mutate(count1 = case_when(group1 == "Nc" ~ hostnum,
                          group1 == "Cc" ~ hostnum, 
                          group1 == "Li" ~ lithnum,
                          group1 == "Vi" ~ virnum))%>%
  mutate(count2 = case_when(group2 == "Nc" ~ hostnum,
                            group2 == "Cc" ~ hostnum, 
                            group2 == "Li" ~ lithnum,
                            group2 == "Vi" ~ virnum))%>%
  mutate(den1 = case_when(group1 == "Nc" ~ 1.06,
                            group1 == "Cc" ~ 1.28, ##average of mod calc and strongly calc uses 
                            group1 == "Li" ~ 1.25,
                            group1 == "Vi" ~ Den_CH2O))%>%
  mutate(den2 = case_when(group2 == "Nc" ~ 1.06,
                         group2 == "Cc" ~ 1.28, ##average of mod calc and strongly calc uses 
                         group2 == "Li" ~ 1.25,
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

modataext$E_all <- modataext$beta_all*modataext$count1
modataext$disratef <- as.factor (modataext$disrate)

ggplot (data=modataext %>% filter (group1 %in% c ("Nc")) %>% filter (!(group2 %in% c("Nc"))), aes(x=log10(disrate),y = log10(E_all), color=group2)) + geom_line() + geom_point ()

ggplot(modataext %>% filter(disrate %in% c("0.001")), aes(group1, group2)) +
  geom_tile(aes(fill = log10(E_all)), colour = "grey50") +
  scale_fill_viridis(option="cividis") +
  ylim(rev(levels(modata$group2))) +
  theme_Publication() +
  theme(axis.title = element_blank(), legend.position = "right", legend.title= element_blank(), legend.direction = "vertical", legend.key.height=unit(2.5,"line"), legend.key.width = unit (1.5, "line"))

#export data
setwd("D:/R program")
require(openxlsx)
write.xlsx(modataext, file = "Postdoc-R/Exported Tables/encounters 10 to the 4 host.xlsx")


##-------------STOP HERE---------------------------------------------------------##
#readin data
library(readxl)
three <- read_excel("Postdoc-R/Exported Tables/encounters 10 to the 3 host.xlsx")
four <- read_excel("Postdoc-R/Exported Tables/encounters 10 to the 4 host.xlsx")
five <- read_excel("Postdoc-R/Exported Tables/encounters 10 to the 5 host.xlsx")

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
ggplot(comb, aes(x=group2, y=group1)) +
  geom_tile(aes(fill = log10(E_all)), colour = "grey50") +
  scale_fill_viridis(option="cividis") +
  ylim(rev(levels(modata$group1))) + labs (fill="log10 encounters per day") + 
  theme_Publication() +
  theme(axis.title = element_blank(), legend.position = "bottom", legend.direction = "horizontal", legend.key.height=unit(1,"line"), legend.key.width = unit (2, "line")) +facet_grid(host~disrate)


###--------------------------STOP HERE-------------------------------##
##make Heidi's graph (virus density vs. disrate)
#change as necessary
virdis <- five %>% filter (group2=="Vi")

expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))
viralcount <- rep_len (c (1 %o% 10^(seq(2,7, 1))), length.out=6)
virdis2 <- expand.grid.df(virdis, viralcount)

virdis2$E_allvir <- virdis2$beta_all*virdis2$y

resize.win(12,9)
ggplot(virdis2 , aes(y=log10(y), x=disratef)) +
  geom_tile(aes(fill = log(E_allvir)), colour = "grey50") +
  scale_fill_viridis(option="cividis") +
  theme_Publication() + labs (fill="log10 encounters per day", x= "dissipation rate", y="log10 virus density") + 
  theme(legend.direction = "horizontal", legend.key.height=unit(1.5,"line"), legend.key.width = unit (2.5, "line")) + facet_wrap(~group1, ncol=2)

#check experimental density
a600<- virdis2 %>% filter (group1=="Nc") %>% filter(disratef=="0.001")
a350<- virdis2 %>% filter (group1=="Nc") %>% filter(disratef=="1e-04")
astill<- virdis2 %>% filter (group1=="Nc") %>% filter(disratef=="1e-08")

nc <- virdis2 %>% filter (group1=="Nc")

ggplotly(ggplot(data=virdis2 %>% filter (group1=="Nc"), 
       aes(x=disratef, y=E_allvir, colour=as.factor(y), shape=as.factor(y))) +
  geom_point(size=5))

resize.win (10,8)
ggplot(data=virdis2 %>% filter (group1=="Nc"), 
       aes(x=log10(disrate), y=log10(E_allvir), colour=as.factor(y), shape=as.factor(y))) +
  geom_point(size=5) + labs (x= "dissipation rate", y= "log10 encounters per day") +
  theme_Publication() +
  guides(shape=guide_legend(title="virus density"), color=guide_legend(title="virus density")) +
  theme (legend.position = "right", legend.direction = "vertical")

####---USELESS LOOPING BUT COULD BE OF USE SOMETIME IN THE FUTURE------######
###LOOP

# list of values to loop over
modataext$disratef <- as.factor (modataext$disrate)
uniq_disrate <- unique(modataext$disratef)

# Loop
plot.list = list()
dir = "D:/R program/Postdoc-R/PDF and tiffs/theoretical/diff densities all disrates/10E3/"
setwd(dir)

for (i in uniq_disrate) { 
  
  temp_plot =  ggplot(data= subset(modataext, disrate == i)) + 
    geom_tile(aes(x=group2, y=group1, fill = log10(E_all)), colour = "grey50") +
    scale_fill_viridis(option="cividis") +
    ylim(rev(levels(modata$group1))) +
    theme_Publication() +
    theme(axis.title = element_blank(), legend.position = "none") +
    ggtitle(i)
  plot.list [[i]] = temp_plot
  ggsave(temp_plot, filename=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

temp_legend <- ggplot(modataext %>% filter(disrate %in% c("0.001")), aes(group2, group1)) +
  geom_tile(aes(fill = log10(E_all)), colour = "grey50") +
  scale_fill_viridis(option="cividis") +
  ylim(rev(levels(modata$group1))) + labs (fill="log10 encounters per day") + 
  theme_Publication() +
  theme(axis.title = element_blank(), legend.position = "bottom", legend.direction = "horizontal", legend.key.height=unit(1,"line"), legend.key.width = unit (2, "line"))

library(gridExtra)
# create get_legend function
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(temp_legend)

#make one plot for everything
resize.win(16,12)
grid.arrange(grobs=c(plot.list,list(legend)))