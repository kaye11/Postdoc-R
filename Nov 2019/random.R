#3b. calculate PIC and density for lith
lithvol <- ((4/3)*pi*(1.5*1.15*0.07))*1e-12 #1.25-lith rad long axis, 1.14 lith rad short axis, 0.07 lith thickness from paper (Johnsen et al 2019) in cm3, from CJ's paper
lithvol <- (pi*(1.25^2)*0.07)*(1e-12) #disc
lithvol <- (0.02*(2.5^3))*1e-12 #in cm3

PIC$perlith <- PIC$PICpercell/14 #in g, assuming 14 liths attached
PIC$perlithpg <- PIC$perlith*1e12 #in pg
#PIC$lithvol <- PIC$perlith/2.6 #in um3
PIC$Denlith_noOCM <- (PIC$perlith/lithvol) #in g/cm3, without organic matter attached
ggplotly(ggplot(data=PIC, aes(x=Strain, y=Denlith_noOCM, color=group)) + geom_boxplot()+geom_point(size=2) 
         +theme_Publication())

#4. calculate sinking velocity of cells,liths, and viruses
PIC$SinkVel <- ((2*((PIC$rad*100)^2)*(981)*(PIC$Den_celltotal-Den_CH2O))/(9*(mu*10)))*864 #meter per day
PIC$SinkVel_lith <- ((2*((Reh_lith*100)^2)*(981)*(2-Den_CH2O))/(9*(mu*10)))*864 #meter per day
Ehv_SinkVel <- ((2*((Rehv*100)^2)*(981)*(Den_CH2O-Den_CH2O))/(9*(mu*10)))*864  #equals to 0
#g is converted to per day, 864 is the one that converts cm/s to m/day
#plot sinking velocity vs calcification


ggplot(data=PIC, aes(x=PICpercellpg, y=SinkVel, color=Strain, shape=group2)) + geom_point(size=5)+theme_Publication()+
  labs(y = expression("Sinking velocity"~("m"~day^-1)), x = expression("PIC pg"~cell^-1)) +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) 

ggplot(data=PIC %>% filter (group=="calcified"), aes(x=perlithpg, y=SinkVel_lith, color=Strain, shape=group2)) + 
  geom_point(size=5)+theme_Publication()+
  labs(y = expression("Sinking velocity"~("m"~day^-1)), x = expression("PIC pg"~lith^-1)) +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank())

#5. calculate betas for DS
PIC$beta_DS <-(pi*(((PIC$rad+Rehv)*100)^2)*(abs((PIC$SinkVel-Ehv_SinkVel)/864)))*86400 #in encounters cm3/day
PIC$beta_DS_lith <- (pi*(((Reh_lith+Rehv)*100)^2)*(abs((PIC$SinkVel_lith-Ehv_SinkVel)/864)))*86400 #in encounters cm3/day

ggplot(data=PIC, aes(x=PICpercellpg, y=beta_DS, color=Strain,  shape=group2)) + geom_point(size=5)+theme_Publication()+
  labs(y = expression(beta~("Encounters"~cm^3~day^-1)), x = expression("PIC pg"~cell^-1))  +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=2),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) + annotation_logticks(sides="l")+
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) 

ggplot(data=PIC %>% filter (group=="calcified"), aes(x=perlithpg, y=beta_DS_lith, color=Strain, shape=group2)) + geom_point(size=5)+theme_Publication()+
  labs(y = expression(beta~("Encounters"~cm^3~day^-1)), x = expression("PIC pg"~lith^-1))  +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=2),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) + annotation_logticks(sides="l")+
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) 

