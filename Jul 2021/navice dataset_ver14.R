#simulation NAVICE

## ------------------------------------------------------------------------
######add betas inside data frame, trim data at 30m, plot entity concentrations
source("inspack.R")
navice_orig <- read.csv("D:/R program/Postdoc-R/Exported Tables/navice_alldata_foranalysis_mar2021.csv", sep=";") 

#get data for betas
BM <- read.csv("D:/R program/Postdoc-R/Exported Tables/beta_master9_BM.csv", sep=";")
DS <- read.csv("D:/R program/Postdoc-R/Exported Tables/beta_master9_DS.csv", sep=";")

##add betas
navice <- left_join (navice_orig, BM) %>% left_join(., DS %>% select (group, beta_DS))

#change dates
navice$date <- as.Date(navice$date)
navice$date <-format(navice$date , format="%m-%d")
navice$date2 <- factor(navice$date, levels=c("06-30","07-01", "07-02", "07-03", "07-04", "07-05", "07-07", "07-08", "07-09","07-10", "07-06","07-12","06-24", "06-25", "06-27"))

#summary and trend
navice$abundance <- as.numeric(navice$abundance)
navice.sum <- summarySE (navice %>% filter (!(abundance %in% c(NA))), measurevar = "abundance", groupvars =c("date", "Infection", "group"))

#trim data at 30m
navice_30m <- navice %>% filter (depth>=(-30)) 

#calculate beta turb
v= 1.099*(10)^-6 #m2/s kinematic viscosity in 18C
Rehv= 90*(10)^-9 #in m radius virus

navice_30m$beta_turb.temp <- (0.42*pi*((navice_30m$disrate/(v))^0.5)*((navice_30m$rad+Rehv)^3))*86400*10^6 

navice_30m<- data.frame(navice_30m, beta_turb = ifelse(navice_30m$group %in% c("Cc", "Li"), 
                                                       navice_30m$beta_turb.temp, "NA")) 

navice_30m$beta_turb <- as.numeric(as.character(navice_30m$beta_turb))
navice_30m$beta_turb.temp <- NULL

navice_30m$beta_all <- navice_30m$beta_BM + navice_30m$beta_DS + navice_30m$beta_turb

navice_30m$group = factor(navice_30m$group, levels=c("Cc", "Li", "Vi"))

#entity concentration plot
navice_30m$group <- reorder.factor (navice_30m$group, new.order = c("Cc", "Li" ,"Vi"))

navice_30m$group2 <- factor (navice_30m$group, levels = c("Cc", "Li", "Vi"),
                             labels = c("E. huxleyi", "coccoliths", "EhVs"))

navice$Infection <- factor (navice$Infection, levels = c("Early Infection", "Early Infection 2", "Early Infection 3", "Late Infection","Post Bloom"), labels = c("Early Infection 1", "Early Infection 2", "Early Infection 3", "Late Infection","Post Infection"))

resize.win(11.5,4.5)
ggplot(navice_30m, aes(x=date2, y=log10(abundance), color=Infection)) + geom_boxplot() + geom_point (size=4)  + 
  geom_smooth(aes(group=1), color="#525252", method="loess") +
  labs (y = expression(log[10]~"concentration "~"mL"^-1), x= "date", color="infection phase") + theme_Publication2() +
  theme(legend.title=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),  
        panel.spacing.x = unit(0.5, "lines"),legend.margin=margin(0,0,0,0), axis.ticks.length = unit(5, "pt"),
        strip.text=element_text(face="italic")) + 
  facet_wrap(group2~., scales="free") +
  scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506"))


## ------------------------------------------------------------------------
#calculate encounters, for this remove EhV data
virus <- navice_30m %>% filter (group=="Vi")
virus$propfastEhV <- virus$abundance*virus$fastEhV
virus$propslowEhV <- virus$abundance*virus$slowEhV

navice_30m.trim <- navice_30m %>% filter (!(group %in% c("Vi"))) %>% select (-c(fastEhV, slowEhV))
virus2 <- virus[rep(seq_len(nrow(virus)), 2), ]

navice_30m.trim$EhV <- virus2$abundance
navice_30m.trim$slow <- virus2$propslowEhV
navice_30m.trim$fast <- virus2$propfastEhV
#check if matching is good, or is it TRUE
#all(navice_30m.trim$propslowEhV == virus2$propslowEhV, na.rm=TRUE)

melted_navice <- reshape2::melt(navice_30m.trim %>% select ("date", "Infection", "depth", "abundance", "group", "group2", "disrate", "beta_BM", "beta_DS", "beta_turb", "beta_all", "EhV" , "slow", "fast"), id.vars=c("date", "Infection", "depth", "abundance", "group", "group2","disrate", "beta_BM", "beta_DS", "beta_turb", "beta_all", "EhV"))

#this data is melted with fast slow virus 
#rename variable and value with variable=virus, value=propEhV
melted_navice <- melted_navice %>% rename(virus = variable, propEhV = value)

write.table(melted_navice, "Postdoc-R/Exported Tables/melted_navice_30m_v14.csv", sep=";", col.names=T, row.names=F)

#focus on early infections
navice_EI <- melted_navice %>% filter (Infection %in% c("Early Infection 1", "Early Infection 2", "Early Infection 3"))
navice_EI$virus <- as.factor(navice_EI$virus)

##-----##
###encounters, adsorptions, infections
#beta on encounters are based on beta_all where beta_turb is dependent on disrates

#make a data frame on infectability parameters
probs <- as.data.frame(list (group = as.factor(c("Cc", "Li","Cc", "Li")), virus = c("high","high", "low", "low"), 
                             ads= c(0.63, 0.63, 0.63, 0.63),  inf = c(0.3, NA, 0.06, NA)))

navice_EI$group <- as.factor(navice_EI$group)
navice_EI$virus <- factor (navice_EI$virus,levels= c("slow", "fast"), labels = c("low", "high"))

#join navice_EI and probs
navice_EI <- left_join(navice_EI, probs)

#calculate encounters
navice_EI$enc_prop <- navice_EI$beta_all*navice_EI$propEhV 

#adsorption
navice_EI$adstot_prop <- navice_EI$enc_prop*navice_EI$ads

#sucinf
navice_EI$sucinf_prop <- navice_EI$enc_prop*navice_EI$ads*navice_EI$inf

##trying this
#combined high and low
high <- navice_EI %>% filter (virus=="high")
low <- navice_EI %>% filter (virus=="low")
navice_EI_comb <- high %>% select (c(date, Infection, depth, group, virus, propEhV, abundance))
navice_EI_comb$enccomb <- high$enc_prop + low$enc_prop
navice_EI_comb$adscomb <- high$adstot_prop + low$adstot_prop
navice_EI_comb$infcomb <- high$sucinf_prop + low$sucinf_prop

#melt data to get summary of variables
melted_EI2 <- reshape2::melt(navice_EI_comb %>% select ("date", "Infection", "group", "virus", "propEhV", "abundance", "enccomb", "adscomb", "infcomb"), id.vars=c("date", "Infection", "group", "virus", "propEhV", "abundance"))

abundance <- navice_EI_comb %>% select ("date", "Infection", "group", "virus", "propEhV", "abundance")
abundance$variable <- "abundance"
abundance$value <- abundance$abundance

melted_EI3 <-rbind (melted_EI2, abundance)

#ratio of virus to host
navice_EI$ratvirhost <- navice_EI$propEhV/navice_EI$abundance

###PLOTS
##melt all EIs

melted_EI3$group <- reorder.factor (melted_EI3$group, new.order = c("Cc", "Li")) 
melted_EI3$group2 <- factor (melted_EI3$group, levels = c("Cc", "Li"),
                             labels = c("E. huxleyi", "coccoliths"))

variable_labs <- c(
  `abundance` = 'concentration~(mL^{-1})',
  `enccomb` = 'encounters~entity^{-1}~d^{-1}',
  `adscomb` = 'adsorptions~entity^{-1}~d^{-1}',
  `infcomb` = 'infections~entity^{-1}~d^{-1}'
)

#change labels
resize.win(10, 4)
melted_EI3$variable <-  reorder.factor (melted_EI3$variable, new.order = c("abundance", "enccomb", "adscomb", "infcomb")) 

ggplot(melted_EI3, aes(x=group2, y=log10(value), color=group))  + geom_boxplot() + geom_point (position=position_jitterdodge()) + facet_grid(~variable, labeller = labeller(variable = as_labeller(variable_labs, label_parsed))) + 
  theme_Publication2() + 
  theme (legend.title = element_blank(), axis.title.x = element_blank(), legend.position = "none", 
         axis.text.x = element_text(face="italic")) + 
  labs (y=expression(log[10])) + scale_color_manual (values=c("#5ab4ac", "#d8b365")) 

melted_EI3$percent <- melted_EI3$value*100

resize.win(4, 4)
ggplot(melted_EI3 %>% filter ((variable=="infcomb")) %>% filter (group=="Cc"), 
       aes(x=group2, y=log10(percent), color=group))  + geom_boxplot() + geom_point (position=position_jitterdodge(), size=5) +
  theme_Publication2() + 
  scale_y_continuous(breaks = c(2, 1, 0, -1, -2, -3),label = c(100, 10, 1, 0.1, 0.01, 0.001)) +
  theme (legend.title = element_blank(), axis.title.x = element_blank(), legend.position = "none",  
         axis.text.x = element_blank(), panel.spacing.x = unit(1.5, "lines"),legend.margin=margin(0,0,0,0),
         strip.background.x  = element_blank(), axis.ticks.x = element_blank()) + 
  labs (y=expression("% population infected "~d^-1)) + scale_color_manual (values=c("#5ab4ac")) +  
  geom_hline(yintercept =log10(10), linetype="dashed")

ggplot(melted_EI3 %>% filter ((variable=="adscomb")), aes(x=group2, y=log10(percent), color=group2))  + geom_boxplot() + geom_point (position=position_jitterdodge()) + theme_Publication2() + 
  scale_y_continuous(breaks = c(2, 1, 0, -1, -2, -3),label = c(100, 10, 1, 0.1, 0.01, 0.001)) +
  theme (legend.title = element_blank(), axis.title.x = element_blank(), legend.position = "none",  
         panel.spacing.x = unit(1.5, "lines"),legend.margin=margin(0,0,0,0),strip.background.x  = element_blank(),
         axis.ticks.length = unit(5, "pt"), axis.title.y = element_text(vjust=0.001)) + 
  labs (y=expression("% population with\nadsorbed EhVs"~d^-1)) + scale_color_manual (values=c("#5ab4ac", "#d8b365"))

melted_EI3.sum <- melted_EI3 %>% filter (!(value=="NA")) %>% group_by (group, variable ) %>%  summarise (mean=mean(percent), median=median(percent))

##save datafile
write.table(melted_EI3, "Postdoc-R/Exported Tables/NAVICE_simulation_v14.csv", sep=";", col.names=T, row.names=F)

wind.sum <- summarySE(data=navice_30m, measurevar = "disrate", group=c("depth"))
wind.sum$depth <- abs(wind.sum$depth)

resize.win(5,5)

ggplot(wind.sum, aes(y=log10(disrate), x=depth)) + geom_point (size=6)  + geom_errorbar(aes(ymin=log10(disrate-se), ymax=log10(disrate+se)), width=2, size=1.3) + coord_flip() + scale_y_continuous(position = 'left') + theme_Publication2() + scale_x_continuous(breaks = c (0, 10, 20, 30), trans="reverse")+ labs (y = expression(log[10]~epsilon~(m^2~s^-3)), x="depth (m)") + geom_smooth(color="#525252", size=1.1)

####
##ratio host virus, virus host
lith <- navice_30m %>% filter (group=="Li") %>% select (c("cast", "group", "abundance", "Infection"))
ehux <- navice_30m %>% filter (group=="Cc") %>% select (c("group", "abundance"))
ehv <- navice_30m %>% filter (group=="Vi") %>% select (c("group", "abundance"))

colnames (lith) [2:3] <- c("lith", "lithcount")
colnames (ehux) [1:2] <- c("ehux", "ehuxcount")
colnames (ehv) [1:2] <- c("ehv", "ehvcount")

lithcellehv <- cbind (lith, ehux, ehv) 
lithcellehv$ratvir <- lithcellehv$ehvcount/lithcellehv$ehuxcount 
lithcellehv$ratcoc <- lithcellehv$lithcount/lithcellehv$ehuxcount 

lithcellehv.sum <- summarise (lithcellehv, ratvir.mean=mean(ratvir, na.rm=TRUE), ratcoc.mean=mean(ratcoc, na.rm=TRUE), ratvir.med=median(ratvir, na.rm=TRUE), ratcoc.med=median(ratcoc, na.rm=TRUE))

lithcellehv$Infection.2lines <- factor (lithcellehv$Infection,labels= c("Early\nInfection 1", "Early\nInfection 2", "Early\nInfection 3", "Late\nInfection", "Post\nInfection"))

resize.win(6.5,5)
ggplot (data=lithcellehv, aes(x=Infection.2lines, y=ratcoc, color=Infection)) + 
  geom_boxplot() + geom_point(size=5, position=position_jitterdodge())  +
  labs (y = "coccolith: E. huxleyi ratio", color="infection phase") + theme_Publication2() + scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506")) + guides(colour=guide_legend(nrow=2,byrow=TRUE, title.position = "top")) + theme (legend.title=element_blank(), axis.title.x = element_blank(), legend.position = "none")


#ratio of ehux and virus
ggplot (data=lithcellehv, aes(x=Infection.2lines, y=log10(ratvir), color=Infection)) + 
  geom_boxplot() + geom_point(size=5, position=position_jitterdodge())  +
  labs (y = expression(log[10]~"EhV: E. huxleyi ratio"), color="infection phase") + theme_Publication2() + scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506")) + guides(colour=guide_legend(nrow=2,byrow=TRUE, title.position = "top")) + theme (legend.title=element_blank(), axis.title.x = element_blank(), legend.position = "none")

write.table (lithcellehv, "Postdoc-R/Exported Tables/lithcellehv_ratio_v14.csv", sep=";", col.names=T, row.names=F)

##################--------------------------------------------###################
#zooming in to the navice data#
#depth profile figure of PI vs coccoice

coccoice <- read.csv("D:/Postdoc/theoretical/NA-VICE/PB and coccoice_v2.csv")

#kay preferred with smooth
coccoice$Infection <- reorder.factor (coccoice$Infection, new.order = c("Post Infection", "Cocco Ice"))

resize.win(5.5, 4)
coccoice$depth <- abs(coccoice$depth)
ggplot (data= coccoice %>% filter (depth<(31)), 
        aes (y=log10(abundance), x=depth, color= Infection, fill=Infection, shape=entity)) + 
  geom_point(size=3.5) + 
  geom_smooth(alpha=0.2, method="gam", formula=y~s(x, k=3)) +
  coord_flip () +
  labs (y = expression(log[10]~"concentration "~"mL"^-1), x="depth (m)") + 
  scale_x_continuous(trans="reverse") +
  scale_color_manual(values = c("#662506", "#2b83ba")) + scale_fill_manual(values = c("#662506", "#2b83ba")) +
  theme_Publication2() + theme (legend.title = element_blank()) 

##make boxplot
lithcellehv_coccoice <- read.csv("D:/R program/Postdoc-R/CSV Files/lithcellehv_coccoice.csv")

lithcellehv_coccoice$Infection <- reorder.factor (lithcellehv_coccoice$Infection, new.order = c("Early Infection 1", "Early Infection 2", "Early Infection 3", "Late Infection","Post Infection", "Cocco Ice"))

resize.win(4,4)
ggplot(data=lithcellehv_coccoice %>% filter (Infection %in% c("Post Infection", "Cocco Ice")), aes(color=Infection, x=Infection, y=log10(ratvir))) + geom_boxplot() + geom_point(size=3, position=position_jitterdodge()) +
  scale_color_manual (values=c("#662506", "#2b83ba")) +
  labs (y=expression(log[10]~"EhV: E.huxleyi ratio")) +
  theme_Publication2() + theme (axis.title.x=element_blank(), legend.position = "none") 



       