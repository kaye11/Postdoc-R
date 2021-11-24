#data is wrangled in excel because it is easier

source("inspack.R")
navice_orig <- read.csv("D:/R program/Postdoc-R/Exported Tables/navice_alldata_foranalysis_200629.csv", sep=";") #averaged Cc beta_DS was used

#BM
library(readxl)
BM_master7 <- read_excel("Postdoc-R/Exported Tables/BM_master7.xlsx")

#DS
DS_pred <- read.csv("D:/R program/Postdoc-R/CSV Files/DS_pred_edited_master7.csv")


##change betas inside
navice <- navice_orig %>%
  mutate (beta_BM = case_when (entitycode=="Cc" ~ 5.67e-06,
                               entitycode=="Nc" ~ 4.53e-06,
                               entitycode=="Li" ~ 3.40e-06)) %>%
  mutate (beta_DS = case_when (entitycode=="Cc" ~ 2.49255e-06,
                               entitycode=="Nc" ~ 4.30011e-07,
                               entitycode=="Li" ~ 2.70258e-06))

winddata <- read.csv("D:/R program/Postdoc-R/Exported Tables/winddata_navice.csv", sep=";") #de

#change dates
navice$date <- as.Date(navice$date)
navice$date <-format(navice$date , format="%m-%d")
navice$date2 = factor(navice$date, levels=c("06-30","07-01", "07-02", "07-03", "07-04", "07-05", "07-07", "07-08", "07-09","07-10", "07-06","07-12","06-24", "06-25", "06-27"))

#for checking trends with depth, change x to ads and inf
ggplot(navice_orig %>% filter (entitycode=="Ehux") %>% filter (depth > -50), aes(x=depth, y=log10(abundance)))  + geom_point()  + geom_smooth () +coord_flip()


#trim data
navice <- navice_orig %>% filter (depth>=(-30)) 
navice_alldata <- navice %>% filter (!(entityperml %in% c("Ehux_total", "EhVExtra")))

#calculate betas
v= 1.099*(10)^-6 #m2/s kinematic viscosity in 18C
Rehv= 90*(10)^-9 #in m radius virus

navice_alldata$beta_turb.temp <- (4.2*pi*((navice_alldata$disrate/(v))^0.5)*((navice_alldata$rad+Rehv)^3))*86400*10^6 

navice_alldata<- data.frame(navice_alldata, beta_turb = ifelse(navice_alldata$entitycode %in% c("Cc", "Nc", "Li"), navice_alldata$beta_turb.temp, "NA")) 

navice_alldata$beta_turb <- as.numeric(as.character(navice_alldata$beta_turb))
navice_alldata$beta_turb.temp <- NULL

navice_alldata$beta_all <- navice_alldata$beta_BM + navice_alldata$beta_DS + navice_alldata$beta_turb

navice_alldata$entitycode = factor(navice_alldata$entitycode, levels=c("Cc", "Nc", "Li", "Vi"))

##get the first 30m
#backup navice_alldata
navice_alldata_backup <- navice_alldata

####------THESE ARE ALL THE UPPER 30M-----######
#plot Ehux, EhV, and liths
navice_alldata$abundance <- as.numeric(navice_alldata$abundance)

#summary and trend
navice_alldata$Infection <- as.factor(navice_alldata$Infection)
navice$abundance <- as.numeric(navice$abundance)
navice.sum <- summarySE (navice %>% filter (!(abundance %in% c(NA))), measurevar = "abundance", groupvars =c("date", "Infection", "entitycode", "entityperml"))

navice.sum$entitycode <- factor (navice.sum$entitycode, levels = c("Cc", "Ehux", "Li", "Nc", "Vi"),
                                 labels = c("calcified", "E. huxleyi", "coccolith", "naked", "EhV"))

resize.win(12,6) #saved
ggplot(navice.sum %>% filter (entityperml %in% c("Ehux_total", "EhVIntra", "lith")), aes(x=date, y=log10(abundance), color=Infection)) + geom_point (size=4)  + geom_errorbar(aes(ymin=log10(abundance-se), ymax=log10(abundance+se)), width=0.25, size=1) + geom_smooth(aes(group=1), color="#525252", method="loess") +
  labs (y = expression(log[10]~"concentration "~"(mL"^-1~")"), x= "date", color="infection phase") + theme_Publication2() +
  theme(legend.title=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank()) + facet_wrap(entitycode~., scales="free") +
  scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506"))


#boxplots

navice$entitycode <- reorder.factor (navice$entitycode, new.order = c("Ehux", "Nc", "Cc", "Vi" ,"Li"))

navice$entitycode <- factor (navice$entitycode, levels = c("Cc", "Ehux", "Li", "Nc", "Vi"),
                                 labels = c("calcified", "E. huxleyi", "lith", "naked", "EhVs"))

navice$Infection <- factor (navice$Infection, levels = c("Early Infection", "Early Infection 2", "Early Infection 3", "Late Infection","Post Bloom"), labels = c("Early Infection 1", "Early Infection 2", "Early Infection 3", "Late Infection","Post Infection"))

#entity concentration
resize.win(11.5,4.5)
ggplot(navice %>% filter (entityperml %in% c("Ehux_total", "EhVIntra", "lith")), 
       aes(x=date, y=log10(abundance), color=Infection)) + geom_boxplot() + geom_point (size=4)  + 
  geom_smooth(aes(group=1), color="#525252", method="loess") +
  labs (y = expression(log[10]~"concentration "~"mL"^-1), x= "date", color="infection phase") + theme_Publication2() +
  theme(legend.title=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),  
        panel.spacing.x = unit(0.5, "lines"),legend.margin=margin(0,0,0,0), axis.ticks.length = unit(5, "pt")) + facet_wrap(entitycode~., scales="free") +
  scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506"))

#calculate encounters, for this remove EhVdata
virus <- navice_alldata %>% filter (entitycode=="Vi")

navice_alldata.trim <- navice_alldata %>% filter (!(entitycode %in% c("Vi"))) %>% select (-c(fastEhV, slowEhV, propfastEhV, propslowEhV))
virus2 <- virus[rep(seq_len(nrow(virus)), 3), ]

navice_alldata.trim$EhV <- virus2$abundance
navice_alldata.trim$slow <- virus2$propslowEhV
navice_alldata.trim$fast <- virus2$propfastEhV
#check if matching is good, or is it TRUE
all(navice_alldata.trim$propslowEhV == virus2$propslowEhV, na.rm=TRUE)

melted_navice <- reshape2::melt(navice_alldata.trim %>% select ("date", "Infection", "depth", "abundance", "entitycode", "disrate", "beta_BM", "beta_DS", "beta_turb", "beta_all", "EhV" , "slow", "fast"), id.vars=c("date", "Infection", "depth", "abundance", "entitycode", "disrate", "beta_BM", "beta_DS", "beta_turb", "beta_all", "EhV"))

#this data is melted with fast slow virus yipeeee
#rename variable and value with variable=virus, value=propEhV
melted_navice <- melted_navice %>% rename(virus = variable, propEhV = value)

#edit in excel
write.table(melted_navice, "Postdoc-R/Exported Tables/melted_navice_30m_v11.csv", sep=";", col.names=T, row.names=F)

navice_backup <- melted_navice

melted_navice$propEhV <- as.numeric (melted_navice$propEhV)
melted_navice$EhV <- as.numeric (melted_navice$EhV)
melted_navice$abundance <- as.numeric (melted_navice$abundance)

#focus on early infections
navice_EI <- melted_navice %>% filter (Infection %in% c("Early Infection", "Early Infection 2", "Early Infection 3"))
navice_EI$virus <- as.factor(navice_EI$virus)

##-----##
###encounters, adsorptions, infections
#beta on encounters are based on beta_all where beta_turb is dependent on disrates

#make a data frame on infectability parameters
probs <- as.data.frame(list (entitycode = as.factor(c("Nc", "Cc", "Li", "Nc", "Cc", "Li")), virus = c("high","high", "high", "low", "low", "low"), ads= c(0.0305, 0.0095, 0.0134, 0.0305, 0.0095, 0.0134),  inf = c(0.3, 0.3, NA, 0.06, 0.06, NA)))

navice_EI$entitycode <- as.factor(navice_EI$entitycode)

navice_EI_backup <- navice_EI
navice_EI <- navice_EI_backup

navice_EI$virus <- factor (navice_EI$virus,levels= c("slow", "fast"), labels = c("low", "high"))

#join navice_EI and probs
navice_EI <- left_join(navice_EI, probs)

#calculate encounters
navice_EI$enc_prop <- navice_EI$beta_all*navice_EI$propEhV #entitysense host

#adsorption
navice_EI$adstot_prop <- navice_EI$enc_prop*navice_EI$ads

#sucinf
navice_EI$sucinf_prop <- navice_EI$enc_prop*navice_EI$ads*navice_EI$inf

#ratvir
navice_EI$ratvirtotal <- navice_EI$EhV/navice_EI$abundance

##trying this
#combined high and low
high <- navice_EI %>% filter (virus=="high")
low <- navice_EI %>% filter (virus=="low")
navice_EI_comb <- high %>% select (c(date, Infection, depth, entitycode, virus, propEhV, abundance))
navice_EI_comb$enccomb <- high$enc_prop + low$enc_prop
navice_EI_comb$adscomb <- high$adstot_prop + low$adstot_prop
navice_EI_comb$infcomb <- high$sucinf_prop + low$sucinf_prop

##percentage of parameters
navice_EI_comb$perencounters <- (navice_EI_comb$enccomb)*100
navice_EI_comb$peradsorbed <- (navice_EI_comb$adscomb)*100
navice_EI_comb$perinf <- (navice_EI_comb$infcomb)*100

navice_EI_comb$ratvir <- navice_EI_comb$propEhV/navice_EI_comb$abundance

#melt data to get summary of variables
melted_EI2 <- reshape2::melt(navice_EI_comb %>% select ("date", "Infection", "entitycode", "virus", "propEhV", "abundance", "enccomb", "adscomb", "infcomb"), id.vars=c("date", "Infection", "entitycode", "virus", "propEhV", "abundance"))

abundance <- navice_EI_comb %>% select ("date", "Infection", "entitycode", "virus", "propEhV", "abundance")
abundance$variable <- "abundance"
abundance$value <- abundance$abundance

melted_EI3 <-rbind (melted_EI2, abundance)

navice_EI.sum <- summarySE (melted_EI3, measurevar = "value", groupvars =c("date", "Infection", "entitycode", "variable", "virus"), na.rm=TRUE)

write.table(navice_EI_comb, "Postdoc-R/Exported Tables/navice_EI_comb_upper30m_v11.csv", sep=";", col.names=T, row.names=F)

write.table(melted_EI3, "Postdoc-R/Exported Tables/melted_EI3_v11.csv", sep=";", col.names=T, row.names=F)

navice_EI$ratvirhost <- navice_EI$propEhV/navice_EI$abundance

###PLOTS
##melt all EIs

melted_EI3$entitycode <- reorder.factor (melted_EI3$entitycode, new.order = c("Nc", "Cc", "Li")) 

variable_labs <- c(
  `abundance` = 'concentration~(mL^{-1})',
  `enccomb` = 'encounters~entity^{-1}~d^{-1}',
  `adscomb` = 'adsorptions~entity^{-1}~d^{-1}',
  `infcomb` = 'infections~entity^{-1}~d^{-1}'
)

#change labels
resize.win(10, 4)
melted_EI3$group <- factor (melted_EI3$entitycode,labels= c("naked", "calcified", "lith"))
melted_EI3$variable <-  reorder.factor (melted_EI3$variable, new.order = c("abundance", "enccomb", "adscomb", "infcomb")) 

ggplot(melted_EI3, aes(x=group, y=log10(value), color=entitycode))  + geom_boxplot() + geom_point (position=position_jitterdodge()) + facet_grid(~variable, labeller = labeller(variable = as_labeller(variable_labs, label_parsed))) + 
  theme_Publication2() + 
  theme (legend.title = element_blank(), axis.title.x = element_blank(), legend.position = "none") + labs (y=expression(log[10])) + scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) +   geom_hline(yintercept = log10(1), linetype="dashed") 

#for checking trends with depth, change x to ads and inf
ggplot(navice_EI_comb, aes(x=depth, y=log10(enccomb), color=entitycode))  + geom_point()  + geom_smooth () +coord_flip() + facet_grid(~entitycode) 

#calculate days to encounter
melted_EI3$daysent <- 1/melted_EI3$value #this is what is used
melted_EI3$percent <- melted_EI3$value*100

##plot
variable_labs2 <- c(
  `abundance` = 'concentration~(mL^{-1})',
  `enccomb` = 'to~encounter',
  `adscomb` = 'to~adsorb',
  `infcomb` = 'to~be~infected'
)

#for entity
ggplot(melted_EI3 %>% filter (!(variable=="abundance")), aes(x=group, y=log10(daysent), color=entitycode))  + geom_boxplot() + geom_point (position=position_jitterdodge()) + facet_grid(~variable, labeller = labeller(variable = as_labeller(variable_labs2, label_parsed))) + theme_Publication2() + 
  theme (legend.title = element_blank(), axis.title.x = element_blank(), legend.position = "none",  
         panel.spacing.x = unit(1.5, "lines"),legend.margin=margin(0,0,0,0),strip.background.x  = element_blank(),
         axis.ticks.length = unit(5, "pt")) +
  labs (y=expression(log[10]~"days for an entity")) + scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) +   geom_hline(yintercept = log10(1), linetype="dashed") 

#for entity, encounter only
resize.win(3.5, 4)
ggplot(melted_EI3 %>% filter ((variable=="enccomb")), aes(x=group, y=log10(daysent), color=entitycode))  + geom_boxplot() + geom_point (position=position_jitterdodge()) + theme_Publication2() + 
  theme (legend.title = element_blank(), axis.title.x = element_blank(), legend.position = "none",  
         panel.spacing.x = unit(1.5, "lines"),legend.margin=margin(0,0,0,0),strip.background.x  = element_blank(),
         axis.ticks.length = unit(5, "pt")) +
  labs (y=expression(log[10]~"days to encounter a virus ")) + scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) +   geom_hline(yintercept = log10(1), linetype="dashed") 

write.table(melted_EI3, "Postdoc-R/Exported Tables/melted_navice_30m_v11.csv", sep=";", col.names=T, row.names=F)

ggplot(melted_EI3 %>% filter ((variable=="infcomb")) %>% filter (!(group=="lith")), aes(x=group, y=log10(percent), color=entitycode))  + geom_boxplot() + geom_point (position=position_jitterdodge()) + theme_Publication2() + 
  scale_y_continuous(breaks = c(1, 0, -1, -2, -3, -4),label = c(10, 1, 0.1, 0.01, 0.001, 0.0001)) +
  theme (legend.title = element_blank(), axis.title.x = element_blank(), legend.position = "none",  
         panel.spacing.x = unit(1.5, "lines"),legend.margin=margin(0,0,0,0),strip.background.x  = element_blank(),
         axis.ticks.length = unit(5, "pt")) + 
  labs (y=expression("% population infected "~d^-1)) + scale_color_manual (values=c("#e41a1c", "#377eb8")) 

resize.win(4, 4)

ggplot(melted_EI3 %>% filter ((variable=="adscomb")), aes(x=group, y=log10(percent), color=entitycode))  + geom_boxplot() + geom_point (position=position_jitterdodge()) + theme_Publication2() + 
  scale_y_continuous(breaks = c(1, 0, -1, -2, -3, -4),label = c(10, 1, 0.1, 0.01, 0.001, 0.0001)) +
  theme (legend.title = element_blank(), axis.title.x = element_blank(), legend.position = "none",  
         panel.spacing.x = unit(1.5, "lines"),legend.margin=margin(0,0,0,0),strip.background.x  = element_blank(),
         axis.ticks.length = unit(5, "pt"), axis.title.y = element_text(vjust=0.001)) + 
  labs (y=expression("% population with\nadsorbed EhVs"~d^-1)) + scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a"))

wind.sum <- summarySE(data=navice_alldata, measurevar = "disrate", group=c("depth"))
#this is the only one that works

wind.sum$depth <- abs(wind.sum$depth)

resize.win(5,5)

ggplot(wind.sum, aes(y=log10(disrate), x=depth)) + geom_point (size=6)  + geom_errorbar(aes(ymin=log10(disrate-se), ymax=log10(disrate+se)), width=2, size=1.3) + coord_flip() + scale_y_continuous(position = 'left') + theme_Publication2() + scale_x_continuous(breaks = c (0, 10, 20, 30), trans="reverse")+ labs (y = expression(log[10]~epsilon~(m^2~s^-3)), x="depth (m)") + geom_smooth(color="#525252", size=1.1)

#+ geom_smooth(color="#2C6700", fill="#2C6700", size=1, alpha=0.2)

lith <- navice %>% filter (entityperml=="lith") %>% select (c("entityperml", "abundance", "Infection"))
ehux <- navice %>% filter (entityperml=="Ehux_total") %>% select (c("entityperml", "abundance"))
ehv <- navice %>% filter (entityperml=="EhVIntra") %>% select (c("entityperml", "abundance"))

colnames (lith) [1:2] <- c("lith", "lithcount")
colnames (ehux) [1:2] <- c("ehux", "ehuxcount")
colnames (ehv) [1:2] <- c("ehv", "ehvcount")

lithcellehv <- cbind(lith, ehux, ehv)
lithcellehv$ratvir <- lithcellehv$ehvcount/lithcellehv$ehuxcount 
lithcellehv$ratcoc <- lithcellehv$lithcount/lithcellehv$ehuxcount 

lithcellehv.allsum <- summary(lithcellehv)

lithcellehv.sum <- summarise (lithcellehv, ratvir.mean=mean(ratvir, na.rm=TRUE), ratcoc.mean=mean(ratcoc, na.rm=TRUE), ratvir.med=median(ratvir, na.rm=TRUE), ratcoc.med=median(ratcoc, na.rm=TRUE))

#ratio cell and lith
resize.win(6.5,6)
ggplot (data=lithcellehv, aes(x=log10(ehuxcount), y=log10(lithcount), color=Infection)) + 
  geom_point(size=5)  + geom_smooth(aes(group=1), color="#525252", method="loess") +   
  labs (x = expression(log[10]~"calcified E. huxleyi "~mL^-1), y = expression(log[10]~"free liths "~mL^-1), color="infection phase") + theme_Publication2() + scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506")) + guides(colour=guide_legend(nrow=2,byrow=TRUE, title.position = "top")) + theme (legend.title=element_blank())

lithcellehv$Infection.2lines <- factor (lithcellehv$Infection,labels= c("Early\nInfection 1", "Early\nInfection 2", "Early\nInfection 3", "Late\nInfection", "Post\nInfection"))

resize.win(6.5,5)
ggplot (data=lithcellehv, aes(x=Infection.2lines, y=ratcoc, color=Infection)) + 
  geom_boxplot() + geom_point(size=5, position=position_jitterdodge())  +
  labs (y = "lith:calcified E. huxleyi ratio", color="infection phase") + theme_Publication2() + scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506")) + guides(colour=guide_legend(nrow=2,byrow=TRUE, title.position = "top")) + theme (legend.title=element_blank(), axis.title.x = element_blank(), legend.position = "none")

#ratio of ehux and virus

ggplot (data=lithcellehv, aes(x=Infection.2lines, y=log10(ratvir), color=Infection)) + 
  geom_boxplot() + geom_point(size=5, position=position_jitterdodge())  +
  labs (y = expression(log[10]~"EhV:calcified E. huxleyi ratio"), color="infection phase") + theme_Publication2() + scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506")) + guides(colour=guide_legend(nrow=2,byrow=TRUE, title.position = "top")) + theme (legend.title=element_blank(), axis.title.x = element_blank(), legend.position = "none")

ggplot (data=lithcellehv, aes(x=Infection, y=ratvir, color=Infection))  + geom_boxplot() #+ geom_point(position=position_jitter())

##encounters propEhV, there's no proportion of viruses for the other phases, so it doesnt matter, you only have props for EIs :(

write.table (lithcellehv, "Postdoc-R/Exported Tables/lithcellehv_v11.csv", sep=";", col.names=T, row.names=F)

##################--------------------------------------------###################
#zooming in to the navice data#
#depth profile figure of PI vs coccoice
coccoice <- read.csv("D:/Postdoc/theoretical/NA-VICE/PB and coccoice.csv")
resize.win(7,5)
coccoice.trim <- coccoice %>% filter (depth>(-31))

coccoice_v2 <- read.csv("D:/Postdoc/theoretical/NA-VICE/PB and coccoice_v2.csv")

#kay preferred with smooth
coccoice_v2$Infection <- reorder.factor (coccoice_v2$Infection, new.order = c("Post Infection", "Cocco Ice"))

resize.win(5.5, 4)
coccoice_v2$depth <- abs(coccoice_v2$depth)
ggplot (data= coccoice_v2 %>% filter (depth<(31)), 
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
  labs (y=expression(log[10]~"EhV:E.huxleyi ratio")) +
  theme_Publication2() + theme (axis.title.x=element_blank(), legend.position = "none") 



       