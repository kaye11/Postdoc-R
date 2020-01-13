#data is wrangled in excel because it is easier

source("inspack.R")
library(readxl)
navice <- read_excel("D:/Postdoc/theoretical/NA-VICE/NAVICE_long_wrangledinexcel.xlsx")

#change dates
navice$date <-format(navice$date , format="%m-%d")
navice$date2 = factor(navice$date, levels=c("06-30","07-01", "07-02", "07-03", "07-04", "07-05", "07-07", "07-08", "07-09","07-10", "07-06","07-12","06-24", "06-25", "06-27"))

#trim data
navice_alldata <- navice %>% filter (!(entityperml %in% c("Ehux_total", "EhVExtra")))

#calculate betas
v= 1.099*(10)^-6 #m2/s kinematic viscosity in 18C
Rehv= 90*(10)^-9 #in m radius virus

navice_alldata$beta_turb.temp <- (4.2*pi*((navice_alldata$disrate/(v*100^2))^0.5)*(((navice_alldata$rad+Rehv)*100)^3))*86400 

navice_alldata<- data.frame(navice_alldata, beta_turb = ifelse(navice_alldata$entitycode %in% c("Cc", "Nc", "Li"), navice_alldata$beta_turb.temp, "NA")) 

navice_alldata$beta_turb <- as.numeric(as.character(navice_alldata$beta_turb))
navice_alldata$beta_turb.temp <- NULL

navice_alldata$beta_all <- navice_alldata$beta_BM + navice_alldata$beta_DS + navice_alldata$beta_turb

navice_alldata$entitycode = factor(navice_alldata$entitycode, levels=c("Cc", "Nc", "Li", "Vi"))

#plot Ehux, EhV, and liths
navice_alldata$abundance <- as.numeric(navice_alldata$abundance)
#by date
ggplot(navice_alldata, aes(x=as.factor(date2), y=log10(abundance), color=Infection, shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + 
  labs (y="log10 entities per ml", x= "date") + theme_Publication2() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(entitycode~.)

#by infection
ggplot(navice_alldata, aes(x=Infection, y=log10(abundance), color=Infection, shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + 
  labs (y="log10 entities per ml", x= "infection stage") + theme_Publication2() + theme (legend.position="none") + facet_grid(entitycode~.)

#summary and trend
navice_alldata$Infection <- as.factor(navice_alldata$Infection)
navice$abundance = as.numeric(navice$abundance)
navice.sum <- summarySE (navice %>% filter (!(abundance %in% c(NA))), measurevar = "abundance", groupvars =c("date2", "Infection", "entitycode", "entityperml"))

ggplot(navice.sum %>% filter (entityperml %in% c("Ehux_total", "EhVIntra", "lith")), aes(x=date2, y=log10(abundance), color=Infection)) + geom_point (size=4)  + geom_errorbar(aes(ymin=log10(abundance-se), ymax=log10(abundance+se)), width=0.25, size=1) + geom_smooth(aes(group=1), color="black", method="loess") +
  labs (y="log10 entities per mL", x= "date") + theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(entitycode~., scales="free")

ggplot(navice.sum %>% filter (entityperml %in% c("Ehux_naked", "Ehux_calc", "EhVIntra", "lith")), aes(x=date2, y=log10(abundance), color=Infection)) + geom_point (size=4)  + geom_errorbar(aes(ymin=log10(abundance-se), ymax=log10(abundance+se)), width=0.25, size=1) + geom_smooth(aes(group=1), color="black", method="loess") +
  labs (y="log10 entities per mL", x= "date") + theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(entitycode~., scales="free")

#plot just host
ggplot(navice.sum %>% filter (entityperml %in% c("Ehux_naked", "Ehux_calc")), aes(x=date2, y=log10(abundance), color=Infection)) + geom_point (size=4)  + geom_errorbar(aes(ymin=log10(abundance-se), ymax=log10(abundance+se)), width=0.25, size=1) + geom_smooth(aes(group=1), color="black", method="loess") +
  labs (y="log10 entities per mL", x= "date") + theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(entitycode~.)

#calculate encounters, for this remove EhVdata
virus <- navice_alldata %>% filter (entitycode=="Vi")
navice_alldata.trim <- navice_alldata %>% filter (!(entitycode %in% c("Vi"))) %>% select (-c(fastEhV, slowEhV, propfastEhV, propslowEhV))
navice_alldata$encounters <- navice_alldata$beta_all*(navice_alldata %>% filter (entitycode=="Vi") %>% select ("abundance"))
virus2 <- virus$abundance
navice_alldata.trim$EhV <-rep_len (virus2, length.out = 381)
navice_alldata.trim$encounters <- navice_alldata.trim$beta_all*navice_alldata.trim$EhV

#plot encounters first
#by date
ggplot(navice_alldata.trim %>% drop_na(encounters), aes(x=as.factor(date2), y=log10(encounters), color=Infection, shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + labs (y = expression("Encounters "~day^-1~entity^-1), x= "date") + theme_Publication2() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(~entitycode)

#by infection
ggplot(navice_alldata.trim %>% drop_na(encounters), aes(x=entitycode, y=log10(encounters), color=Infection, shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + labs (y = expression("Encounters "~day^-1~entity^-1), x= "entity") + theme_Publication2() + theme(legend.title = element_blank()) #+ facet_grid(~entitycode)

#overlay encounters and cell density
ylim.prim <- c(0, 100000)   # in this example, Ehux
ylim.sec <- c(0,5)    # in this example, encounters
b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])

ggplot(navice_alldata.trim %>% drop_na(encounters), aes(x=as.factor(date2), y=log10(abundance), color=Infection)) +geom_boxplot() + geom_point(aes(y = log10(encounters), shape = entitycode), size=4) + scale_y_continuous("log10 entity per ml", sec.axis = sec_axis(~ (. - a)/b, name = "encounter rates")) + theme_Publication2() + labs (x="date") + theme (axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(~entitycode) 

#focus on early infections
navice_EI <- navice_alldata.trim %>% filter (Infection %in% c("Early Infection", "Early Infection 2", "Early Infection 3"))

#overlay encounters and cell density
ggplot(navice_EI %>% drop_na(encounters), aes(x=as.factor(date2), y=log10(abundance), color=Infection)) +geom_boxplot() + geom_point(aes(y = log10(encounters), shape = entitycode), size=4) + scale_y_continuous("log10 entity per ml", sec.axis = sec_axis(~ (. - a)/b, name = "encounter rates")) + theme_Publication2() + labs (x="date") + theme (axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(~entitycode) + scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38" ))

#consider entity concentration as well
navice_EI$encounters_ent <- navice_EI$encounters*navice_EI$abundance

ggplot(navice_EI %>% drop_na(encounters_ent), aes(x=as.factor(date2), y=log10(encounters_ent), color=Infection, shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + labs (y = expression("Encounters "~day^-1~ml^-1), x= "date") + theme_Publication2() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(~entitycode)

##-----##
###BIOLOGICAL INFECTABILITY####
#1. adsorption
#viruses have same adsorption across the board
#entities differ. calculated adsorptivity are Nc=0.05, Cc=0.004, Li=0.36 (see evernote note on how this was calculated)

#2. infectivity
#fast viruses have higher infectivity (0.3), slow viruses have lower infectivity (0.06) from Jozef's paper

#3. host susceptability
#CJ's data 
#calcified cells are less susceptible, infection will go lower (prob=0.25)
#naked cells are more susceptible, infection will go higher (prob=1)

#make a data frame on infectability parameters
probs <- as.data.frame(list (entitycode = as.factor(c("Nc", "Cc", "Li", "Nc", "Cc", "Li", "Nc", "Cc", "Li", "Nc", "Cc", "Li")), ads= c(0.05, 0.04, 0.36, 0.05, 0.04, 0.36, 0.05, 0.04, 0.36, 0.05, 0.04, 0.36), virus = c("slow","slow", NA, "fast", "fast", NA, "slow","slow", NA, "fast", "fast", NA), inf = c(0.3, 0.3, NA, 0.6, 0.6, NA, 0.3, 0.3, NA, 0.6, 0.6, NA), hst = c(0.25, 0.25, NA, 1, 1, NA, 1, 1, NA, 0.25, 0.25, NA)))

navice_EI$entitycode <- as.factor(navice_EI$entitycode)

probs$bioinf <- probs$ads*probs$inf*probs$hst

#navice_EI_backup <- navice_EI
navice_EI <- navice_EI_backup

#join navice_EI and probs
navice_EI <- left_join(navice_EI, probs)

#calculate total effects of bioinf
navice_EI$sucinf <- navice_EI$encounters*navice_EI$bioinf

#plot successful infection first
#by date
ggplot(navice_EI %>% drop_na(sucinf) , aes(x=as.factor(date2), y=log10(sucinf), color=Infection, shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + labs (y = expression("successful infection "~day^-1~entity^-1), x= "date") + theme_Publication2() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(virus~entitycode) +  scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38" ))

#by infection
ggplot(navice_EI %>% drop_na(sucinf), aes(x=entitycode, y=log10(sucinf), color=Infection, shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + labs (y = expression("successful infection "~day^-1~entity^-1), x= "entity") + theme_Publication2() + theme(legend.title = element_blank()) + facet_grid(~virus) + scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38" ))

#overlay encounters and cell density
ylim.prim2 <- c(0, 100000)   # in this example, Ehux
ylim.sec2 <- c(0,0.15)    # in this example, encounters
b2 <- diff(ylim.prim2)/diff(ylim.sec2)
a2 <- b2*(ylim.prim2[1] - ylim.sec2[1])

#by date
ggplot(navice_EI %>% drop_na(sucinf), aes(x=as.factor(date2), y=log10(abundance), color=Infection)) +geom_boxplot() + geom_point(aes(y = log10(sucinf), shape = entitycode), size=4) + scale_y_continuous("log10 entity per ml", sec.axis = sec_axis(~ (. - a)/b, name = "successful infection per day")) + theme_Publication2() + labs (x="date") + theme (axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(virus~entitycode) + scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38" ))

#by infection
ggplot(navice_EI %>% drop_na(sucinf), aes(x=entitycode, y=log10(abundance), color=Infection)) +geom_boxplot() + geom_point(aes(y = log10(sucinf), shape = entitycode), size=4, position = position_jitterdodge()) + scale_y_continuous("log10 entity per ml", sec.axis = sec_axis(~ (. - a)/b, name = "successful infection per day")) + theme_Publication2() + labs (x="date") + theme (legend.title = element_blank()) + facet_grid(~virus) + scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38" ))

#CONSIDER PROPORTIONS OF SLOW AND FAST VIRUSES

navice_EI2 <- navice_EI_backup 
virusprop <- virus %>% filter (Infection %in% c("Early Infection", "Early Infection 2", "Early Infection 3"))
navice_EI2 <- left_join(navice_EI2, virusprop %>% select ("date", "Station", "Infection", "depth","propfastEhV", "propslowEhV"))

write.table(navice_EI2, "Postdoc-R/Exported Tables/navice_EI.csv", sep=";", col.names=T, row.names=F)

#edited data readin
navice_EI3 <- read_excel("D:/Postdoc/theoretical/NA-VICE/navice_EI.xlsx")
navice_EI3$propEhV <- as.numeric(navice_EI3$propEhV)
navice_EI3$encounters <- navice_EI3$beta_all*navice_EI3$propEhV


#join navice_EI3 and probs
navice_EI3$entitycode <- as.factor(navice_EI3$entitycode)
navice_EI3 <- left_join(navice_EI3, probs)

#calculate total effects of bioinf
navice_EI3$sucinf <- navice_EI3$encounters*navice_EI3$bioinf

#plot successful infection first
#by date
navice_EI3$date <-format(navice_EI3$date , format="%m-%d")
navice_EI3$date2 = factor(navice_EI3$date, levels=c("06-30","07-01", "07-02", "07-03", "07-04", "07-05", "07-07", "07-08", "07-09","07-10", "07-06","07-12","06-24", "06-25", "06-27"))

ggplot(navice_EI3 %>% drop_na(sucinf) , aes(x=as.factor(date2), y=log10(sucinf), color=Infection, shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + labs (y = expression("successful infection "~day^-1~entity^-1), x= "date") + theme_Publication2() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(virus~entitycode) +  scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38" ))

#by infection
ggplot(navice_EI3 %>% drop_na(sucinf), aes(x=entitycode, y=log10(sucinf), color=Infection, shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + labs (y = expression("successful infection "~day^-1~entity^-1), x= "entity") + theme_Publication2() + theme(legend.title = element_blank()) + facet_grid(~virus) + scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38" ))

#overlay encounters and cell density
navice_EI3$abundance <- as.numeric (navice_EI3$abundance)
ylim.prim2 <- c(0, 100000)   # in this example, Ehux
ylim.sec2 <- c(0,0.03)    # in this example, encounters
b2 <- diff(ylim.prim2)/diff(ylim.sec2)
a2 <- b2*(ylim.prim2[1] - ylim.sec2[1])

#by date
ggplot(navice_EI3 %>% drop_na(sucinf), aes(x=as.factor(date2), y=log10(abundance), color=Infection)) +geom_boxplot() + geom_point(aes(y = log10(sucinf), shape = entitycode), size=4) + scale_y_continuous("log10 entity per ml", sec.axis = sec_axis(~ (. - a2)/b2, name = "successful infection per day")) + theme_Publication2() + labs (x="date") + theme (axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(virus~entitycode) + scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38" ))

#by infection
ggplot(navice_EI3 %>% drop_na(sucinf), aes(x=entitycode, y=log10(abundance), color=Infection)) +geom_boxplot() + geom_point(aes(y = log10(sucinf), shape = entitycode), size=4, position = position_jitterdodge()) + scale_y_continuous("log10 entity per ml", sec.axis = sec_axis(~ (. - a2)/b2, name = "successful infection per day")) + theme_Publication2() + labs (x="date") + theme (legend.title = element_blank()) + facet_grid(~virus) + scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38" ))

