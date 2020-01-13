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

resize.win(12,6)
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
virus2 <- virus[rep(seq_len(nrow(virus)), 3), ]

navice_alldata.trim$EhV <- virus2$abundance
navice_alldata.trim$slow <- virus2$propslowEhV
navice_alldata.trim$fast <- virus2$propfastEhV
#check if matching is good, or is it TRUE
all(navice_alldata.trim$propslowEhV == virus2$propslowEhV, na.rm=TRUE)

melted_navice <- reshape2::melt(navice_alldata.trim %>% select ("date2", "Infection", "depth", "abundance", "entitycode", "disrate", "beta_BM", "beta_DS", "beta_turb", "beta_all", "EhV" , "slow", "fast"), id.vars=c("date2", "Infection", "depth", "abundance", "entitycode", "disrate", "beta_BM", "beta_DS", "beta_turb", "beta_all", "EhV"))

#this data is melted with fast slow virus yipeeee
#rename variable and value with variable=virus, value=propEhV
melted_navice <- melted_navice %>% rename(virus = variable, propEhV = value)

#edit in excel
write.table(melted_navice, "Postdoc-R/Exported Tables/melted_navice.csv", sep=";", col.names=T, row.names=F)

navice_backup <- melted_navice

melted_navice$propEhV <- as.numeric (melted_navice$propEhV)
melted_navice$encountersEhV <- melted_navice$beta_all*melted_navice$EhV
melted_navice$encounters_propEhV <- melted_navice$beta_all*melted_navice$propEhV

#plot encounters first
#by date
ggplot(melted_navice %>% drop_na(encountersEhV), aes(x=as.factor(date2), y=log10(encountersEhV), color=Infection, shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + labs (y = expression("Encounters "~day^-1~entity^-1), x= "date") + theme_Publication2() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(~entitycode)

ggplot(melted_navice %>% drop_na(encounters_propEhV), aes(x=as.factor(date2), y=log10(encounters_propEhV), color=Infection, shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + labs (y = expression("Encounters "~day^-1~entity^-1), x= "date") + theme_Publication2() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(virus~entitycode)

#by infection
ggplot(melted_navice %>% drop_na(encountersEhV), aes(x=entitycode, y=log10(encountersEhV), color=Infection, shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + labs (y = expression("Encounters "~day^-1~entity^-1), x= "entity") + theme_Publication2() + theme(legend.title = element_blank()) #+ facet_grid(~entitycode)

#overlay encounters and cell density
ylim.prim <- c(0, 92000)   # in this example, Ehux
ylim.sec <- c(0,5)    # in this example, encounters
b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])

ggplot(melted_navice %>% drop_na(encountersEhV), aes(x=as.factor(date2), y=log10(abundance), color=Infection)) +geom_boxplot() + geom_point(aes(y = log10(encountersEhV), shape = entitycode), size=4) + scale_y_continuous("log10 entity per ml", sec.axis = sec_axis(~ (. - a)/b, name = "encounter rates")) + theme_Publication2() + labs (x="date") + theme (axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(~entitycode) 

#focus on early infections
navice_EI <- melted_navice %>% filter (Infection %in% c("Early Infection", "Early Infection 2", "Early Infection 3"))

#overlay encounters and cell density
ggplot(navice_EI %>% drop_na(encountersEhV), aes(x=as.factor(date2), y=log10(abundance), color=Infection)) +geom_boxplot() + geom_point(aes(y = log10(encountersEhV), shape = entitycode), size=4) + scale_y_continuous("log10 entity per ml", sec.axis = sec_axis(~ (. - a)/b, name = "encounter rates")) + theme_Publication2() + labs (x="date") + theme (axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(~entitycode) + scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38" ))

ggplot(navice_EI %>% drop_na(encounters_propEhV), aes(x=as.factor(date2), y=log10(abundance), color=Infection)) +geom_boxplot() + geom_point(aes(y = log10(encounters_propEhV), shape = entitycode), size=4) + scale_y_continuous("log10 entity per ml", sec.axis = sec_axis(~ (. - a)/b, name = "encounter rates")) + theme_Publication2() + labs (x="date") + theme (axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(entitycode~virus) + scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38" ))

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
probs <- as.data.frame(list (entitycode = as.factor(c("Nc", "Cc", "Li", "Nc", "Cc", "Li", "Nc", "Cc", "Li", "Nc", "Cc", "Li")), ads= c(0.05, 0.13, 0.36, 0.05, 0.13, 0.36, 0.05, 0.13, 0.36, 0.05, 0.13, 0.36), virus = c("slow","slow", NA, "fast", "fast", NA, "slow","slow", NA, "fast", "fast", NA), inf = c(0.3, 0.3, NA, 0.06, 00.6, NA, 0.3, 0.3, NA, 0.06, 0.06, NA), hst = c(0.25, 0.25, NA, 1, 1, NA, 1, 1, NA, 0.25, 0.25, NA)))

navice_EI$entitycode <- as.factor(navice_EI$entitycode)

probs$bioinf <- probs$ads*probs$inf*probs$hst

#navice_EI_backup <- navice_EI
navice_EI <- navice_EI_backup

#join navice_EI and probs
navice_EI <- left_join(navice_EI, probs)

#calculate total effects of bioinf
navice_EI$sucinf <- navice_EI$encountersEhV*navice_EI$bioinf
navice_EI$sucinf_prop <- navice_EI$encounters_propEhV*navice_EI$bioinf

#plot successful infection first
#by date
ggplot(navice_EI %>% drop_na(sucinf_prop) , aes(x=as.factor(date2), y=log10(sucinf_prop), color=Infection, shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + labs (y = expression("successful infection "~day^-1~entity^-1), x= "date") + theme_Publication2() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(virus~entitycode) +  scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38" ))

#by infection
ggplot(navice_EI %>% drop_na(sucinf_prop), aes(x=entitycode, y=log10(sucinf_prop), color=Infection, shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + labs (y = expression("successful infection "~day^-1~entity^-1), x= "entity") + theme_Publication2() + theme(legend.title = element_blank()) + facet_grid(~virus) + scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38" ))

#overlay encounters and cell density
ylim.prim2 <- c(0, 92000)   # in this example, Ehux
ylim.sec2 <- c(0,0.04)    # in this example, encounters
b2 <- diff(ylim.prim2)/diff(ylim.sec2)
a2 <- b2*(ylim.prim2[1] - ylim.sec2[1])

#by date
ggplot(navice_EI %>% drop_na(sucinf_prop), aes(x=as.factor(date2), y=log10(abundance), color=Infection)) +geom_boxplot() + geom_point(aes(y = log10(sucinf_prop), shape = entitycode), size=4) + scale_y_continuous("log10 entity per ml", sec.axis = sec_axis(~ (. - a)/b, name = "successful infection per day")) + theme_Publication2() + labs (x="date") + theme (axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(virus~entitycode) + scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38" ))

#by infection
ggplot(navice_EI %>% drop_na(sucinf_prop), aes(x=entitycode, y=log10(abundance), color=Infection)) +geom_boxplot() + geom_point(aes(y = log10(sucinf_prop), shape = entitycode), size=4, position = position_jitterdodge()) + scale_y_continuous("log10 entity per ml", sec.axis = sec_axis(~ (. - a)/b, name = "successful infection per day")) + theme_Publication2() + labs (x="date") + theme (legend.title = element_blank()) + facet_grid(~virus) + scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38" ))


#melt data to get summary of variables
melted_EI3 <- reshape2::melt(navice_EI %>% select ("date2", "Infection", "entitycode", "virus", "abundance", "encounters_propEhV", "sucinf_prop"), id.vars=c("date2", "Infection", "entitycode", "virus"))

navice_EI.sum <- summarySE (melted_EI3, measurevar = "value", groupvars =c("date2", "Infection", "entitycode", "variable", "virus"), na.rm=TRUE)

ggplot(navice_EI.sum, aes(x=date2, y=log10(value), color=virus, shape=virus, fill=virus)) + geom_point (size=4)  + geom_errorbar(aes(ymin=log10(value-se), ymax=log10(value+se)), width=0.5, size=1) + geom_smooth(aes(group=virus)) + facet_grid(entitycode~variable) + theme_Publication2() + theme (axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) 

melted_EI3$variable <- factor (melted_EI3$variable,levels= c("abundance", "encounters_propEhV",
                                                             "sucinf_prop"), 
                               labels = c("entity abundance", "encounters per entity per day", "encounters leading to successful infection"))

resize.win(12,9)
ggplot(melted_EI3 %>% filter(!(date2 %in% c("06-30"))), aes(x=date2, y=log10(value), color=virus, shape=virus)) + geom_boxplot() + geom_point (position=position_jitterdodge()) + theme_Publication2() + theme (axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(entitycode~variable)

ggplot(melted_EI3 %>% filter(!(date2 %in% c("06-30"))), aes(x=Infection, y=log10(value), color=virus, shape=virus))  + geom_boxplot() + geom_point (position=position_jitterdodge()) + facet_grid(entitycode~variable) + theme_Publication2() + theme (legend.title = element_blank()) 

write.table(navice_EI, "Postdoc-R/Exported Tables/navice_EI.csv", sep=";", col.names=T, row.names=F)


#just taking beta summaries
beta.sum <- summarySE (navice_alldata, measurevar = "beta_all", groupvars=c("Infection", "entitycode"))
