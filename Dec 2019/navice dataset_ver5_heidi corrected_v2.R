#data is wrangled in excel because it is easier

source("inspack.R")
library(readxl)
navice <- read_excel("D:/Postdoc/theoretical/NA-VICE/NAVICE_long_wrangledinexcel.xlsx")

#change dates
navice$date <-format(navice$date , format="%m-%d")
navice$date2 = factor(navice$date, levels=c("06-30","07-01", "07-02", "07-03", "07-04", "07-05", "07-07", "07-08", "07-09","07-10", "07-06","07-12","06-24", "06-25", "06-27"))

#trim data
navice <- navice%>% filter (depth>=(-30)) 
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

##get the first 30m
#backup navice_alldata
navice_alldata_backup <- navice_alldata

####------THESE ARE ALL THE UPPER 30M-----######
#plot Ehux, EhV, and liths
navice_alldata$abundance <- as.numeric(navice_alldata$abundance)

#by date
ggplot(navice_alldata, aes(x=as.factor(date2), y=log10(abundance), color=Infection, shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + 
  labs (y="log10 entities per ml", x= "date") + theme_Publication2() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(entitycode~.)

#summary and trend
navice_alldata$Infection <- as.factor(navice_alldata$Infection)
navice$abundance <- as.numeric(navice$abundance)
navice.sum <- summarySE (navice %>% filter (!(abundance %in% c(NA))), measurevar = "abundance", groupvars =c("date2", "Infection", "entitycode", "entityperml"))

resize.win(12,6)
ggplot(navice.sum %>% filter (entityperml %in% c("Ehux_total", "EhVIntra", "lith")), aes(x=date2, y=log10(abundance), color=Infection)) + geom_point (size=4)  + geom_errorbar(aes(ymin=log10(abundance-se), ymax=log10(abundance+se)), width=0.25, size=1) + geom_smooth(aes(group=1), color="black", method="loess") +
  labs (y="log10 entities per mL", x= "date", color="Infection Phase") + theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(entitycode~., scales="free")

ggplot(navice.sum %>% filter (entityperml %in% c("Ehux_naked", "Ehux_calc", "EhVIntra", "lith")), aes(x=date2, y=log10(abundance), color=Infection)) + geom_point (size=4)  + geom_errorbar(aes(ymin=log10(abundance-se), ymax=log10(abundance+se)), width=0.25, size=1) + geom_smooth(aes(group=1), color="black", method="loess") +
  labs (y="log10 entities per mL", x= "date") + theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(entitycode~., scales="free")

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
write.table(melted_navice, "Postdoc-R/Exported Tables/melted_navice_30m.csv", sep=";", col.names=T, row.names=F)

navice_backup <- melted_navice

melted_navice$propEhV <- as.numeric (melted_navice$propEhV)
melted_navice$EhV <- as.numeric (melted_navice$EhV)
melted_navice$abundance <- as.numeric (melted_navice$abundance)
melted_navice$encountersEhV <- melted_navice$beta_all*melted_navice$EhV*melted_navice$abundance
melted_navice$encounters_propEhV <- melted_navice$beta_all*melted_navice$propEhV*melted_navice$abundance

#plot encounters first
#by date
ggplot(melted_navice %>% drop_na(encountersEhV), aes(x=as.factor(date2), y=log10(encountersEhV), color=Infection, shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + labs (y = expression("total encounters "~day^-1), x= "date") + theme_Publication2() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(~entitycode)

melted_navice$virus <- factor(melted_navice$virus, levels = c("fast", "slow"))
  
ggplot(melted_navice %>% drop_na(encounters_propEhV), aes(x=as.factor(date2), y=log10(encounters_propEhV), color=Infection, shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + labs (y = expression("total encounters "~day^-1), x= "date") + theme_Publication2() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(virus~entitycode) + scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38"))

#overlay encounters and cell density
ylim.prim <- c(0, 100000)   # in this example, Ehux
ylim.sec <- c(0,100000)    # in this example, encounters
b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])

ggplot(melted_navice %>% drop_na(encountersEhV), aes(x=as.factor(date2), y=log10(abundance), color=Infection)) +geom_boxplot() + geom_point(aes(y = log10(encountersEhV), shape = entitycode), size=4) + scale_y_continuous("log10 entity per ml", sec.axis = sec_axis(~ (. - a)/b, name = "total encounters per day")) + theme_Publication2() + labs (x="date") + theme (axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(~entitycode)

ggplot(melted_navice %>% drop_na(encountersEhV), aes(x=as.factor(date2), y=log10(abundance), color=Infection)) +geom_boxplot() + geom_point(aes(y = log10(encountersEhV), shape = entitycode), size=4) + theme_Publication2() + labs (x="date") + theme (axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(virus~entitycode) 

#+ scale_y_continuous("log10 entity per ml", sec.axis = sec_axis(~ (. - a)/b, name = "total encounters per day")) +

#focus on early infections
navice_EI <- melted_navice %>% filter (Infection %in% c("Early Infection", "Early Infection 2", "Early Infection 3"))

#overlay encounters and cell density
ggplot(navice_EI %>% drop_na(encountersEhV), aes(x=as.factor(date2), y=log10(abundance), color=Infection)) +geom_boxplot() + geom_point(aes(y = log10(encountersEhV), shape = entitycode), size=4) + scale_y_continuous("log10 entity per ml", sec.axis = sec_axis(~ (. - a)/b, name = "encounter rates")) + theme_Publication2() + labs (x="date") + theme (axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(~entitycode) + scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38" ))

ggplot(navice_EI %>% drop_na(encounters_propEhV), aes(x=as.factor(date2), y=log10(abundance), color=Infection)) +geom_boxplot() + geom_point(aes(y = log10(encounters_propEhV), shape = entitycode), size=4) + scale_y_continuous("log10 entity per ml", sec.axis = sec_axis(~ (. - a)/b, name = "total encounters per day")) + theme_Publication2() + labs (x="date") + theme (axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(virus~entitycode) + scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38" ))

##-----##
###BIOLOGICAL INFECTABILITY####
#alpha (ads, inf, hst)
#viruses have same adsorption across the board
#entities differ. calculated alpha are based on cJ's data from his paper. calculations can be found in evernote and formula on thinking notebook. 
# resulting ads*hst for naked cells (1.14e-01), calcified cells (4.82 e-03)
# for liths, ads is alpha which is 1.67e-02

#2. infectivity
#fast viruses have higher infectivity (0.3), slow viruses have lower infectivity (0.06) from Jozef's paper

#make a data frame on infectability parameters
probs <- as.data.frame(list (entitycode = as.factor(c("Nc", "Cc", "Li", "Nc", "Cc", "Li")), virus = c("slow","slow", "slow", "fast", "fast", "fast"), ads= c(1.14e-01, 4.82e-03, 1.67e-02, 1.14e-01, 4.82e-03, 1.67e-02),  inf = c(0.3, 0.3, NA, 0.06, 0.06, NA), hst = c(1, 1, NA, 1, 1, NA)))

probs$bioinf <- probs$ads*probs$inf*probs$hst
navice_EI$entitycode <- as.factor(navice_EI$entitycode)

#navice_EI_backup <- navice_EI
navice_EI <- navice_EI_backup

#join navice_EI and probs
navice_EI <- left_join(navice_EI, probs)

#calculate effects of ads or ads*hst
navice_EI$noinf <- navice_EI$ads*navice_EI$encountersEhV
navice_EI$noinf_prop <- navice_EI$ads*navice_EI$encounters_propEhV

#calculate total effects of bioinf
navice_EI$sucinf <- navice_EI$encountersEhV*navice_EI$bioinf
navice_EI$sucinf_prop <- navice_EI$encounters_propEhV*navice_EI$bioinf

#plotjust ads and ads*hst
navice_EI$entitycode <- factor(navice_EI$entitycode, levels = c("Cc", "Nc", "Li"))

ggplot(navice_EI %>% drop_na(noinf_prop) , aes(x=as.factor(date2), y=log10(noinf_prop), color=Infection, shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + labs (y = expression("adsorbed entities from encounters "~day^-1), x= "date") + theme_Publication2() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(virus~entitycode) +  scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38" ))

#plot successful infection first
#by date
ggplot(navice_EI %>% drop_na(sucinf_prop) , aes(x=as.factor(date2), y=log10(sucinf_prop), color=Infection, shape=Infection)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + labs (y = expression("successful infection "~day^-1), x= "date") + theme_Publication2() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(virus~entitycode) +  scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38" ))

#overlay encounters and cell density
ylim.prim2 <- c(0, 92000)   # in this example, Ehux
ylim.sec2 <- c(0,5)    # in this example, encounters
b2 <- diff(ylim.prim2)/diff(ylim.sec2)
a2 <- b2*(ylim.prim2[1] - ylim.sec2[1])

#by date
ggplot(navice_EI %>% drop_na(sucinf_prop), aes(x=as.factor(date2), y=log10(abundance), color=Infection)) +geom_boxplot() + geom_point(aes(y = log10(sucinf_prop), shape = entitycode), size=4) + scale_y_continuous("log10 entity per ml", sec.axis = sec_axis(~ (. - a)/b, name = "successful infection per day")) + theme_Publication2() + labs (x="date") + theme (axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(virus~entitycode) + scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38" ))

#melt data to get summary of variables
melted_EI3 <- reshape2::melt(navice_EI %>% select ("date2", "Infection", "entitycode", "virus", "abundance", "encounters_propEhV", "noinf_prop", "sucinf_prop"), id.vars=c("date2", "Infection", "entitycode", "virus"))

navice_EI.sum <- summarySE (melted_EI3, measurevar = "value", groupvars =c("date2", "Infection", "entitycode", "variable", "virus"), na.rm=TRUE)

navice_EI.sum2 <- summarySE (melted_EI3, measurevar = "value", groupvars =c("Infection", "entitycode", "variable", "virus"), na.rm=TRUE)

navice_EI.sum3 <- summarySE (melted_EI3, measurevar = "value", groupvars =c("entitycode", "variable", "virus"), na.rm=TRUE)

write.table(navice_EI.sum, "Postdoc-R/Exported Tables/navice_EIsum_30m.csv", sep=";", col.names=T, row.names=F)
write.table(navice_EI.sum3, "Postdoc-R/Exported Tables/navice_EIsum3_30m.csv", sep=";", col.names=T, row.names=F)


ggplot(navice_EI.sum, aes(x=date2, y=log10(value), color=virus, shape=virus, fill=virus)) + geom_point (size=4)  + geom_errorbar(aes(ymin=log10(value-se), ymax=log10(value+se)), width=0.5, size=1) + geom_smooth(aes(group=virus)) + facet_grid(entitycode~variable) + theme_Publication2() + theme (axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) 

melted_EI3$variable <- factor (melted_EI3$variable,levels= c("abundance", "encounters_propEhV", "noinf_prop", "sucinf_prop"), labels = c("entity abundance", "encounters per day", "adsorbed entities per day", "successful infection per day"))
resize.win(16,9)
ggplot(melted_EI3 %>% filter(!(date2 %in% c("06-30"))), aes(x=date2, y=log10(value), color=virus, shape=virus)) + geom_boxplot() + geom_point (position=position_jitterdodge()) + theme_Publication2() + theme (axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) + facet_grid(entitycode~variable) + labs (y="log10 value", x="date")

ggplot(melted_EI3 %>% filter(!(date2 %in% c("06-30"))), aes(x=Infection, y=log10(value), color=virus, shape=virus))  + geom_boxplot() + geom_point (position=position_jitterdodge()) + facet_grid(entitycode~variable) + theme_Publication2() + theme (legend.title = element_blank()) + labs (y="log10 value", x="date")

write.table(navice_EI, "Postdoc-R/Exported Tables/navice_EI_upper30m.csv", sep=";", col.names=T, row.names=F)


#check data
ehux <- navice %>% filter (entityperml %in% c("Ehux_total"))
ehv <- navice %>% filter (entityperml %in% c("EhVIntra"))

cor (ehv$abundance, ehux$abundance, use = "complete.obs") #resulr is 0.03


ggplot() + geom_point(aes(y=log10(ehux$abundance), x=log10(ehv$abundance))) + geom_smooth(method="lm", aes(y=log10(ehux$abundance), x=log10(ehv$abundance)))

