##wind data NA-VICE

wind.sum <- summarySE(data=navice_alldata, measurevar = "disrate", group=c("depth"))
resize.win(6,6)
#this is the only one that works

ggplot(wind.sum, aes(y=log10(disrate), x=depth)) + geom_point (size=6)  + geom_errorbar(aes(ymin=log10(disrate-se), ymax=log10(disrate+se)), width=2, size=1.3) + coord_flip() + scale_y_continuous(position = 'left') + theme_Publication() + scale_x_continuous(breaks = c (0, -10, -20, -30))+ labs (y=expression("log10 dissipation rate"~ (m^2~s^-3)), x="depth (m)") + geom_smooth(color="#525252", size=1.1)

#+ geom_smooth(color="#2C6700", fill="#2C6700", size=1, alpha=0.2)

resize.win(12,6) #saved

#all counts, saved
ggplot(navice.sum %>% filter (entityperml %in% c("Ehux_total", "EhVIntra", "lith")), aes(x=date2, y=log10(abundance), color=Infection)) + geom_point (size=4)  + geom_errorbar(aes(ymin=log10(abundance-se), ymax=log10(abundance+se)), width=0.25, size=1) + geom_smooth(aes(group=1), color="#525252", method="loess") +
  labs (y = expression(log10~"entities "~mL^-1), x= "date", color="infection phase") + theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(entitycode~., scales="free") +
  scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506"))

melted_navice.sum.enc <- summarySE (melted_navice %>% filter (!(encountersEhV %in% c(NA))), measurevar = "encountersEhV", groupvars =c("date2", "Infection", "entitycode"))

#arranging it will destroy dates
#library(DescTools)
#melted_navice.sum.enc$entitycode <- reorder.factor (melted_navice.sum$entitycode, new.order = c("Nc", "Cc", "Li")) 

#encounters, all
ggplot(melted_navice.sum.enc, aes(x=date2, y=log10(encountersEhV), color=Infection)) + geom_point (size=4)  + geom_errorbar(aes(ymin=log10(encountersEhV-se), ymax=log10(encountersEhV+se)), width=0.25, size=1) + geom_smooth(aes(group=1), color="#525252", method="loess") +
  labs (y = expression(log10~"total encounters "~day^-1), x= "date", color="infection phase") + theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(entitycode~., scales="free") +
  scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506")) + geom_hline(yintercept = log10(1), linetype="dashed")

#encounters, just viruses
melted_navice.sum.encvir <- summarySE (melted_navice %>% filter (!(encountersEhV_vironly %in% c(NA))), measurevar = "encountersEhV_vironly", groupvars =c("date2", "Infection", "entitycode"))

ggplot(melted_navice.sum.encvir, aes(x=date2, y=log10(encountersEhV_vironly), color=Infection)) + geom_point (size=4)  + geom_errorbar(aes(ymin=log10(encountersEhV_vironly-se), ymax=log10(encountersEhV_vironly+se)), width=0.25, size=1) + geom_smooth(aes(group=1), color="#525252", method="loess") +
  labs (y = expression("viral encounters " ~day^-1~cell^-1), x= "date", color="infection phase") + theme_Publication2() +   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(entitycode~.) +
  scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506")) + geom_hline(yintercept = log10(1), linetype="dashed")

lith <- navice %>% filter (entityperml=="lith") %>% select (c("entityperml", "abundance", "Infection"))
ehux <- navice %>% filter (entityperml=="Ehux_total") %>% select (c("entityperml", "abundance"))
ehv <- navice %>% filter (entityperml=="EhVIntra") %>% select (c("entityperml", "abundance"))

colnames (lith) [1:2] <- c("lith", "lithcount")
colnames (ehux) [1:2] <- c("ehux", "ehuxcount")
colnames (ehv) [1:2] <- c("ehv", "ehvcount")

lithcell <- cbind(lith, ehux)

#ratio cell and lith
resize.win(6,6)
ggplot (data=lithcell, aes(x=log10(ehuxcount), y=log10(lithcount), color=Infection)) + geom_point(size=4)  + geom_smooth(aes(group=1), color="#525252", method="loess") +   labs (x = expression(log10~"E. huxleyi "~mL^-1), y = expression(log10~"free coccoliths "~mL^-1), color="infection phase") + theme_Publication2() +   scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506")) + guides(colour=guide_legend(nrow=2,byrow=TRUE, title.position = "top"))

#ratio of ehux and virus
allratio <- cbind (ehux, ehv, lith)
ggplot (data=allratio, aes(x=log10(ehuxcount), y=log10(ehvcount), color=Infection)) + geom_point(size=4)  + geom_smooth(aes(group=1), color="#525252", method="loess") +   labs (y = expression(log10~"EhV "~mL^-1), x = expression(log10~"E. huxleyi "~mL^-1), color="infection phase") + theme_Publication2() +   scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506")) + guides(colour=guide_legend(nrow=2,byrow=TRUE, title.position = "top"))

##encounters propEhV, there's no proportion of viruses for the other phases, so it doesnt matter, you only have props for EIs :(

melted_navice.sum.enc_prop <- summarySE (melted_navice %>% filter (!(encounters_propEhV %in% c(NA))), measurevar = "encounters_propEhV", groupvars =c("date2", "Infection", "entitycode", "virus"))

resize.win (11,6)
ggplot(melted_navice.sum.enc_prop, aes(x=date2, y=log10(encounters_propEhV), color=Infection)) + geom_point (size=4)  + geom_errorbar(aes(ymin=log10(encounters_propEhV-se), ymax=log10(encounters_propEhV+se)), width=0.25, size=1) + geom_smooth(aes(group=1), color="#525252", method="loess") +
  labs (y = expression(log10~"total encounters "~day^-1), x= "date", color="infection phase") + theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(entitycode~., scales="free") +
  scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506"))  + facet_grid(virus~entitycode) + geom_hline(yintercept = log10(1), linetype="dashed")


#adsorption

navice_EI.sum.ads <- summarySE (navice_EI %>% filter (!(encounters_propEhV %in% c(NA))), measurevar = "adstot_prop", groupvars =c("date2", "Infection", "entitycode", "virus"))

resize.win (11,6)

library(DescTools)
navice_EI.sum.ads$entitycode <- reorder.factor (navice_EI.sum.ads$entitycode, new.order = c("Nc", "Cc", "Li")) 
ggplot(navice_EI.sum.ads, aes(x=date2, y=log10(adstot_prop), color=Infection)) + geom_point (size=4)  + geom_errorbar(aes(ymin=log10(adstot_prop-se), ymax=log10(adstot_prop+se)), width=0.25, size=1) + geom_smooth(aes(group=1), color="#525252", method="loess") +
  labs (y = expression(log10~"total adsorbed EhVs "~day^-1), x= "date", color="infection phase") + theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(entitycode~., scales="free") +
  scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506"))  + facet_grid(virus~entitycode) + geom_hline(yintercept = log10(1), linetype="dashed")

navice_EI.sum.sucinf <- summarySE (navice_EI %>% filter (!(sucinf_prop %in% c(NA))), measurevar = "sucinf_prop", groupvars =c("date2", "Infection", "entitycode", "virus"))

library(DescTools)
navice_EI.sum.sucinf$entitycode <- reorder.factor (navice_EI.sum.sucinf$entitycode, new.order = c("Nc", "Cc", "Li")) 

ggplot(navice_EI.sum.sucinf, aes(x=date2, y=log10(sucinf_prop), color=Infection)) + geom_point (size=4)  + geom_errorbar(aes(ymin=log10(sucinf_prop-se), ymax=log10(sucinf_prop+se)), width=0.25, size=1) + geom_smooth(aes(group=1), color="#525252", method="loess") +
  labs (y = expression(log10~"total successful infection "~day^-1), x= "date", color="infection phase") + theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(entitycode~., scales="free") +
  scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506"))  + facet_grid(virus~entitycode) + geom_hline(yintercept = log10(1), linetype="dashed")

##melt all EIs

melted_EI3$entitycode <- reorder.factor (melted_EI3$entitycode, new.order = c("Nc", "Cc", "Li")) 

resize.win(9,14)
ggplot(melted_EI3, aes(x=entitycode, y=log10(value), color=entitycode, shape=entitycode))  + geom_boxplot() + geom_point (position=position_jitterdodge()) + facet_grid(virus~variable) + theme_Publication2() + theme (legend.title = element_blank(), axis.title.x = element_blank(), legend.position = "none") + labs (y="log10 value") + scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) +   geom_hline(yintercept = log10(1), linetype="dashed") 
  
##plots like the beta plots, all EIs in one bar

resize.win (4,4)

navice_EI$entitycode <- reorder.factor (navice_EI$entitycode, new.order = c("Nc", "Cc", "Li")) 


ggplot(navice_EI %>% drop_na(abundance), aes(x=as.factor(entitycode), y=log10(abundance), color=entitycode, shape=entitycode)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=1) + labs (y = expression("log10 entities "~day^-1))  + facet_grid(~virus) + theme_Publication2() + theme(axis.title.x = element_blank(),legend.position = "none")+ scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) +   geom_hline(yintercept = log10(1), linetype="dashed") 

ggplot(navice_EI %>% drop_na(encounters_propEhV), aes(x=as.factor(entitycode), y=log10(encounters_propEhV), color=entitycode, shape=entitycode)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=1) + labs (y = expression("log10 total encounters "~day^-1)) + facet_grid(~virus) + theme_Publication2() + theme(axis.title.x = element_blank(),legend.position = "none")+ scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) +   geom_hline(yintercept = log10(1), linetype="dashed") 

ggplot(navice_EI %>% drop_na(adstot_prop), aes(x=as.factor(entitycode), y=log10(adstot_prop), color=entitycode, shape=entitycode)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=1) + labs (y = expression("log10 total adsorbed EhVs "~day^-1)) + facet_grid(~virus) + theme_Publication2() + theme(axis.title.x = element_blank(),legend.position = "none")+ scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) +   geom_hline(yintercept = log10(1), linetype="dashed") 

ggplot(navice_EI %>% drop_na(sucinf_prop), aes(x=as.factor(entitycode), y=log10(sucinf_prop), color=entitycode, shape=entitycode)) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=1) + labs (y = expression("log10 successful infection "~day^-1)) + facet_grid(~virus) + theme_Publication2() + theme(axis.title.x = element_blank(),legend.position = "none")+ scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) +   geom_hline(yintercept = log10(1), linetype="dashed") 

#######
#BOXPLOTS WITH SMOOTH##

#boxplot, count with smooth
ggplot(navice %>% filter (entityperml %in% c("Ehux_total", "EhVIntra", "lith")), aes(x=date2, y=log10(abundance), color=Infection)) + geom_boxplot()+geom_point (size=4)  + geom_smooth( method="loess", se=TRUE, aes(group=1),  color="#525252") +
  labs (y = expression(log10~"entities "~mL^-1), x= "date", color="infection phase") + theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(entitycode~., scales="free") +
  scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506"))

#encounters total
ggplot(melted_navice %>% filter(!(date2 %in% c("06-30"))), aes(x=date2, y=log10(encountersEhV), color=Infection)) + geom_boxplot()+geom_point (size=4)  + geom_smooth( method="loess", se=TRUE, aes(group=1),  color="#525252") +
  labs (y = expression(log10~"total encounters "~day^-1), x= "date", color="infection phase") + 
  theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(entitycode~.) +
  scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506")) +
  geom_hline(yintercept = log10(1), linetype="dashed")

#encounters vironly
ggplot(melted_navice %>% filter(!(date2 %in% c("06-30"))), aes(x=date2, y=log10(encountersEhV_vironly), color=Infection)) + geom_boxplot()+geom_point (size=4)  + geom_smooth( method="loess", se=TRUE, aes(group=1),  color="#525252") +
  labs (y = expression("viral encounters " ~day^-1~cell^-1), x= "date", color="infection phase") + 
  theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(entitycode~.) +
  scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506")) +
  geom_hline(yintercept = log10(1), linetype="dashed")

#only EIs
ggplot(navice_EI %>% filter(!(date2 %in% c("06-30"))), aes(x=date2, y=log10(encountersEhV), color=Infection)) + geom_boxplot()+geom_point (size=4)  + 
  geom_smooth( method="loess", se=TRUE, aes(group=1),  color="#525252") +
  labs (y = expression(log10~"total encounters "~day^-1), x= "date", color="infection phase") + 
  theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(entitycode~.) +
  scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506")) +
  geom_hline(yintercept = log10(1), linetype="dashed")

ggplot(navice_EI %>% filter(!(date2 %in% c("06-30"))), aes(x=date2, y=log10(encountersEhV_vironly), color=Infection)) + geom_boxplot()+geom_point (size=4)  + geom_smooth( method="loess", se=TRUE, aes(group=1),  color="#525252") +
  labs (y = expression("viral encounters " ~day^-1~cell^-1), x= "date", color="infection phase") + 
  theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(entitycode~.) +
  scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506")) +
  geom_hline(yintercept = log10(1), linetype="dashed")

#propEhVs
resize.win (9,7)
ggplot(navice_EI %>% filter(!(date2 %in% c("06-30"))), aes(x=date2, y=log10(encountersEhV), color=Infection)) + geom_boxplot()+geom_point (size=4)  + 
  geom_smooth( method="loess", se=TRUE, aes(group=1),  color="#525252") +
  labs (y = expression(log10~"total encounters "~day^-1), x= "date", color="infection phase") + 
  theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(entitycode~.) +
  scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506")) +
  geom_hline(yintercept = log10(1), linetype="dashed") + facet_grid(virus~entitycode)

ggplot(navice_EI %>% filter(!(date2 %in% c("06-30"))), aes(x=date2, y=log10(encountersEhV_vironly), color=Infection)) + geom_boxplot()+geom_point (size=4)  + geom_smooth( method="loess", se=TRUE, aes(group=1),  color="#525252") +
  labs (y = expression("viral encounters " ~day^-1~cell^-1), x= "date", color="infection phase") + 
  theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(entitycode~.) +
  scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506")) +
  geom_hline(yintercept = log10(1), linetype="dashed") + facet_grid(virus~entitycode)

#adsorption

navice_EI$entitycode <- reorder.factor (navice_EI$entitycode, new.order = c("Nc", "Cc", "Li")) 

ggplot(navice_EI %>% filter(!(date2 %in% c("06-30"))), aes(x=date2, y=log10(adstot_prop), color=Infection)) + geom_boxplot()+geom_point (size=4)  + 
  geom_smooth( method="loess", se=TRUE, aes(group=1),  color="#525252") +
  labs (y = expression(log10~"total adsorbed EhVs "~day^-1), x= "date", color="infection phase") + 
  theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(entitycode~.) +
  scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506")) +
  geom_hline(yintercept = log10(1), linetype="dashed") + facet_grid(virus~entitycode)

#sucinf
resize.win(6,7)
ggplot(navice_EI %>% filter(!(date2 %in% c("06-30"))) %>% filter(!(entitycode %in% c("Li"))) , aes(x=date2, y=log10(sucinf), color=Infection)) + geom_boxplot()+geom_point (size=4)  + 
  geom_smooth( method="loess", se=TRUE, aes(group=1),  color="#525252") +
  labs (y = expression(log10~"successful infection "~day^-1), x= "date", color="infection phase") + 
  theme_Publication2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(entitycode~.) +
  scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506")) +
  geom_hline(yintercept = log10(1), linetype="dashed") + facet_grid(virus~entitycode) + 
  guides(colour=guide_legend(nrow=1,byrow=TRUE, title.position = "top"))

