##wind data NA-VICE

wind.sum <- summarySE(data=navice_alldata, measurevar = "disrate", group=c("depth"))
resize.win(6,6)
#this is the only one that works

ggplot(wind.sum, aes(y=log10(disrate), x=depth)) + geom_point (size=6)  + geom_errorbar(aes(ymin=log10(disrate-se), ymax=log10(disrate+se)), width=2, size=1.3) + coord_flip() + scale_y_continuous(position = 'left') + theme_Publication() + scale_x_continuous(breaks = c (0, -10, -20, -30))+ labs (y=expression("log10 dissipation rate"~ (m^2~s^-3)), x="depth (m)") + geom_smooth(color="#525252", size=1.1)

#+ geom_smooth(color="#2C6700", fill="#2C6700", size=1, alpha=0.2)

resize.win(12,6) #saved

resize.win (9,4.5)
#all counts, saved
#BOXPLOTS WITH SMOOTH##

#boxplot, count with smooth
ggplot(navice %>% filter (entityperml %in% c("Ehux_total", "EhVIntra", "lith")), aes(x=date2, y=log10(abundance), color=Infection)) + geom_boxplot()+geom_point (size=4)  + geom_smooth( method="loess", se=TRUE, aes(group=1),  color="#525252") +
  labs (y = expression(log10~"entities "~mL^-1), x= "date", color="infection phase") + theme_Publication() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), legend.title = element_blank()) + facet_wrap(entitycode~., scales="free") +
  scale_color_manual (values=c("#CC79A7","#fdb462","#7fc97f", "#D55E00","#662506"))

melted_navice.sum.enc <- summarySE (melted_navice %>% filter (!(encountersEhV %in% c(NA))), measurevar = "encountersEhV", groupvars =c("date2", "Infection", "entitycode"))

#arranging it will destroy dates
#library(DescTools)
#melted_navice.sum.enc$entitycode <- reorder.factor (melted_navice.sum$entitycode, new.order = c("Nc", "Cc", "Li")) 


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


##melt all EIs

melted_EI3$entitycode <- reorder.factor (melted_EI3$entitycode, new.order = c("Nc", "Cc", "Li")) 

resize.win(9,6)
ggplot(melted_EI3, aes(x=entitycode, y=log10(value), color=entitycode, shape=entitycode))  + geom_boxplot() + geom_point (position=position_jitterdodge()) + facet_grid(virus~variable) + theme_Publication() + theme (legend.title = element_blank(), axis.title.x = element_blank(), legend.position = "none") + labs (y="log10 value") + scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) +   geom_hline(yintercept = log10(1), linetype="dashed") 
  
