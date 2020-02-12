#summaries
BM #BM summary
DS_pred #predicted data


#for paper
resize.win(6,6)

#PIC vs sinkvel cell, real data, saved SI with lm and no lm
ggplot(data=PIC, aes(x=PICpercellpg, y=SinkVel, color=group2, shape=Strain)) + geom_point(size=7)+theme_Publication2()+
  labs(y = expression("sinking velocity "~("m"~day^-1)), x = expression("PIC pg"~cell^-1)) + 
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) + #geom_smooth (method="lm", aes(group=1), color="#525252") + 
  scale_color_manual(values=c("#e41a1c","#4a1486", "#377eb8")) + scale_shape_manual(values=c(1:11)) #get regression

#regression, 
PIC_SinkVel_reg <- lm (SinkVel ~ PICpercellpg, data=PIC)
cor(PIC$PICpercellpg, PIC$SinkVel) #r=1

#PIC vs beta_DS, real data, saved SI w/ and w/o loess
ggplot(data=PIC, aes(x=PICpercellpg, y=log10(beta_DS), color=group2,  shape=Strain)) + geom_point(size=5)+theme_Publication2()+
  labs(y = expression(log10~beta~("encounters "~mL^-1~day^-1)), x = expression("PIC pg"~cell^-1))  +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) + geom_smooth (method="loess", aes(group=1), color="#525252") + 
  scale_color_manual(values=c("#e41a1c","#4a1486", "#377eb8")) + scale_shape_manual(values=c(1:11)) 

#Predicted data (10,000 data points), saved with lm and no lm
ggplot(data=PIC_newdata, aes(x=PICpercellpg, y=SinkVel, color=group2, shape=group2)) + geom_point(size=5)+theme_Publication2()+
  labs(y = expression("sinking velocity "~("m"~day^-1)), x = expression("PIC pg"~cell^-1)) +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) +
  geom_smooth (method="loess", aes(group=1), color="#525252") + 
  scale_color_manual(values=c("#e41a1c","#4a1486", "#377eb8"))

#Predicted data (10,000 data points), w/ and w/o loess
ggplot(data=PIC_newdata, aes(x=PICpercellpg, y=log10(beta_DS), color=group2,  shape=group2)) + geom_point(size=5)+theme_Publication2()+
  labs(y = expression(log10~beta~("encounters "~mL^-1~day^-1)), x = expression("PIC pg"~cell^-1))  +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) + #geom_smooth (method="loess", aes(group=1), color="#525252") + 
  scale_color_manual(values=c("#e41a1c","#4a1486", "#377eb8"))  

#encounters all betas
#viral encounters per day per cell
resize.win(6,5)
ggplot(data=all, aes(x=disrate,y = E_all_low , color=group, fill=group)) + 
  geom_smooth(size=1, position=position_jitter(w=0.02, h=0), aes(linetype="field", fill=group))+
  geom_smooth(data = all, aes(y= E_all_high, color=group,fill=group, linetype="lab")) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=2),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=7),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks()+
  theme_Publication2() +
  theme(legend.title = element_blank(), legend.key.width=unit(1,"cm"))+
  labs(y = expression("viral encounters " ~day^-1~cell^-1), x = expression("dissipation rate "~(m^2~s^-3))) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a")) + 
  scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a"))


ggplot(data=all, aes(x=log10(disrate),y = log10(E_all_low) , color=group, fill=group)) + 
  geom_smooth(size=1, position=position_jitter(w=0.02, h=0), aes(linetype="field", fill=group))+
  geom_smooth(data = all, aes(y= log10(E_all_high), color=group,fill=group, linetype="lab")) +
  theme_Publication2() +
  theme(legend.title = element_blank(), legend.key.width=unit(1,"cm"))+
  labs(y = expression("log10 viral encounters " ~day^-1~cell^-1), x = expression("log10 dissipation rate "~(m^2~s^-3))) +
  theme(legend.title = element_blank()) +
  scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) + 
  scale_fill_manual (values=c("#e41a1c", "#377eb8", "#4daf4a"))

#for all encounters
ggplot(data=all, aes(x=log10(disrate),y = log10(E_all_low_resvi) , color=group, fill=group)) + 
  geom_smooth(size=1, position=position_jitter(w=0.02, h=0), aes(linetype="field", fill=group))+
  geom_smooth(data = all, aes(y= log10(E_all_high_resvi), color=group,fill=group, linetype="lab")) +
  theme_Publication2() +
  theme(legend.title = element_blank(), legend.key.width=unit(1,"cm"))+
  labs(y = expression("log10 total encounters " ~day^-1), x = expression("log10 dissipation rate "~(m^2~s^-3))) +
  theme(legend.title = element_blank()) +
  scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) + 
  scale_fill_manual (values=c("#e41a1c", "#377eb8", "#4daf4a"))

#change color schemes for everything. follow scheme for naked, calc, lith

###days in the extracellular millieu
#2.knowles graph
calm <- all %>% filter (disrate %in% c ("1e-08"))
calm <- calm[rep(seq_len(nrow(calm)), 6), ]
calm$host <- rep_len(c (1 %o% 10^(seq(1, 6, 1))), length.out = 30)

stormy <- all %>% filter (disrate %in% c ("1e-05"))
stormy <- stormy[rep(seq_len(nrow(stormy)), 6), ]
stormy$host <- rep_len(c (1 %o% 10^(seq(1, 6, 1))), length.out = 30)

calmstormy <- rbind (calm, stormy)
calmstormy$E_all_host <- calmstormy$beta_all*(calmstormy$host*10)
calmstormy$days <- 1/(calmstormy$beta_all*calmstormy$host)

#ben knowles graph is hard to understand
ggplot(data=calmstormy, aes(x=log10(host),y = log10(days) , color=group, fill=group)) + geom_smooth() + facet_grid (~disrate) +  
  theme_Publication2() +
  theme(legend.title = element_blank(), legend.key.width=unit(1.5,"cm"))+
  labs(y = expression("log10 days in the extracellular milieu"), x = expression("log10 E. huxleyi"~mL^-1)) +
  theme(legend.title = element_blank()) +
  geom_hline(yintercept = log10(3), linetype="dashed") +
  scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) + 
  scale_fill_manual (values=c("#e41a1c", "#377eb8", "#4daf4a"))

#viruses adsorbed
probs <- expand.grid(list (group2 =c("Nc", "Cc-Mc", "Cc-Hc", "Li-Mc", "Li-Hc"), virus = c ("slow", 'fast'), condition = c("field", "lab")))

probs <- as.data.frame(list (group2 = as.factor(rep(c("Cc-Hc", "Cc-Mc", "Li-Hc", "Li-Mc", "Nc"), 4)), virus = rep(c("slow", "fast"), 2, each=5), condition=rep(c("field", "lab"), 1, each=10), hostnum = rep(c(10^3, 10^5), 1, each=10),  virnum = rep(c(10^4, 10^6), 1, each=10), propvir= rep(c(0.67, 0.33), 2, each=5), ads = rep(c(0.00482, 0.00482, 0.01670, 0.01670, 0.11400), 4), inf = rep(c(0.3, 0.3, NA, NA, 0.3, 0.06, 0.06, NA, NA, 0.06), 1)))

probs$bioinf <- probs$ads*probs$inf*probs$ads

calmstormy$group2 <- as.factor(calmstormy$group2)

#calmstormy_backup <- calmstormy
calmstormy <- calmstormy_backup

#join calmstormy and probs
calmstormy <- left_join(calmstormy, probs)

#calculate propEhV
calmstormy$propEhV <- calmstormy$virnum* calmstormy$propvir

#calculate encounters fast slow
calmstormy$encounters_propEhV <- calmstormy$beta_all*calmstormy$propEhV*calmstormy$hostnum #total enc
calmstormy$encounters_propEhV_pervir <- calmstormy$beta_all*calmstormy$propEhV #vir per day

#calculate total adsorption by virus props
calmstormy$adstot_prop <- calmstormy$encounters_propEhV*calmstormy$ads

#calculate total successful infections
calmstormy$sucinf_prop <- calmstormy$encounters_propEhV*calmstormy$bioinf

#plot encountered viruses
resize.win(5,7)
ggplot(calmstormy %>% drop_na(encounters_propEhV) , aes(x=as.factor(condition), y=log10(encounters_propEhV), color=as.factor(group), shape=as.factor(group))) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + labs (y = expression("log10 total encounters "~day^-1)) + theme_Publication2() + theme(legend.title = element_blank(), axis.title.x = element_blank()) + facet_grid(virus~disrate) + scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) +   geom_hline(yintercept = log10(1), linetype="dashed") 


#plot adsorbed viruses
ggplot(calmstormy, aes(x=as.factor(condition), y=log10(adstot_prop), color=as.factor(group), shape=as.factor(group))) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + labs (y = expression("log10 total adsorbed viruses "~day^-1)) + theme_Publication2() + theme(legend.title = element_blank(), axis.title.x = element_blank()) + facet_grid(virus~disrate) + scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) +   geom_hline(yintercept = log10(1), linetype="dashed")

#plot successful infection first
ggplot(calmstormy %>% drop_na(sucinf_prop) , aes(x=as.factor(condition), y=log10(sucinf_prop), color=as.factor(group), shape=as.factor(group))) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=2) + labs (y = expression("log10 total adsorbed viruses "~day^-1)) + theme_Publication2() + theme(legend.title = element_blank(), axis.title.x = element_blank()) + facet_grid(virus~disrate) +  scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) +   geom_hline(yintercept = log10(1), linetype="dashed")
