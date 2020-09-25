#summaries
BM #BM summary
DS_pred #predicted data


#for paper
resize.win(6,6)

#PIC vs sinkvel cell, real data, saved SI with lm and no lm
ggplot(data=PIC, aes(x=PICpercellpg, y=SinkVel, color=group2, shape=Strain)) + geom_point(size=7)+theme_Publication()+
  labs(y = expression("sinking velocity "~("m"~day^-1)), x = expression("PIC pg"~cell^-1)) + 
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) + #geom_smooth (method="lm", aes(group=1), color="#525252") + 
  scale_color_manual(values=c("#e41a1c","#4a1486", "#377eb8")) + scale_shape_manual(values=c(1:11)) #get regression

ggplot(data=PIC, aes(x=PICpercellpg, y=SinkVel, color=group2)) + geom_point(aes(size=Den_celltotal))+
  scale_size(range = c(3,8)) + theme_Publication()+
  labs(y = expression("sinking velocity "~("m"~day^-1)), x = expression("PIC pg"~cell^-1)) + 
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) + #geom_smooth (method="lm", aes(group=1), color="#525252") + 
  scale_color_manual(values=c("#e41a1c","#4a1486", "#377eb8")) + scale_shape_manual(values=c(1:11)) +
  guides(colour = guide_legend(override.aes = list(size=5)))

#regression, 
PIC_SinkVel_reg <- lm (SinkVel ~ PICpercellpg, data=PIC)
cor(PIC$PICpercellpg, PIC$SinkVel) #r=1

#PIC vs beta_DS, real data, saved SI w/ and w/o loess
ggplot(data=PIC, aes(x=PICpercellpg, y=log10(beta_DS), color=group2,  shape=Strain)) + geom_point(size=7)+theme_Publication()+
  labs(y = expression(log10~beta~("encounters "~mL^-1~day^-1)), x = expression("PIC pg"~cell^-1))  +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) + #geom_smooth (method="loess", aes(group=1), color="#525252") + 
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
  theme_Publication() +
  theme(legend.title = element_blank(), legend.key.width=unit(1,"cm"))+
  labs(y = expression("log10 viral encounters " ~day^-1~cell^-1), x = expression("log10 dissipation rate "~(m^2~s^-3))) +
  theme(legend.title = element_blank()) +
  scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) + 
  scale_fill_manual (values=c("#e41a1c", "#377eb8", "#4daf4a"))

#for all encounters
ggplot(data=all, aes(x=log10(disrate),y = log10(E_all_low_resvi) , color=group, fill=group)) + 
  geom_smooth(size=1, position=position_jitter(w=0.02, h=0), aes(linetype="field", fill=group))+
  geom_smooth(data = all, aes(y= log10(E_all_high_resvi), color=group,fill=group, linetype="lab")) +
  theme_Publication() +
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
calm$watcon <- "calm"

stormy <- all %>% filter (disrate %in% c ("0.001"))
stormy <- stormy[rep(seq_len(nrow(stormy)), 6), ]
stormy$host <- rep_len(c (1 %o% 10^(seq(1, 6, 1))), length.out = 30)
stormy$watcon <- "stormy"

calmstormy <- rbind (calm, stormy)
knowlesgraph <- calmstormy
knowlesgraph$E_all_host <- knowlesgraph$beta_all*(knowlesgraph$host*10)
knowlesgraph$days <- 1/(knowlesgraph$beta_all*knowlesgraph$host)

#ben knowles graph is hard to understand
ggplot(data=knowlesgraph, aes(x=log10(host),y = log10(days) , color=group, fill=group)) + geom_smooth() + facet_grid (~watcon) +  
  theme_Publication() +
  theme(legend.title = element_blank(), legend.key.width=unit(1.5,"cm"))+   labs(y = expression("log10 days in the \nextracellular milieu"), x = expression("log10 E. huxleyi"~mL^-1)) +
  theme(legend.title = element_blank(), axis.title.y=element_text(vjust=-2, margin = unit(c(0, 5, 0, 0), "mm"))) +
  geom_hline(yintercept = log10(3), linetype="dashed") +
  scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) + 
  scale_fill_manual (values=c("#e41a1c", "#377eb8", "#4daf4a"))

#make another calmstormy df
calm <- all %>% filter (disrate %in% c ("1e-08"))
calm$watcon <- "calm"

stormy <- all %>% filter (disrate %in% c ("0.001"))
stormy$watcon <- "stormy"

calmstormy <- rbind (calm, stormy)

#viruses probs
probs <- as.data.frame(list (group = as.factor (rep(c("Cc", "Cc", "Li",  "Li", "Nc"), 4)), group2 = as.factor(rep(c("Cc-Hc", "Cc-Mc", "Li-Hc", "Li-Mc", "Nc"), 4)), virus = rep(c("high", "low"), 1, each=10), condition=rep(c("field", "lab"), 2, each=5), hostnum = rep(c(10^3, 10^3, 10^4, 10^4, 10^3, 10^5, 10^5, 10^6, 10^6, 10^5), 1, each=1),  virnum = rep(c(10^4, 10^6), 2, each=5), prophost = rep(c(0.9, 0.9, 1, 1, 0.1), 4), propvir= rep(c(0.33, 0.67), 1, each=10), ads = rep(c(0.074, 0.074, 0.154, 0.154, 0.230), 4), inf = rep(c(0.3, 0.3, NA, NA, 0.3, 0.3, 0.3, NA, NA, 0.3, 0.06, 0.06, NA, NA, 0.06, 0.06, 0.06, NA, NA, 0.06))))

calmstormy$group2 <- as.factor(calmstormy$group2)

#calmstormy_backup <- calmstormy
calmstormy <- calmstormy_backup

#join calmstormy and probs
calmstormy <- left_join(calmstormy, probs)

#calculate propEhV
calmstormy$propEhV <- calmstormy$virnum* calmstormy$propvir

#calculate prophost
calmstormy$prophost <- calmstormy$hostnum* calmstormy$prophost

#calculate encounters fast slow
calmstormy$encounters_propEhV <- calmstormy$beta_all*calmstormy$propEhV #total enc

#calculate total adsorption by virus props
calmstormy$adstot_prop <- calmstormy$encounters_propEhV*calmstormy$ads

#calculate total successful infections
calmstormy$sucinf_prop <- calmstormy$encounters_propEhV*calmstormy$ads*calmstormy$inf

calmstormy$group <- reorder.factor (calmstormy$group, new.order = c("Nc", "Cc", "Li"))

#plot encountered viruses
resize.win(6,6)
ggplot(calmstormy %>% drop_na(encounters_propEhV) , aes(x=as.factor(condition), y=log10(encounters_propEhV), color=as.factor(group), shape=as.factor(group))) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=1) + labs (y = expression("log10 total encounters "~day^-1)) + theme_Publication2() + theme(legend.title = element_blank(), axis.title.x = element_blank()) + facet_grid(disrate~virus) + scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) +   geom_hline(yintercept = log10(1), linetype="dashed") 

melted_calmstormy <- reshape2::melt(calmstormy %>% select ("group", "group2", "disrate", "watcon", "virus", "condition", "hostnum", "encounters_propEhV", "adstot_prop", "sucinf_prop"), id.vars=c("group", "group2", "disrate", "virus", "condition", "watcon"))

melted_calmstormy$variable <- factor (melted_calmstormy$variable,levels= c("hostnum", "encounters_propEhV", "adstot_prop", "sucinf_prop"), labels = c("entity abundance\n mL-1", "encounters\n entity-1 day-1 mL-1", "adsorbed EhVs\n entity-1 day-1 mL-1", "successful infection\n entity-1 day-1 mL-1"))


resize.win (9.5,7)#change field to lab and vice-versa
ggplot(melted_calmstormy %>% filter (condition=="field"), aes(x=watcon, y=log10(value), color=group, shape=group))  + geom_boxplot()+ geom_point (position=position_jitterdodge(), size=2.5) + facet_grid(virus~variable) + theme_Publication() + theme (axis.title.x = element_blank(), legend.title = element_blank()) + labs (y="log10 value") + geom_hline(yintercept = log10(1), linetype="dashed") + scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) 
