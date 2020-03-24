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
ggplot(data=PIC, aes(x=PICpercellpg, y=log10(beta_DS), color=group2,  shape=Strain)) + geom_point(size=7)+theme_Publication2()+
  labs(y = expression(log10~beta~("encounters "~mL^-1~day^-1)), x = expression("PIC pg"~cell^-1))  +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) + #geom_smooth (method="loess", aes(group=1), color="#525252") + 
  scale_color_manual(values=c("#e41a1c","#4a1486", "#377eb8")) + scale_shape_manual(values=c(1:11)) 

#Predicted data (10,000 data points), saved with lm and no lm
ggplot(data=PIC_newdata, aes(x=PICpercellpg, y=SinkVel, color=group2, shape=group2)) + geom_point(size=5)+theme_Publication()+
  labs(y = expression("sinking velocity "~("m"~day^-1)), x = expression("PIC pg"~cell^-1)) +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) +
  geom_smooth (method="loess", aes(group=1), color="#525252") + 
  scale_color_manual(values=c("#e41a1c","#4a1486", "#377eb8"))

#Predicted data (10,000 data points), w/ and w/o loess
ggplot(data=PIC_newdata, aes(x=PICpercellpg, y=log10(beta_DS), color=group2,  shape=group2)) + geom_point(size=5)+theme_Publication()+
  labs(y = expression(log10~beta~("encounters "~mL^-1~day^-1)), x = expression("PIC pg"~cell^-1))  +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) + geom_smooth (method="loess", aes(group=1), color="#525252") + 
  scale_color_manual(values=c("#e41a1c","#4a1486", "#377eb8"))  

#turbulence
resize.win(6,6)
ggplot(data = turb, aes(x = log10(disrate), y = log10(beta_turb), color=group, shape=group)) + geom_point(size =5) +
  theme_Publication() +
  labs(y = expression(log10~beta~("encounters "~mL^-1~day^-1)), 
       x = expression("dissipation rate "~(m^2~s^-3))) +
  theme(legend.title = element_blank()) +
  scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a"))

#encounters all betas
#viral encounters per day per cell
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
resize.win (7,6)
ggplot(data=all, aes(x=log10(disrate),y = log10(E_all_low_resvi) , color=group, fill=group)) + 
  geom_smooth(size=1, position=position_jitter(w=0.02, h=0), aes(linetype="open ocean", fill=group))+
  geom_smooth(data = all, aes(y= log10(E_all_high_resvi), color=group,fill=group, linetype="coastal")) +
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
calm$host <-  rep(c (1 %o% 10^(seq(1, 6, 1))), 1, each=4)
calm$watcon <- "calm"

stormy <- all %>% filter (disrate %in% c ("0.001"))
stormy <- stormy[rep(seq_len(nrow(stormy)), 6), ]
stormy$host <- rep(c (1 %o% 10^(seq(1, 6, 1))), 1, each=4)
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
probs <- as.data.frame(list (group = as.factor (rep(c("Cc", "Cc", "Li", "Nc"), 4)), group2 = as.factor(rep(c("Cc-Oc", "Cc-Mc", "Li", "Nc"), 4)), virus = rep(c("high", "low"), 1, each=8), condition=rep(c("open ocean", "coastal"), 2, each=4), hostnum = rep(c(10^3, 10^3, 10^4, 10^3, 10^5, 10^5, 10^6, 10^5), 1, each=1),  virnum = rep(c(10^4, 10^6), 2, each=4), prophost = rep(c(0.9, 0.9, 1, 0.1), 4), propvir= rep(c(0.33, 0.67), 1, each=8), ads = rep(c(0.0130, 0.0130, 0.0226, 0.0304), 4), inf = rep(c(0.3, 0.3, NA, 0.3, 0.3, 0.3, NA, 0.3, 0.06, 0.06, NA, 0.06, 0.06, 0.06, NA, 0.06))))

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
calmstormy$encounters_propEhV <- calmstormy$beta_all*calmstormy$propEhV*calmstormy$prophost #total enc

#calculate total adsorption by virus props
calmstormy$adstot_prop <- calmstormy$encounters_propEhV*calmstormy$ads

#calculate total successful infections
calmstormy$sucinf_prop <- calmstormy$encounters_propEhV*calmstormy$ads*calmstormy$inf

calmstormy$group <- reorder.factor (calmstormy$group, new.order = c("Nc", "Cc", "Li"))

#plot encountered viruses
resize.win(6,6)
ggplot(calmstormy %>% drop_na(encounters_propEhV) , aes(x=as.factor(condition), y=log10(encounters_propEhV), color=as.factor(group), shape=as.factor(group))) +geom_boxplot() + geom_point (position = position_jitterdodge(), size=1) + labs (y = expression("log10 total encounters "~day^-1)) + theme_Publication2() + theme(legend.title = element_blank(), axis.title.x = element_blank()) + facet_grid(disrate~virus) + scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) +   geom_hline(yintercept = log10(1), linetype="dashed") 

melted_calmstormy <- reshape2::melt(calmstormy %>% select ("group", "group2", "disrate", "watcon", "virus", "condition", "hostnum", "encounters_propEhV", "adstot_prop", "sucinf_prop"), id.vars=c("group", "group2", "disrate", "virus", "condition", "watcon"))

melted_calmstormy$variable <- factor (melted_calmstormy$variable,levels= c("hostnum", "encounters_propEhV", "adstot_prop", "sucinf_prop"), labels = c("entity abundance\n mL-1", "total encounters\n day-1 mL-1", "total entities with\n adsorbed EhVs day-1 mL-1", "total lytic\n infections day-1 mL-1"))

melted_calmstormy$condition <- reorder.factor (melted_calmstormy$condition, new.order = c("open ocean", "coastal"))

resize.win (10.5,7)#change virus to low and high
ggplot(melted_calmstormy %>% filter (virus=="low"), aes(x=watcon, y=log10(value), color=group, shape=group))  + geom_boxplot()+ geom_point (position=position_jitterdodge(), size=2.5) + facet_grid(condition~variable) + theme_Publication() + theme (axis.title.x = element_blank(), legend.title = element_blank()) + labs (y="log10") + geom_hline(yintercept = log10(1), linetype="dashed") + scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) 

#combined high and low
high <- calmstormy %>% filter (virus=="high")
low <- calmstormy %>% filter (virus=="low")
calmstormy_comb <- calmstormy %>% filter (virus=="high") %>% select (c(group2, group, watcon, condition, prophost))
calmstormy_comb$enccomb <- high$encounters_propEhV + low$encounters_propEhV
calmstormy_comb$adscomb <- high$adstot_prop + low$adstot_prop
calmstormy_comb$infcomb <- high$sucinf_prop + low$sucinf_prop

##percentage of parameters
calmstormy_comb$perencounters <- (calmstormy_comb$enccomb/calmstormy_comb$prophost)
calmstormy_comb$peradsorbed <- (calmstormy_comb$adscomb/calmstormy_comb$prophost)
calmstormy_comb$perinf <- (calmstormy_comb$infcomb/calmstormy_comb$prophost)
calmstormy_comb <- calmstormy_comb %>% mutate(perencounters= if_else(perencounters > 1, 1, perencounters)) %>% mutate(peradsorbed= if_else(perencounters > 1, 1, peradsorbed)) %>% mutate(perinf= if_else(perinf > 1, 1, perinf)) 

calmstormy_comb$perencounters_noenc <- 1- calmstormy_comb$perencounters
calmstormy_comb$perads_noads <- 1- calmstormy_comb$peradsorbed
calmstormy_comb$perinf_noinf <- 1- calmstormy_comb$perinf

#drop Cc-Mc
calmstormy_comb.drop <- calmstormy_comb %>% filter(!(group2 %in% c("Cc-Mc")))

melt_enc <- reshape2::melt(calmstormy_comb.drop %>% select ("group", "group2", "watcon","condition", "perencounters", "perencounters_noenc"), id.vars=c("group", "group2", "watcon", "condition"))

melt_ads <- reshape2::melt(calmstormy_comb.drop %>% select ("group", "group2", "watcon","condition", "peradsorbed", "perads_noads" ), id.vars=c("group", "group2", "watcon", "condition"))

melt_inf <- reshape2::melt(calmstormy_comb.drop %>% select ("group", "group2", "watcon","condition", "perinf", "perinf_noinf" ), id.vars=c("group", "group2", "watcon", "condition"))

#encounters
resize.win (6,4)

ggplot(melt_enc %>% filter (watcon=="calm"), aes(x = "", y = value, fill=variable)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values=c("#808000", "#FFFFFF")) +
  theme_void() + theme (legend.position = "bottom") + facet_grid(condition~group) + geom_text(aes(y = value, label=sprintf("%0.4f", round(value, digits = 4)), size=12), position = position_stack(vjust = 0.5))

ggplot(melt_enc %>% filter (watcon=="stormy"), aes(x = "", y = value, fill=variable)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values=c("#808000", "#FFFFFF")) +
  theme_void() + theme (legend.position = "bottom") + facet_grid(condition~group) + geom_text(aes(y = value, label=sprintf("%0.4f", round(value, digits = 4)), size=12), position = position_stack(vjust = 0.5))

#adsorbed
ggplot(melt_ads %>% filter (watcon=="calm"), aes(x = "", y = value, fill=variable)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values=c("#008080", "#FFFFFF")) +
  theme_void() + theme (legend.position = "bottom") + facet_grid(condition~group) + geom_text(aes(y = value, label=sprintf("%0.6f", round(value, digits = 6)), size=12), position = position_stack(vjust = 0.5))

ggplot(melt_ads %>% filter (watcon=="stormy"), aes(x = "", y = value, fill=variable)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values=c("#008080", "#FFFFFF")) +
  theme_void() + theme (legend.position = "bottom") + facet_grid(condition~group) + geom_text(aes(y = value, label=sprintf("%0.6f", round(value, digits = 6)), size=12), position = position_stack(vjust = 0.5))

#infected
ggplot(melt_inf %>% filter (watcon=="calm") %>% filter(!(group %in% c("Li"))), aes(x = "", y = value, fill=variable)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values=c("#800080", "#FFFFFF")) +
  theme_void() + theme (legend.position = "bottom") + facet_grid(condition~group) + geom_text(aes(y = value, label=sprintf("%0.6f", round(value, digits = 6)), size=12), position = position_stack(vjust = 0.5))

ggplot(melt_inf %>% filter (watcon=="stormy") %>% filter(!(group %in% c("Li"))), aes(x = "", y = value, fill=variable)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values=c("#800080", "#FFFFFF")) +
  theme_void() + theme (legend.position = "bottom") + facet_grid(condition~group) + geom_text(aes(y = value, label=sprintf("%0.6f", round(value, digits = 6)), size=12), position = position_stack(vjust = 0.5))


##summaries of all host entities

allhost <- calmstormy %>% filter (condition=="coastal") %>% select(c(group, group2, beta_all, watcon, virus, propvir, prophost, ads, inf))

allhost <- allhost[rep(seq_len(nrow(allhost)), 6), ]
allhost$host <- rep(c (1 %o% 10^(seq(1, 6, 1))), 1, each=16)
allhost$virnum <- (allhost$host) * 10 *(allhost$propvir)

allhost$enc <- allhost$beta_all*allhost$host*allhost$virnum
allhost$adsprob <- allhost$enc*allhost$ads
allhost$sucinf <- allhost$adsprob*allhost$inf

ggplot(allhost, aes(x=as.factor(host), y=log10(enc), color=group, shape=group))  + geom_boxplot()+ geom_point (position=position_jitterdodge(), size=2.5) + facet_grid(watcon~virus) + theme_Publication() + theme (axis.title.x = element_blank(), legend.title = element_blank()) + labs (y="log10 value") + geom_hline(yintercept = log10(1), linetype="dashed") + scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) 

#combined high and low
high_allhost <- allhost %>% filter (virus=="high")
low_allhost <- allhost %>% filter (virus=="low")
allhost_comb <- allhost %>% filter (virus=="high") %>% select (c(group2, group, watcon, host, virnum, host))
allhost_comb$enccomb <- high_allhost$enc + low_allhost$enc
allhost_comb$adscomb <- high_allhost$adsprob + low_allhost$adsprob
allhost_comb$infcomb <- high_allhost$sucinf + low_allhost$sucinf

##percentage of parameters
allhost_comb$perencounters <- (allhost_comb$enccomb/allhost_comb$host)
allhost_comb$peradsorbed <- (allhost_comb$adscomb/allhost_comb$host)
allhost_comb$perinf <- (allhost_comb$infcomb/allhost_comb$host)
allhost_comb <- allhost_comb %>% mutate(perencounters= if_else(perencounters > 1, 1, perencounters)) %>% mutate(peradsorbed= if_else(perencounters > 1, 1, peradsorbed)) %>% mutate(perinf= if_else(perinf > 1, 1, perinf)) 

##allhost_comb is the summary


