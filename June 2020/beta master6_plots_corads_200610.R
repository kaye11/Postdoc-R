##Workspace: beta_master5

#summaries
BM #BM summary
DS_pred #predicted data


#for paper
resize.win(6,6)

#PIC vs sinkvel cell, real data, saved SI with lm and no lm
PIC$group <- reorder.factor (PIC$group, new.order = c("Nc", "Cc"))
PIC_newdata$group <- reorder.factor (PIC_newdata$group, new.order = c("Nc", "Cc"))

ggplot(data=PIC, aes(x=PICpercellpg, y=SinkVel, color=group, shape=Strain)) + geom_point(size=7)+theme_Publication2()+
  labs(y = expression("sinking velocity "~("m"~day^-1)), x = expression("PIC pg"~cell^-1)) + 
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) + #geom_smooth (method="lm", aes(group=1), color="#525252") + 
  scale_color_manual(values=c("#e41a1c", "#377eb8")) + scale_shape_manual(values=c(1:12)) #get regression

ggplot(data=PIC, aes(x=PICpercellpg, y=SinkVel, color=group)) + geom_point(aes(size=Den_celltotal))+
  scale_size(range = c(3,8)) + theme_Publication()+
  labs(y = expression("sinking velocity "~("m"~day^-1)), x = expression("PIC pg"~cell^-1)) + 
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) + #geom_smooth (method="lm", aes(group=1), color="#525252") + 
  scale_color_manual(values=c("#e41a1c","#377eb8")) + scale_shape_manual(values=c(1:11)) +
  guides(colour = guide_legend(override.aes = list(size=5)))

ggplot(data=PIC, aes(x=PICpercellpg, y=log10(beta_DS), color=group)) + geom_point(size=5)+theme_Publication()+
  labs(y = expression(log[10]~beta[S]~("encounters "~mL~day^-1)), x = expression("PIC pg"~cell^-1))  +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) +  
  scale_color_manual(values=c("#e41a1c", "#377eb8"))  

#Predicted data (equally spaced data) sinkvel
ggplot(data=PIC_newdata, aes(x=PICpercellpg, y=SinkVel, color=group)) + geom_point(size=5)+theme_Publication()+
  labs(y = expression("sinking velocity "~("m"~day^-1)), x = expression("PIC pg"~cell^-1)) +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) +
  scale_color_manual(values=c("#e41a1c","#377eb8"))

#Predicted data (equally spaced data) betas
ggplot(data=PIC_newdata, aes(x=PICpercellpg, y=log10(beta_DS), color=group)) + geom_point(size=5)+theme_Publication()+
  labs(y = expression(log[10]~beta[S]~("encounters "~mL~day^-1)), x = expression("PIC pg"~cell^-1))  +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) +  
  scale_color_manual(values=c("#e41a1c", "#377eb8"))  

#turbulence
resize.win(6,6)
ggplot(data = turb, aes(x = log10(disrate), y = log10(beta_turb), color=group)) + geom_line(size =2) +
  theme_Publication() +
  labs(y = expression(log[10]~beta[T]~("encounters "~mL~day^-1)), 
       x = expression(log[10]~epsilon~(m^2~s^-3))) +
  theme(legend.title = element_blank()) +
  scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) + guides(colour = guide_legend(override.aes = list(size=5)))

#encounters all betas
#viral encounters per day per cell
resize.win(7,6)
all$group <- reorder.factor (all$group, new.order = c("Nc", "Cc", "Li"))

ggplot(data=all, aes(x=log10(disrate),y = log10(E_all_low) , color=group)) + 
  geom_line(size=2, position=position_jitter(w=0.02, h=0), aes(linetype="shelf slope"))+
  geom_line(size=2, data = all, aes(y= log10(E_all_high), color=group,linetype="open ocean")) +
  theme_Publication() +
  theme(legend.title = element_blank(), legend.key.width=unit(1,"cm"))+
  labs(y = expression(log[10]~"viral encounters " ~day^-1~cell^-1), x = expression(log[10]~epsilon~(m^2~s^-3))) +
  theme(legend.title = element_blank()) +
  scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) + 
  scale_fill_manual (values=c("#e41a1c", "#377eb8", "#4daf4a"))

#for all encounters
resize.win (7,6)
ggplot(data=all, aes(x=log10(disrate),y = log10(E_all_low_resvi) , color=group, fill=group)) + 
  geom_line(size=2, position=position_jitter(w=0.02, h=0), aes(linetype="open ocean"))+
  geom_line(size=2, data = all, aes(y= log10(E_all_high_resvi), color=group, linetype="shelf slope")) +
  theme_Publication() +
  theme(legend.title = element_blank(), legend.key.width=unit(1,"cm"))+
  labs(y = expression(log[10]~"total encounters " ~day^-1~cell^-1), x = expression(log[10]~epsilon~(m^2~s^-3))) +
  theme(legend.title = element_blank()) +
  scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) + 
  scale_fill_manual (values=c("#e41a1c", "#377eb8", "#4daf4a"))

#change color schemes for everything. follow scheme for naked, calc, lith

#merge 3 disrates (10e-3, 10e-5, 10e-8)
calm <- all %>% filter (disrate %in% c ("1e-08")) %>% select (group, beta_BM, beta_DS, disrate, beta_turb)
calm$watcon <- "calm"

stormy <- all %>% filter (disrate %in% c ("0.001")) %>% select (group, beta_BM, beta_DS, disrate, beta_turb)
stormy$watcon <- "stormy"

mid <- all %>% filter (disrate %in% c ("1e-05")) %>% select (group, beta_BM, beta_DS, disrate, beta_turb)
mid$watcon <- "mid"

calmstormy <- rbind (calm, stormy, mid)
calmstormy$beta_all <- calmstormy$beta_BM + calmstormy$beta_DS + calmstormy$beta_turb

##probabilities check alpha v4
probs <- as.data.frame(list (group = as.factor (rep(c("Cc","Li", "Nc"), 4)), virus = rep(c("high", "low"), 1, each=6), condition=rep(c("open ocean", "shelf slope"), 2, each=3), hostnum = rep(c(10^3, 10^4, 10^2, 10^5, 10^6, 10^4), 1),  virnum = rep(c(10^4, 10^6), 2, each=3), prophost = rep(c(1, 1, 0.1), 4), propvir= rep(c(0.33, 0.67), 1, each=6), ads = rep(c(0.0243, 0.0302, 0.2994), 4), inf = rep(c(0.3, NA, 0.3, 0.3, NA, 0.3, 0.06, NA, 0.06, 0.06, NA, 0.06))))

#calmstormy_backup <- calmstormy
calmstormy <- calmstormy_backup

#join calmstormy and probs
calmstormy <- left_join(calmstormy, probs)

#calculate propEhV
calmstormy$propEhV <- calmstormy$virnum* calmstormy$propvir

#calculate prophost
#calmstormy$prophost <- calmstormy$hostnum* calmstormy$prophost

#calculate encounters fast slow
calmstormy$encounters_propEhV <- calmstormy$beta_all*calmstormy$propEhV*calmstormy$hostnum #total enc

#calculate total adsorption by virus props
calmstormy$adstot_prop <- calmstormy$encounters_propEhV*calmstormy$ads

#calculate total successful infections
calmstormy$sucinf_prop <- calmstormy$encounters_propEhV*calmstormy$ads*calmstormy$inf

calmstormy$group <- reorder.factor (calmstormy$group, new.order = c("Nc", "Cc", "Li"))

#edit in excel to make life easier
write.table(calmstormy, "Postdoc-R/Exported Tables/calmstormy.csv", sep=";", col.names=T, row.names=F)

#data <- calmstormy %>% select(group, watcon, virus, condition, encounters_propEhV, adstot_prop, sucinf_prop)

#make individual data table for getting the distilled plot
conc <- calmstormy[1:12, ] %>% select (group, virus, condition, hostnum)
conc$parameter="concentration"
conc$calm <- 0
conc$stormy <- 0
conc$mid <-conc$hostnum

enc <- reshape(calmstormy %>% select(group, watcon, virus, condition, hostnum, encounters_propEhV), idvar=c("group", "virus", "condition", "hostnum"), timevar="watcon", direction="wide")
enc$parameter= "encounters"
ads <- reshape(calmstormy %>% select(group, watcon, virus, condition, hostnum, adstot_prop), idvar=c("group", "virus", "condition", "hostnum"), timevar="watcon", direction="wide")
ads$parameter= "adsorption"
inf <- reshape(calmstormy %>% select(group, watcon, virus, condition, hostnum, sucinf_prop), idvar=c("group", "virus", "condition", "hostnum"), timevar="watcon", direction="wide")
inf$parameter= "infections"

#renaming
enc <- setnames (enc, c("encounters_propEhV.calm", "encounters_propEhV.stormy", "encounters_propEhV.mid"), c("calm", "stormy", "mid"))
ads <- setnames (ads, c("adstot_prop.calm", "adstot_prop.stormy", "adstot_prop.mid"), c("calm", "stormy", "mid"))
inf <- setnames (inf, c("sucinf_prop.calm", "sucinf_prop.stormy", "sucinf_prop.mid"), c("calm", "stormy", "mid"))

#combined
allsteps <- rbind (conc, enc, ads, inf)

resize.win(6,6)

#correct
ggplot(enc, aes(x=as.factor(group), y=log10(mid), color=as.factor(group), shape=as.factor(group))) +
  geom_point (size=5) + geom_errorbar(aes (ymax=log10(mid + stormy), ymin= log10(mid-calm))) + facet_grid(condition~virus)

##make the plot

variable_labs <- c(
  `concentration` = 'concentration~(mL^{-1})',
  `encounters` = 'encounter~rate~(mL^{-1}~d^{-1})',
  `adsorption` = 'adsorption~rate~(mL^{-1}~d^{-1})',
  `infections` = 'infection~rate~(mL^{-1}~d^{-1})'
)

resize.win (11,7)#change virus to low and high
allsteps$parameter <- reorder.factor (allsteps$parameter, new.order = c("concentration", "encounters", "adsorption", "infections"))

ggplot(allsteps %>% filter (virus=="high"), aes(x=group, y=log10(mid), color=group, shape=group))  + 
  geom_point (size=6) + 
  geom_errorbar(aes (ymax=log10(mid + stormy), ymin= log10(mid-calm)), width=0.5, size=1) +
  facet_grid(condition~parameter,labeller = labeller(parameter = as_labeller(variable_labs, label_parsed))) +
  theme_Publication2() + theme (axis.title.x = element_blank(), legend.position = "none") + labs (y = expression(log[10])) + geom_hline(yintercept = log10(1), linetype="dashed") + scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) 

#allsteps split
#combined high and low
high <- reshape(allsteps %>% filter (virus=="low") %>% select (c (group, condition, hostnum, parameter, mid)), idvar=c("group", "condition", "hostnum"), timevar="parameter", direction="wide")
low <- reshape(allsteps %>% filter (virus=="low") %>% select (c (group, condition, hostnum, parameter, mid)), idvar=c("group", "condition", "hostnum"), timevar="parameter", direction="wide")

allsteps_comb <- high %>% select (c(group, condition, hostnum))

#combine high and low
allsteps_comb$enccomb <- high$mid.encounters + low$mid.encounters
allsteps_comb$adscomb <- high$mid.adsorption + low$mid.adsorption
allsteps_comb$infcomb <- high$mid.infections + low$mid.infections

##percentage of parameters
allsteps_comb$perencounters <- (allsteps_comb$enccomb/allsteps_comb$hostnum)
allsteps_comb$peradsorbed <- (allsteps_comb$adscomb/allsteps_comb$hostnum)
allsteps_comb$perinf <- (allsteps_comb$infcomb/allsteps_comb$hostnum)
allsteps_comb <- allsteps_comb %>% mutate(perencounters= if_else(perencounters > 1, 1, perencounters)) %>% mutate(peradsorbed= if_else(perencounters > 1, 1, peradsorbed)) %>% mutate(perinf= if_else(perinf > 1, 1, perinf)) 

allsteps_comb$perencounters_noenc <- 1- allsteps_comb$perencounters
allsteps_comb$perads_noads <- 1- allsteps_comb$peradsorbed
allsteps_comb$perinf_noinf <- 1- allsteps_comb$perinf

#melt data
melt_enc <- reshape2::melt(allsteps_comb %>% select ("group", "condition", "perencounters", "perencounters_noenc"), id.vars=c("group", "condition"))

melt_ads <- reshape2::melt(allsteps_comb %>% select ("group", "condition", "peradsorbed", "perads_noads" ), id.vars=c("group","condition"))

melt_inf <- reshape2::melt(allsteps_comb %>% select ("group", "condition", "perinf", "perinf_noinf" ), id.vars=c("group", "condition"))

#encounters
resize.win (6,4)

#no watcon
ggplot(melt_enc, aes(x = "", y = value, fill=variable)) +
  geom_bar(width = 1, stat = "identity", color = "black") +   coord_polar("y", start = 0)+
  scale_fill_manual(values=c("#808000", "#FFFFFF")) + theme_void() +  
  theme (legend.position = "bottom", legend.text = element_text(size = 15), 
         strip.text = element_text(size=15), legend.title=element_blank()) + facet_grid(condition~group) + 
  geom_text(aes(y = value, label=sprintf("%0.4f", round(value, digits = 4))), size=5, position = position_stack(vjust = 0.5))

#adsorbed
ggplot(melt_ads, aes(x = "", y = value, fill=variable)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values=c("#008080", "#FFFFFF")) +
  theme_void() + theme (legend.position = "bottom", legend.text = element_text(size = 15), 
                        strip.text = element_text(size=15), legend.title=element_blank()) + facet_grid(condition~group) + 
  geom_text(aes(y = value, label=sprintf("%0.6f", round(value, digits = 4))), size=5, position = position_stack(vjust = 0.5))

#infected
ggplot(melt_inf %>% filter(!(group %in% c("Li"))), aes(x = "", y = value, fill=variable)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values=c("#800080", "#FFFFFF")) +
  theme_void() +  theme (legend.position = "bottom", legend.text = element_text(size = 15), 
                         strip.text = element_text(size=15), legend.title=element_blank()) + facet_grid(condition~group) + 
  geom_text(aes(y = value, label=sprintf("%0.6f", round(value, digits = 4))), size=5, position = position_stack(vjust = 0.5))

####all host

##summaries of all host entities

allhost <- calmstormy %>% filter (condition=="shelf slope") %>% select(c(group, beta_all, watcon, virus, propvir, ads, inf))

allhost <- allhost[rep(seq_len(nrow(allhost)), 8), ]

allhost$hostnum <- rep(c (1 %o% 10^(seq(0, 7, 1))), 1, each=18)
allhost$virnum <- rep(c (1 %o% 10^(seq(1, 8, 1))), 1, each=18) #*(allhost$propvir)

allhost <- allhost %>% mutate(virnum= if_else(group == "Li", hostnum, virnum))

#calculate encounters fast slow
allhost$encounters_propEhV <- allhost$beta_all*allhost$propvir*allhost$virnum*allhost$hostnum #total enc

#calculate total adsorption by virus props
allhost$adstot_prop <- allhost$encounters_propEhV*allhost$ads

#calculate total successful infections
allhost$sucinf_prop <- allhost$encounters_propEhV*allhost$ads*allhost$inf

allhost$group <- reorder.factor (allhost$group, new.order = c("Nc", "Cc", "Li"))

#allhost_del <- allhost [-c(127, 128, 133, 134, 139, 140, 131, 132, 137, 138, 143, 144, 3, 4, 9, 10, 15, 16), ]


allhost_conc <- allhost[c(1:6, 19:24, 37:42, 55:60, 73:78, 91:96, 109:114, 127:132), ] %>% select (group, virus, hostnum)
allhost_conc$parameter="concentration"
allhost_conc$calm <- 0
allhost_conc$stormy <- 0
allhost_conc$mid <-allhost_conc$hostnum

allhost_enc <- reshape(allhost %>% select (group, virus, hostnum, watcon, encounters_propEhV), idvar=c("group", "virus", "hostnum"), timevar="watcon", direction="wide")
allhost_enc$parameter= "encounters"
allhost_ads <- reshape(allhost %>% select (group, virus, hostnum, watcon, adstot_prop), idvar=c("group", "virus", "hostnum"), timevar="watcon", direction="wide")
allhost_ads$parameter= "adsorption"
allhost_inf <- reshape(allhost %>% select (group, virus, hostnum, watcon, sucinf_prop), idvar=c("group", "virus", "hostnum"), timevar="watcon", direction="wide")
allhost_inf$parameter= "infections"


#renaming
allhost_enc <- setnames (allhost_enc, c("encounters_propEhV.calm", "encounters_propEhV.stormy", "encounters_propEhV.mid"), c("calm", "stormy", "mid"))
allhost_ads <- setnames (allhost_ads, c("adstot_prop.calm", "adstot_prop.stormy", "adstot_prop.mid"), c("calm", "stormy", "mid"))
allhost_inf <- setnames (allhost_inf, c("sucinf_prop.calm", "sucinf_prop.stormy", "sucinf_prop.mid"), c("calm", "stormy", "mid"))

#combined
allhost_allsteps <- rbind (allhost_conc, allhost_enc, allhost_ads, allhost_inf)

#correct
ggplot(allhost_enc %>% filter (virus=="high"), aes(x=as.factor(group), y=log10(mid), color=as.factor(group), shape=as.factor(group))) +
  geom_point (size=5) + geom_errorbar(aes (ymax=log10(mid + stormy), ymin= log10(mid-calm))) + facet_grid(~hostnum)

##plot
resize.win (19,16)
allhost_allsteps$parameter <- reorder.factor (allhost_allsteps$parameter, new.order = c("concentration", "encounters", "adsorption", "infections"))

host_labs <- c(
  `1` = '10^{0}',
  `10` = '10^{1}',
  `100` = '10^{2}',
  `1000` = '10^{3}',
  `10000` = '10^{4}',
  `1e+05` = '10^{5}',
  `1e+06` = '10^{6}',
  `1e+07` = '10^{7}'
)

#change virus to low and high
ggplot(allhost_allsteps %>% filter (virus=="high") %>% filter (!(hostnum %in% c(1, 1e+07))), aes(x=group, y=log10(mid), color=group, shape=group))  + 
  geom_point (size=6) + 
  geom_errorbar(aes (ymax=log10(mid + stormy), ymin= log10(mid-calm)), width=0.5, size=1) +
  facet_grid(hostnum~parameter, labeller = labeller(parameter = as_labeller(variable_labs, label_parsed), 
                                                    hostnum = as_labeller(host_labs, label_parsed))) +
  theme_Publication2() + theme (axis.title.x = element_blank(), legend.position = "none") + labs (y = expression(log[10])) + geom_hline(yintercept = log10(1), linetype="dashed") + scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a"))
