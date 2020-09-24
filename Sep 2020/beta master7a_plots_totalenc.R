##Workspace: beta_master7

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

#edited as lab suggested
resize.win (6,5)
ggplot(data=PIC, aes(x=PICpercellpg, y=SinkVel, color=group)) + geom_point(aes(size=Den_celltotal))+
  scale_size(range = c(1,10)) + theme_Publication2()+
  labs(y = expression("sinking velocity "~("m"~d^-1)), x = expression("PIC pg"~cell^-1)) + 
  theme(legend.direction = "vertical", legend.box = "vertical", legend.position="right", legend.title = element_blank()) +
  scale_color_manual(values=c("#e41a1c","#377eb8")) + scale_shape_manual(values=c(1:11)) +
    guides(colour = guide_legend(override.aes = list(size=5)))

resize.win (6,6)
ggplot(data=PIC, aes(x=PICpercellpg, y=log10(beta_DS), color=group, shape=Strain)) + geom_point(size=5)+theme_Publication2()+
  labs(y = expression(log[10]~beta[S]~("encounters "~mL~day^-1)), x = expression("PIC pg"~cell^-1))  +
  theme(legend.direction = "horizontal", legend.box = "vertical")+  
  scale_color_manual(values=c("#e41a1c", "#377eb8"))  + scale_shape_manual(values=c(1:12))

PIC$group <- factor (PIC$group,labels= c("naked", "calcified"))
PIC$group <- reorder.factor (PIC$group, new.order = c("naked", "calcified"))
liths <- data.frame (group=c("lith"), Den_celltotal =2.6, SinkVel = 0.45, beta_DS = 2.70*10^-6)

#edited as lab suggested
resize.win (6,6)
ggplot(data=PIC, aes(x=SinkVel, y=log10(beta_DS), color=group, shape=Strain)) + geom_point(size=5)+theme_Publication2()+
  geom_point (data=liths, aes(x=SinkVel, y=log10(beta_DS), color="lith"), size=9, shape=20) +
  labs(y = expression(log[10]~beta[S]~("encounters "~mL~d^-1)), x = expression("sinking velocity "~("m"~d^-1)))  +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank())+  
  scale_color_manual(values=c( "#377eb8", "#4daf4a",  "#e41a1c"))  + scale_shape_manual(values=c(1:12))
  
resize.win(5,5)
#Predicted data (equally spaced data) sinkvel
PIC_newdata$group <- factor (PIC_newdata$group,labels= c("naked", "calcified"))
ggplot(data=PIC_newdata, aes(x=PICpercellpg, y=SinkVel, color=group)) + geom_point(size=5)+theme_Publication2()+
  labs(y = expression("sinking velocity "~("m"~d^-1)), x = expression("PIC pg"~cell^-1)) +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) +
  scale_color_manual(values=c("#e41a1c", "#377eb8"))

#Predicted data (equally spaced data) betas, with liths, beta vs sinkvel
ggplot(data=PIC_newdata, aes(x=SinkVel, y=log10(beta_DS), color=group)) + geom_point(size=5)+theme_Publication2()+
  geom_point (data=liths, aes(x=SinkVel, y=log10(beta_DS), color="lith"), size=9, shape=20) +
  labs(y = expression(log[10]~beta[S]~("encounters "~mL~d^-1)), x = expression("sinking velocity "~("m"~d^-1)))  +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) +  
  scale_color_manual(values=c("#377eb8", "#4daf4a", "#e41a1c"))  

#Predicted data (equally spaced data) betas, without liths, beta vs. PIC
ggplot(data=PIC_newdata, aes(x=PICpercellpg, y=log10(beta_DS), color=group)) + geom_point(size=5)+theme_Publication2()+
  labs(y = expression(log[10]~beta[S]~("encounters "~mL~d^-1)), x = expression("PIC pg"~cell^-1))  +
  theme(legend.direction = "horizontal", legend.box = "vertical", legend.title = element_blank()) +  
  scale_color_manual(values=c("#e41a1c", "#377eb8"))

#turbulence
resize.win(5,5)
turb$group <- factor (turb$group,labels= c("naked", "calcified", "lith"))
ggplot(data = turb, aes(x = log10(disrate), y = log10(beta_turb), color=group)) + geom_line(size =2) +
  theme_Publication2() +
  labs(y = expression(log[10]~beta[T]~("encounters "~mL~d^-1)), 
       x = expression(log[10]~epsilon~(m^2~s^-3))) +
  theme(legend.title = element_blank()) +
  scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) + guides(colour = guide_legend(override.aes = list(size=5)))

#encounters all betas
#viral encounters per day per cell
resize.win(7.5,6)
all$group <- reorder.factor (all$group, new.order = c("Nc", "Cc", "Li"))
all$group <- factor (all$group,labels= c("naked", "calcified", "lith"))

ggplot(data=all, aes(x=log10(disrate),y = log10(E_all_lowhost) , color=group)) + 
  geom_line(size=2, position=position_jitter(w=0.02, h=0), aes(linetype="shelf slope"))+
  geom_line(size=2, data = all, aes(y= log10(E_all_highhost), color=group,linetype="open ocean")) +
  theme_Publication2() +
  theme(legend.title = element_blank(), legend.key.width=unit(1,"cm"))+
  labs(y = expression(log[10]~"viral encounters " ~entity^-1~d^-1), x = expression(log[10]~epsilon~(m^2~s^-3))) +
  theme(legend.title = element_blank()) +
  scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) + 
  scale_fill_manual (values=c("#e41a1c", "#377eb8", "#4daf4a"))

#for all encounters
resize.win (7.5,6)
ggplot(data=all, aes(x=log10(disrate),y = log10(E_all_low_resvi) , color=group, fill=group)) + 
  geom_line(size=2, position=position_jitter(w=0.02, h=0), aes(linetype="open ocean"))+
  geom_line(size=2, data = all, aes(y= log10(E_all_high_resvi), color=group, linetype="shelf/slope")) +
  theme_Publication2() +
  theme(legend.title = element_blank(), legend.key.width=unit(1,"cm"))+
  labs(y = expression(log[10]~"total encounters " ~mL^-1~d^-1), x = expression(log[10]~epsilon~(m^2~s^-3))) +
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

##probabilities check alpha v5 final
probs <- as.data.frame(list (group = as.factor (rep(c("calcified","lith", "naked"), 4)), virus = rep(c("high", "low"), 1, each=6), condition=rep(c("open ocean", "shelf/slope"), 2, each=3), hostnum = rep(c(10^3, 10^3, 10^2, 10^5, 10^5, 10^4), 1),  virnum = rep(c(((10^3)*30), ((10^5)*30)), 2, each=3), prophost = rep(c(1, 25, 0.1), 4), propvir= rep(c(0.33, 0.67), 1, each=6), ads = rep(c(0.0095, 0.0134, 0.0305), 4), inf = rep(c(0.3, NA, 0.3, 0.3, NA, 0.3, 0.06, NA, 0.06, 0.06, NA, 0.06))))

calmstormy_backup <- calmstormy
calmstormy <- calmstormy_backup

#join calmstormy and probs
calmstormy <- left_join(calmstormy, probs)

#calculate propEhV
calmstormy$propEhV <- calmstormy$virnum* calmstormy$propvir

#calculate prophost
calmstormy$prophostnum <- calmstormy$hostnum* calmstormy$prophost

#calculate encounters fast slow
calmstormy$encounters <- calmstormy$beta_all*calmstormy$prophostnum*calmstormy$propEhV #total encounters

#calculate total adsorption by virus props
calmstormy$adstot <- calmstormy$encounters*calmstormy$ads

#calculate total successful infections
calmstormy$sucinf <- calmstormy$encounters*calmstormy$ads*calmstormy$inf

calmstormy$group <- reorder.factor (calmstormy$group, new.order = c("naked", "calcified", "lith"))

#edit in excel to make life easier
write.table(calmstormy, "Postdoc-R/Exported Tables/calmstormy_master7a_totalenc.csv", sep=";", col.names=T, row.names=F)

#make individual data table for getting the distilled plot
conc <- calmstormy[1:12, ] %>% select (group, virus, condition, prophostnum)
conc$parameter="concentration"
conc$calm <- NA
conc$stormy <- NA
conc$mid <-conc$prophostnum

enc <- reshape(calmstormy %>% select(group, watcon, virus, condition, prophostnum, encounters), idvar=c("group", "virus", "condition", "prophostnum"), timevar="watcon", direction="wide")
enc$parameter= "encounters"
ads <- reshape(calmstormy %>% select(group, watcon, virus, condition, prophostnum, adstot), idvar=c("group", "virus", "condition", "prophostnum"), timevar="watcon", direction="wide")
ads$parameter= "adsorption"
inf <- reshape(calmstormy %>% select(group, watcon, virus, condition, prophostnum, sucinf), idvar=c("group", "virus", "condition", "prophostnum"), timevar="watcon", direction="wide")
inf$parameter= "infections"

#renaming
enc <- setnames (enc, c("encounters.calm", "encounters.stormy", "encounters.mid"), c("calm", "stormy", "mid"))
ads <- setnames (ads, c("adstot.calm", "adstot.stormy", "adstot.mid"), c("calm", "stormy", "mid"))
inf <- setnames (inf, c("sucinf.calm", "sucinf.stormy", "sucinf.mid"), c("calm", "stormy", "mid"))

#combined
allsteps <- rbind (conc, enc, ads, inf)

resize.win(6,6)

#correct
ggplot(enc, aes(x=as.factor(group), y=log10(mid), color=as.factor(group), shape=as.factor(group))) +
  geom_point (size=5) + geom_errorbar(aes (ymax=log10(mid + stormy), ymin= log10(mid-calm))) + facet_grid(condition~virus)

##make the plot

variable_labs <- c(
  `concentration` = 'concentration~(mL^{-1})',
  `encounters` = 'encounters~entity^{-1}~d^{-1}',
  `adsorption` = 'adsorptions~entity^{-1}~d^{-1}',
  `infections` = 'infections~entity^{-1}~d^{-1}'
)

resize.win (11,7)#change virus to low and high
allsteps$parameter <- reorder.factor (allsteps$parameter, new.order = c("concentration", "encounters", "adsorption", "infections"))

ggplot(allsteps %>% filter (virus=="high"), aes(x=group, y=log10(mid), color=group, shape=group))  + 
  geom_point (size=6) + 
  geom_errorbar(aes (ymax=log10(mid + stormy), ymin= log10(mid-calm)), width=0.5, size=1) +
  facet_grid(condition~parameter,labeller = labeller(parameter = as_labeller(variable_labs, label_parsed))) +
  theme_Publication2() + theme (axis.title.x = element_blank(), legend.position = "none") + labs (y = expression(log[10])) + geom_hline(yintercept = log10(1), linetype="dashed") + scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) 

##edited according to the lab
library(lemon)
resize.win (11.5,7)
ggplot(allsteps %>% filter (virus=="high"), aes(x=group, y=log10(stormy), color="stormy"))  + 
  geom_point (size=6, position = position_dodge()) + 
  geom_point (data=allsteps %>% filter (virus=="high"), aes (x=group, y=log10(mid), color="mid"), size=6, position = position_dodge()) +
  geom_point (data=allsteps %>% filter (virus=="high"), aes (x=group, y=log10(calm), color="calm"), size=6, 
              position = position_dodge()) +
  facet_rep_grid(condition~parameter, ##tick marks 
                 labeller = labeller(parameter = as_labeller(variable_labs, label_parsed))) +
  theme_Publication2() + theme (axis.title.x = element_blank(), 
                                strip.background.x  = element_blank(),
                                axis.ticks.length = unit(5, "pt"),panel.spacing.y = unit(1, "lines"), legend.title = element_blank()) + 
  labs (y = expression(log[10])) + 
  geom_hline(yintercept = log10(1), linetype="dashed") + 
  scale_color_manual (values=c('#2b57a7', '#ffc4b4', '#b11346')) 

##calculate days to encounter
allsteps$days.calm <- 1/allsteps$calm
allsteps$days.mid <- 1/allsteps$mid
allsteps$days.stormy <- 1/allsteps$stormy

variable_labs2 <- c(
  `concentration` = 'concentration~(mL^{-1})',
  `encounters` = 'days~to~encounters',
  `adsorption` = 'days~to~adsorptions',
  `infections` = 'days~to~infections'
)

resize.win (10.5,8)

ggplot(allsteps %>% filter (virus=="low") %>% filter (!(parameter=="concentration")), aes(x=group, y=log10(days.stormy), color="stormy"))  + 
  geom_point (size=6, position = position_dodge()) + 
  geom_point (data=allsteps %>% filter (virus=="low") %>% filter (!(parameter=="concentration")), aes (x=group, y=log10(days.mid), color="mid"), size=6, position = position_dodge()) +
  geom_point (data=allsteps %>% filter (virus=="low") %>% filter (!(parameter=="concentration")), aes (x=group, y=log10(days.calm), color="calm"), size=6, 
              position = position_dodge()) +
  facet_rep_grid(condition~parameter, ##tick marks 
                 labeller = labeller(parameter = as_labeller(variable_labs2, label_parsed))) +
  theme_Publication2() + theme (axis.title.x = element_blank(), 
                                strip.background.x  = element_blank(),
                                axis.ticks.length = unit(5, "pt"),panel.spacing.y = unit(1, "lines"), legend.title = element_blank()) + 
  labs (y = expression(log[10])) + 
  geom_hline(yintercept = log10(1), linetype="dashed") + 
  scale_color_manual (values=c('#2b57a7', '#ffc4b4', '#b11346')) 

#stick with host
#allsteps split
#combined high and low
#####notes, since its a per entity sense, open ocean and shelf/slope has same percentages. maybe it makes sense to combine enc, ads, inf in one with different conditions

high <- reshape(allsteps %>% filter (virus=="high") %>% select (c (group, condition, prophostnum, parameter, mid)), idvar=c("group", "condition", "prophostnum"), timevar="parameter", direction="wide")
low <- reshape(allsteps %>% filter (virus=="low") %>% select (c (group, condition, prophostnum, parameter, mid)), idvar=c("group", "condition", "prophostnum"), timevar="parameter", direction="wide")

allsteps_comb <- high %>% select (c(group, condition, prophostnum))

#combine high and low, chose mid turbulence
allsteps_comb$enccomb <- high$mid.encounters + low$mid.encounters
allsteps_comb$adscomb <- high$mid.adsorption + low$mid.adsorption
allsteps_comb$infcomb <- high$mid.infections + low$mid.infections

##percentage of parameters
allsteps_comb$perencounters <- (allsteps_comb$enccomb/allsteps_comb$prophostnum)*100
allsteps_comb$peradsorbed <- (allsteps_comb$adscomb/allsteps_comb$prophostnum)*100
allsteps_comb$perinf <- (allsteps_comb$infcomb/allsteps_comb$prophostnum)*100
#allsteps_comb <- allsteps_comb %>% mutate(perencounters= if_else(perencounters > 1, 1, perencounters)) %>% mutate(peradsorbed= if_else(peradsorbed > 1, 1, peradsorbed)) %>% mutate(perinf= if_else(perinf > 1, 1, perinf)) 
allsteps_comb$perencounters_noenc <- 100- allsteps_comb$perencounters
allsteps_comb$perads_noads <- 100- allsteps_comb$peradsorbed
allsteps_comb$perinf_noinf <- 100- allsteps_comb$perinf

#melt data
melt_enc <- reshape2::melt(allsteps_comb %>% select ("group", "condition", "perencounters", "perencounters_noenc"), id.vars=c("group", "condition"))
melt_enc$parameter <- "encounters" 

melt_ads <- reshape2::melt(allsteps_comb %>% select ("group", "condition", "peradsorbed", "perads_noads" ), id.vars=c("group","condition"))
melt_ads$parameter <- "adsorptions"

melt_inf <- reshape2::melt(allsteps_comb %>% select ("group", "condition", "perinf", "perinf_noinf" ), id.vars=c("group", "condition"))
melt_inf$parameter <- "infections"

#encounters
resize.win (8,8)

#combine plots

melt_steps <- rbind (melt_enc, melt_ads, melt_inf)

melt_steps$parameter <- reorder.factor(melt_steps$parameter, new.order = c("encounters", "adsorptions", "infections"))

ggplot(melt_steps %>% filter (condition=="open ocean"), aes(x = "", y = value, fill=variable)) +
  geom_bar(width = 1, stat = "identity", color = "black") +   coord_polar("y", start = 0)+
  scale_fill_manual(values=c("#808000", "#FFFFFF", "#008080", "#FFFFFF", "#800080", "#FFFFFF")) + 
  theme_void() +  
  theme (legend.position = "bottom", legend.text = element_text(size = 15), 
         strip.text = element_text(size=15), legend.title=element_blank()) + facet_grid(parameter~group) + 
  geom_text(aes(y = value, label=sprintf("%0.6f", round(value, digits = 6))), size=5, position = position_stack(vjust = 0.5))

#####under stormy conditions percentages
#combined high.stormy and low.stormy
high.stormy<- reshape(allsteps %>% filter (virus=="high") %>% select (c (group, condition, prophostnum, parameter, stormy)), idvar=c("group", "condition", "prophostnum"), timevar="parameter", direction="wide")
low.stormy <- reshape(allsteps %>% filter (virus=="low") %>% select (c (group, condition, prophostnum, parameter, stormy)), idvar=c("group", "condition", "prophostnum"), timevar="parameter", direction="wide")

allsteps_comb.stormy <- high.stormy %>% select (c(group, condition, prophostnum))

#combine high.stormy and low.stormy, chose mid turbulence
allsteps_comb.stormy$enccomb <- high.stormy$stormy.encounters + low.stormy$stormy.encounters
allsteps_comb.stormy$adscomb <- high.stormy$stormy.adsorption + low.stormy$stormy.adsorption
allsteps_comb.stormy$infcomb <- high.stormy$stormy.infections + low.stormy$stormy.infections

##percentage of parameters
allsteps_comb.stormy$perencounters <- (allsteps_comb.stormy$enccomb/allsteps_comb.stormy$prophostnum)*100
allsteps_comb.stormy$peradsorbed <- (allsteps_comb.stormy$adscomb/allsteps_comb.stormy$prophostnum)*100
allsteps_comb.stormy$perinf <- (allsteps_comb.stormy$infcomb/allsteps_comb.stormy$prophostnum)*100
#allsteps_comb.stormy <- allsteps_comb.stormy %>% mutate(perencounters= if_else(perencounters > 1, 1, perencounters)) %>% mutate(peradsorbed= if_else(peradsorbed > 1, 1, peradsorbed)) %>% mutate(perinf= if_else(perinf > 1, 1, perinf)) 

allsteps_comb.stormy$perencounters_noenc <- 100- allsteps_comb.stormy$perencounters
allsteps_comb.stormy$perads_noads <- 100- allsteps_comb.stormy$peradsorbed
allsteps_comb.stormy$perinf_noinf <- 100- allsteps_comb.stormy$perinf

#melt data
melt_enc.stormy <- reshape2::melt(allsteps_comb.stormy %>% select ("group", "condition", "perencounters", "perencounters_noenc"), id.vars=c("group", "condition"))
melt_enc.stormy$parameter <- "encounters" 

melt_ads.stormy <- reshape2::melt(allsteps_comb.stormy %>% select ("group", "condition", "peradsorbed", "perads_noads" ), id.vars=c("group","condition"))
melt_ads.stormy$parameter <- "adsorptions"

melt_inf.stormy <- reshape2::melt(allsteps_comb.stormy %>% select ("group", "condition", "perinf", "perinf_noinf" ), id.vars=c("group", "condition"))
melt_inf.stormy$parameter <- "infections" 

#combine plots

melt_steps.stormy <- rbind (melt_enc.stormy, melt_ads.stormy, melt_inf.stormy)

melt_steps.stormy$parameter <- reorder.factor(melt_steps.stormy$parameter, new.order = c("encounters", "adsorptions", "infections"))

ggplot(melt_steps.stormy %>% filter (condition=="open ocean"), aes(x = "", y = value, fill=variable)) +
  geom_bar(width = 1, stat = "identity", color = "black") +   coord_polar("y", start = 0)+
  scale_fill_manual(values=c("#808000", "#FFFFFF", "#008080", "#FFFFFF", "#800080", "#FFFFFF")) + 
  theme_void() +  
  theme (legend.position = "bottom", legend.text = element_text(size = 15), 
         strip.text = element_text(size=15), legend.title=element_blank()) + facet_grid(parameter~group) + 
  geom_text(aes(y = value, label=sprintf("%0.6f", round(value, digits = 6))), size=5, position = position_stack(vjust = 0.5))

####under calm conditions percentages
#combined high.calm and low.calm
high.calm<- reshape(allsteps %>% filter (virus=="high") %>% select (c (group, condition, prophostnum, parameter, calm)), idvar=c("group", "condition", "prophostnum"), timevar="parameter", direction="wide")
low.calm <- reshape(allsteps %>% filter (virus=="low") %>% select (c (group, condition, prophostnum, parameter, calm)), idvar=c("group", "condition", "prophostnum"), timevar="parameter", direction="wide")

allsteps_comb.calm <- high.calm %>% select (c(group, condition, prophostnum))

#combine high.calm and low.calm, chose mid turbulence
allsteps_comb.calm$enccomb <- high.calm$calm.encounters + low.calm$calm.encounters
allsteps_comb.calm$adscomb <- high.calm$calm.adsorption + low.calm$calm.adsorption
allsteps_comb.calm$infcomb <- high.calm$calm.infections + low.calm$calm.infections

##percentage of parameters
allsteps_comb.calm$perencounters <- (allsteps_comb.calm$enccomb/allsteps_comb.calm$prophostnum)*100
allsteps_comb.calm$peradsorbed <- (allsteps_comb.calm$adscomb/allsteps_comb.calm$prophostnum)*100
allsteps_comb.calm$perinf <- (allsteps_comb.calm$infcomb/allsteps_comb.calm$prophostnum)*100
#allsteps_comb.calm <- allsteps_comb.calm %>% mutate(perencounters= if_else(perencounters > 1, 1, perencounters)) %>% mutate(peradsorbed= if_else(peradsorbed > 1, 1, peradsorbed)) %>% mutate(perinf= if_else(perinf > 1, 1, perinf)) 

allsteps_comb.calm$perencounters_noenc <- 100- allsteps_comb.calm$perencounters
allsteps_comb.calm$perads_noads <- 100- allsteps_comb.calm$peradsorbed
allsteps_comb.calm$perinf_noinf <- 100- allsteps_comb.calm$perinf

#melt data
melt_enc.calm <- reshape2::melt(allsteps_comb.calm %>% select ("group", "condition", "perencounters", "perencounters_noenc"), id.vars=c("group", "condition"))
melt_enc.calm$parameter <- "encounters"

melt_ads.calm <- reshape2::melt(allsteps_comb.calm %>% select ("group", "condition", "peradsorbed", "perads_noads" ), id.vars=c("group","condition"))
melt_ads.calm$parameter <- 'adsorptions'

melt_inf.calm <- reshape2::melt(allsteps_comb.calm %>% select ("group", "condition", "perinf", "perinf_noinf" ), id.vars=c("group", "condition"))
melt_inf.calm$parameter <- "infections"

#combine plots

melt_steps.calm <- rbind (melt_enc.calm, melt_ads.calm, melt_inf.calm)

melt_steps.calm$parameter <- reorder.factor(melt_steps.calm$parameter, new.order = c("encounters", "adsorptions", "infections"))

ggplot(melt_steps.calm %>% filter (condition=="open ocean"), aes(x = "", y = value, fill=variable)) +
  geom_bar(width = 1, stat = "identity", color = "black") +   coord_polar("y", start = 0)+
  scale_fill_manual(values=c("#808000", "#FFFFFF", "#008080", "#FFFFFF", "#800080", "#FFFFFF")) + 
  theme_void() +  
  theme (legend.position = "bottom", legend.text = element_text(size = 15), 
         strip.text = element_text(size=15), legend.title=element_blank()) + facet_grid(parameter~group) + 
  geom_text(aes(y = value, label=sprintf("%0.6f", round(value, digits = 6))), size=5, position = position_stack(vjust = 0.5))

