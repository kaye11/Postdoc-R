source("inspack.R")
#readin data
library(readxl)
lowdens <- read_excel("Postdoc-R/Exported Tables/encounters 10 to the 3 host_corden.xlsx")

lowdens.trim <- lowdens %>% select (c("group1", "group2", "disrate","beta_all", "E_all"))
lowdensext <- lowdens.trim[rep(seq_len(nrow(lowdens.trim)), 125), ]

#make variables
#biological part of model is adsorption (ads), infectivity (inf), and host susceptebility (hst). All are probabilities. 

probs <- expand.grid(list (ads = (seq(0,1, 0.25)), inf = (seq(0,1, 0.25)), hst = (seq(0,1, 0.25)) ))

probsext<- probs[rep(seq_len(nrow(probs)), 112), ]

lowdensext$ads <- probsext$ads
lowdensext$inf2 <- probsext$inf
lowdensext$hst2 <- probsext$hst

#consider that inf is only for viruses and hst is only for hosts
lowdensext<- data.frame(lowdensext, inf = ifelse(lowdensext$group1=="Vi", lowdensext$inf2, ifelse (lowdensext$group2=="Vi",lowdensext$inf2, "NA")))

lowdensext<- data.frame(lowdensext, hst = ifelse(lowdensext$group1 %in% c("Cc", "Nc"), lowdensext$hst2, ifelse (lowdensext$group2 %in% c("Cc", "Nc"),lowdensext$hst2, "NA"))) 

lowdensext$inf <- as.numeric(as.character(lowdensext$inf))
lowdensext$hst <- as.numeric(as.character(lowdensext$hst))
lowdensext$inf2 <- NULL
lowdensext$hst2 <- NULL

#compute biological infectability
lowdensext$bioinf <- apply((lowdensext[,c("ads", "inf", "hst")]), 1, prod, na.rm=FALSE)
lowdensext$bioinf_nohst <- apply((lowdensext[,c("ads", "inf")]), 1, prod, na.rm=FALSE)


ggplot(data=lowdensext %>% filter (hst=="1"), aes(x=log10(disrate),y = bioinf, color=group1)) + 
  geom_point(size=1, position=position_jitter(w=0.02, h=0)) + theme_Publication() +
  theme(legend.title = element_blank(), legend.key.width=unit(1,"cm"))+
  facet_grid(group1~inf+ads)

#STOPPED HERE
#how to calculate this from beta because you need counts
lowdensext$sucinf <- apply((lowdensext[,c("E_all", "bioinf")]), 1, prod, na.rm=FALSE)
lowdensext$sucinf_nohst <- apply((lowdensext[,c("E_all", "bioinf_nohst")]), 1, prod, na.rm=FALSE)

ggplot(data=lowdensext %>% filter (group2=="Nc"), aes(x=log10(disrate),y = sucinf, color=group1)) + 
  geom_line(size=1, position=position_jitter(w=0.02, h=0)) + theme_Publication() +
  theme(legend.title = element_blank(), legend.key.width=unit(1,"cm"))+
  facet_grid(ads~inf+hst)

lowdensext$group1 <- factor(lowdensext$group1, levels=c("Cc", "Nc", "Li", "Vi"))
lowdensext$group2 <- factor(lowdensext$group2, levels=c("Cc", "Nc", "Li", "Vi")) 

lowdensext$sucinf2 <- lowdensext$sucinf
lowdensext$sucinf2[lowdensext$sucinf2==0] <- NA


#if you want na.value add it on scale_fill with na.value="white"
ggplot(lowdensext, aes(x=group2, y=group1)) +
  geom_tile(aes(fill = log10(sucinf)), colour = "grey50") +
  scale_fill_viridis(option="cividis", na.value="white") +
  ylim(rev(levels(lowdensext$group1))) + labs (fill="log10 successful infection per day") + 
  theme_Publication() +
  theme(axis.title = element_blank(), legend.position = "bottom", legend.direction = "horizontal", legend.key.height=unit(1,"line"), legend.key.width = unit (2, "line")) +facet_grid(.~disrate)

ggplot(lowdensext %>% filter (disrate=="0.01"), aes(x=group2, y=group1)) +
  geom_tile(aes(fill = log10(sucinf)), colour = "grey50") +
  scale_fill_viridis(option="cividis", na.value="white") +
  ylim(rev(levels(as.factor(lowdensext$group1)))) + labs (fill="log10 successful infection per day") + 
  theme_Publication() +
  theme(axis.title = element_blank(), legend.position = "bottom", legend.direction = "horizontal", legend.key.height=unit(1,"line"), legend.key.width = unit (2, "line")) +facet_grid(ads~inf+hst)

lowdensext$combbio <- as.factor(paste(lowdensext$ads, lowdensext$inf, lowdensext$hst, sep="-"))

lowdensext.noNA <- lowdensext[!is.na(lowdensext$sucinf), ]

ggplot(data=lowdensext.noNA, aes(x=log10(disrate),y = log10(sucinf), color=combbio)) +   geom_line(size=1, position=position_jitter(w=0.02, h=0)) +
  theme_Publication() +
  theme(legend.title = element_blank(), legend.key.width=unit(0.5,"cm"), legend.position = "right")+
  facet_grid(group1~group2)

#no idea on how to plot it
lowdensext.noNA$sucinf_ads <- lowdensext.noNA$E_all*lowdensext.noNA$ads

sum.ads <- summarySE (lowdensext.noNA, measurevar = "sucinf_ads", 
                      groupvars = c ("group1", "group2", "disrate", "ads"))

#ads data only plot
ggplot(data=sum.ads %>% filter (!(ads=="0")) , aes(x=log10(disrate),y = log10(sucinf_ads), color=as.factor(ads))) +   geom_line(size=0.8) +
  theme_Publication() +
  theme(legend.title = element_blank(), legend.key.width=unit(0.5,"cm"), legend.position = "bottom")+
  facet_grid(group1~group2)

sum.ads.inf <- summarySE (lowdensext.noNA, measurevar = "sucinf_nohst", 
                      groupvars = c ("group1", "group2", "disrate", "ads", "inf"))

sum.ads.inf$comb_ads_inf <- as.factor(paste(sum.ads.inf$ads, sum.ads.inf$inf, sep="-"))

ggplot(data=sum.ads.inf %>% filter (!(ads=="0")) , aes(x=log10(disrate),y = log10(sucinf_nohst), color=comb_ads_inf))+   geom_line(size=0.8) +
  theme_Publication() +
  theme(legend.title = element_blank(), legend.key.width=unit(0.5,"cm"), legend.position = "bottom")+
  facet_grid(group1~group2)


###try Hannah's suggestion
#add Cc-Vi and Vi-Cc, Nc-Vi and Vi-Nc encounters later

lowdensext.noNA$group12 <- as.factor(paste(lowdensext.noNA$group1, lowdensext.noNA$group2, sep="-"))

ggplot(data = lowdensext.noNA, aes(x=as.factor(log10(disrate)), y=log10(sucinf), color=group12)) +  stat_boxplot(geom ='errorbar', width = 0.6) +  geom_boxplot(width = 0.6) +
  labs (y= "log10 successful infection per day", x= "dissipation rate") +
  theme_Publication2() + theme (legend.title = element_blank())

ggplot(data = lowdensext.noNA, aes(x=disrate, y=log10(sucinf), color=group12)) + geom_point() + geom_smooth() +
  labs (y= "log10 successful infection per day", x= "dissipation rate") +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=7),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks()+
  theme_Publication2() + theme (legend.title = element_blank())

lowdensext.noNA_grp1 <- lowdensext.noNA %>% filter(group1 %in% c("Cc", "Nc"))
lowdensext.noNA_grp2 <- lowdensext.noNA %>% filter(group2 %in% c("Cc", "Nc"))

lowdensext.noNA_grp1$sucinf_grp12 <- lowdensext.noNA_grp1$sucinf + lowdensext.noNA_grp2$sucinf

ggplot(data = lowdensext.noNA_grp1, aes(x=as.factor(log10(disrate)), y=log10(sucinf_grp12), color=group12)) + 
  geom_boxplot() +
  labs (y= "log10 successful infection per day", x= "dissipation rate") +
  theme_Publication2() + theme (legend.title = element_blank())

##for viroliths

lowdensext.lith <- lowdensext %>% filter(group1 %in% c("Li")) %>% filter(group2 %in% c("Li")) %>% arrange(desc(disrate)) %>% arrange (desc(c(ads))) %>% arrange (desc(c(inf))) %>% arrange (desc(c(hst)))
lowdensext.nc <- lowdensext %>% filter(group1 %in% c("Nc")) %>% filter(group2 %in% c("Nc")) %>% arrange(desc(disrate)) %>% arrange (desc(ads)) %>% arrange (desc(c(inf))) %>% arrange (desc(c(hst)))
lowdensext.vir <- lowdensext %>% filter(group1 %in% c("Vi")) %>% filter(group2 %in% c("Vi")) %>% arrange(desc(disrate)) %>% arrange (desc(ads)) %>% arrange (desc(c(inf))) %>% arrange (desc(c(hst)))
