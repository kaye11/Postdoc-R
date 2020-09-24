#Note: this is the merged data set from Ali and Toby, where waterdepth<100m and abundance<10 were eliminated from the dataset. This is a melted dataset where entities (i.e., Cc and Li) are just in one column
NAdata_merge <- read.csv("D:/Users/karengb/Exported Tables/NAdata_merge.csv")
source("inspack_map.R")

#density plots
resize.win (10, 5)

#200
ggplot (data=NAdata_merge, aes(x=log10(abundance), fill=depthf)) + 
  geom_density(position = "identity", alpha=0.5) + scale_x_continuous(breaks=seq(1,6,1)) +
  theme_Publication2() + labs (x = expression("log abundance"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit(2, "line")) + 
  facet_grid(~entity)

#calling world
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#ggplot version of abundance
resize.win(10,5)
abundance.png <- ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=NAdata_merge %>%  filter(entitycode %in% c("Ncells_max", "Nliths_max")), 
             aes(x = lon, y = lat, color = log10(abundance)))+ scale_color_viridis(breaks = c(seq(0, 7, 1))) + 
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + facet_grid(~entity)

ggsave(filename = "abundance.png", width = 10, height = 5, dpi = 600, device="png", units="in")

resize.win(6,5)
#disrate_upper1m
disrate_1m.png <- ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=NAdata_merge %>% filter (entity=="Cc"), aes(x = lon, y = lat, color = log10(disrate_1m)))+
  scale_color_viridis_c (na.value = "white") + #expand_limits(colour = c(seq(-7, -3, 1))) +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + labs (title= expression("Maximum dissipation rate"~m^2~s^-3~"(1m depth)"))

ggsave(filename = "disrate_1m.png", width = 6, height = 5, dpi = 600, device="png", units="in")

#disrate_30m
disrate_30m.png <- ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=NAdata_merge, aes(x = lon, y = lat, color = log10(disrate_30m)))+
  scale_color_viridis_c (na.value = "white") +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + labs (title= expression("Maximum dissipation rate"~m^2~s^-3~"(30m depth)"))

ggsave(filename = "disrate_30m.png", width = 6, height = 5, dpi = 600, device="png", units="in")


##betas
betas_BM_DS <- read.csv("D:/Users/karengb/CSV Files/betas_BM_DS.txt")

#calculate beta turb
NAdata.betas <- left_join(NAdata_merge, betas_BM_DS)
v= 1.099*(10)^-6 #m2/s kinematic viscosity in 18C
Rehv= 90*(10)^-9 #in m radius virus

#calculate viral abundance
NAdata.betas$virnum <- NAdata.betas$abundance * 10

#split 1m and 30m
NAdata.betas_1m = NAdata.betas %>% select(!(disrate_30m))
#1m
NAdata.betas_1m$beta_turb <- (4.2*pi*((NAdata.betas_1m$disrate_1m/(v*100^2))^0.5)*(((NAdata.betas_1m$rad+Rehv)*100)^3))*86400 
NAdata.betas_1m$beta_all <- NAdata.betas_1m$beta_BM + NAdata.betas_1m$beta_DS + NAdata.betas_1m$beta_turb

#probabilities
probs <- as.data.frame(list (entity = as.factor (rep(c("Cc","Li"), 2)), virus = rep(c("high", "low"), 1, each=8), propvir= rep(c(0.33, 0.67), 1, each=8), ads = rep(c(0.0169, 0.0206), 4), inf = rep(c(0.3, NA,0.3, NA, 0.3, NA, 0.3, NA, 0.06, NA, 0.06, NA, 0.06, NA, 0.06, NA), 1, each=1)))

NAdata.betas_1m$entity <- as.factor(NAdata.betas_1m$entity)

#join NAdata and probs
NAdata.betas_1m <- left_join(NAdata.betas_1m, probs)

#calculate propEhV
NAdata.betas_1m$propEhV <- NAdata.betas_1m$virnum* NAdata.betas_1m$propvir

#calculate encounters fast slow
NAdata.betas_1m$encounters_propEhV <- NAdata.betas_1m$beta_all*NAdata.betas_1m$propEhV*NAdata.betas_1m$abundance #total enc

#calculate total adsorption by virus props
NAdata.betas_1m$adstot_prop <- NAdata.betas_1m$encounters_propEhV*NAdata.betas_1m$ads

#calculate total successful infections
NAdata.betas_1m$sucinf_prop <- NAdata.betas_1m$encounters_propEhV*NAdata.betas_1m$ads*NAdata.betas_1m$inf

#add everything
high_NAdata.betas_1m <- NAdata.betas_1m %>% filter (virus=="high")
low_NAdata.betas_1m <- NAdata.betas_1m %>% filter (virus=="low")

NAdata.betas_comb_1m <- NAdata.betas_1m %>% filter (virus=="high") %>% select (c(lat, lon, entity, depthf, abundance, virnum, disrate_1m))
NAdata.betas_comb_1m$enccomb <- high_NAdata.betas_1m$encounters_propEhV + low_NAdata.betas_1m$encounters_propEhV
NAdata.betas_comb_1m$adscomb <- high_NAdata.betas_1m$adstot_prop + low_NAdata.betas_1m$adstot_prop
NAdata.betas_comb_1m$infcomb <- high_NAdata.betas_1m$sucinf_prop + low_NAdata.betas_1m$sucinf_prop
#NAdata.betas_comb <- NAdata.betas_comb %>% mutate(adscomb2= if_else(adscomb > enccomb, adscomb, adscomb))

ads.sum.1m <- summarySE (data=NAdata.betas_comb_1m %>% filter (!(adscomb=="NA")), measurevar = "adscomb", groupvars = c("entity"))

##percentage of parameters
NAdata.betas_comb_1m$perencounters <- (NAdata.betas_comb_1m$enccomb/NAdata.betas_comb_1m$abundance)*100
NAdata.betas_comb_1m$peradsorbed <- (NAdata.betas_comb_1m$adscomb/NAdata.betas_comb_1m$abundance)*100
NAdata.betas_comb_1m$perinf <- (NAdata.betas_comb_1m$infcomb/NAdata.betas_comb_1m$abundance)*100
NAdata.betas_comb_1m <- NAdata.betas_comb_1m %>% mutate(perencounters= if_else(perencounters > 1, 1, perencounters)) %>% mutate(peradsorbed= if_else(perencounters > 1, 1, peradsorbed)) %>% mutate(perinf= if_else(perinf > 1, 1, perinf)) 

#melt dataset
melted_NAdata.betas_comb_1m <- reshape2::melt (NAdata.betas_comb_1m %>% select (lat, lon, entity, depthf, abundance, enccomb, adscomb, infcomb), id.vars = c("lat", "lon", "depthf", "entity"), value.name = "value", variable.name = "step")

melted_NAdata.betas_comb_1m$step <-  reorder.factor (melted_NAdata.betas_comb_1m$step, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 

melted_NAdata.betas_comb_1m$step2<- factor (melted_NAdata.betas_comb_1m$step, levels= c("abundance", "enccomb", "adscomb", "infcomb"), labels = c("abundance~mL^{-1}", "encounters~day^{-1}~mL^{-1}", "adsorptions~day^{-1}~mL^{-1}", "lytic~infections~\n~day^{-1}~mL^{-1}"))

resize.win(18,6)
Cc_sum.png <- ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=melted_NAdata.betas_comb_1m %>% filter (entity=="Cc"), aes(x = lon, y = lat, color = log10(value)))+ scale_color_viridis() +
  #scale_color_viridis(breaks = c(seq(-5, 9, 2))) + expand_limits(colour = c(seq(-5, 9, 2))) + 
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(),  legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + facet_grid(~step2, labeller = label_parsed) 
ggsave(filename = "Cc_sum.png", width = 18, height = 5, dpi = 600, device="png", units="in")

resize.win(14, 6)
Li_sum.png <- ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=melted_NAdata.betas_comb_1m %>% filter (entity=="Li") %>% filter (!(step=="infcomb")),
             aes(x = lon, y = lat, color = log10(value)))+ scale_color_viridis() +
  #scale_color_viridis(breaks = c(seq(-5, 9, 2))) + expand_limits(colour = c(seq(-5, 9, 2))) + 
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(),  legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + facet_grid(~step2, labeller = label_parsed)  

ggsave(filename = "Li_sum.png", width = 14, height = 5, dpi = 600, device="png", units="in")


#make a plot like NA-VICE but density plot
resize.win(17,5)
sum1.png <- ggplot (data=melted_NAdata.betas_comb_1m, aes(x=log10(value), fill=entity)) + 
  geom_density(position = "identity", alpha=0.5) + scale_color_manual (values=c("#377eb8", "#4daf4a")) + 
  scale_fill_manual (values=c("#377eb8", "#4daf4a")) + geom_vline(xintercept = log10(1), linetype="dashed") +
  scale_x_continuous(breaks=seq(-5, 9, 4)) +
  labs (x="log10") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines")) + facet_grid(~step2, labeller = label_parsed)

ggsave(filename = "sum1.png", width = 17, height = 5, dpi = 600, device="png", units="in")


###-----------------------------30m--------------------------##
#split 30m and 30m
NAdata.betas_30m = NAdata.betas %>% select(!(disrate_1m))
#30m
NAdata.betas_30m$beta_turb <- (4.2*pi*((NAdata.betas_30m$disrate_30m/(v*100^2))^0.5)*(((NAdata.betas_30m$rad+Rehv)*100)^3))*86400 
NAdata.betas_30m$beta_all <- NAdata.betas_30m$beta_BM + NAdata.betas_30m$beta_DS + NAdata.betas_30m$beta_turb

NAdata.betas_30m$entity <- as.factor(NAdata.betas_30m$entity)

#join NAdata and probs
NAdata.betas_30m <- left_join(NAdata.betas_30m, probs)

#calculate propEhV
NAdata.betas_30m$propEhV <- NAdata.betas_30m$virnum* NAdata.betas_30m$propvir

#calculate encounters fast slow
NAdata.betas_30m$encounters_propEhV <- NAdata.betas_30m$beta_all*NAdata.betas_30m$propEhV*NAdata.betas_30m$abundance #total enc

#calculate total adsorption by virus props
NAdata.betas_30m$adstot_prop <- NAdata.betas_30m$encounters_propEhV*NAdata.betas_30m$ads

#calculate total successful infections
NAdata.betas_30m$sucinf_prop <- NAdata.betas_30m$encounters_propEhV*NAdata.betas_30m$ads*NAdata.betas_30m$inf

#add everything
high_NAdata.betas_30m <- NAdata.betas_30m %>% filter (virus=="high")
low_NAdata.betas_30m <- NAdata.betas_30m %>% filter (virus=="low")

NAdata.betas_comb_30m <- NAdata.betas_30m %>% filter (virus=="high") %>% select (c(lat, lon, entity, depthf, abundance, virnum, disrate_30m))
NAdata.betas_comb_30m$enccomb <- high_NAdata.betas_30m$encounters_propEhV + low_NAdata.betas_30m$encounters_propEhV
NAdata.betas_comb_30m$adscomb <- high_NAdata.betas_30m$adstot_prop + low_NAdata.betas_30m$adstot_prop
NAdata.betas_comb_30m$infcomb <- high_NAdata.betas_30m$sucinf_prop + low_NAdata.betas_30m$sucinf_prop
#NAdata.betas_comb <- NAdata.betas_comb %>% mutate(adscomb2= if_else(adscomb > enccomb, adscomb, adscomb))

##percentage of parameters
NAdata.betas_comb_30m$perencounters <- (NAdata.betas_comb_30m$enccomb/NAdata.betas_comb_30m$abundance)*100
NAdata.betas_comb_30m$peradsorbed <- (NAdata.betas_comb_30m$adscomb/NAdata.betas_comb_30m$abundance)*100
NAdata.betas_comb_30m$perinf <- (NAdata.betas_comb_30m$infcomb/NAdata.betas_comb_30m$abundance)*100
NAdata.betas_comb_30m <- NAdata.betas_comb_30m %>% mutate(perencounters= if_else(perencounters > 1, 1, perencounters)) %>% mutate(peradsorbed= if_else(perencounters > 1, 1, peradsorbed)) %>% mutate(perinf= if_else(perinf > 1, 1, perinf)) 

#melt dataset
melted_NAdata.betas_comb_30m <- reshape2::melt (NAdata.betas_comb_30m %>% select (lat, lon, entity, depthf, abundance, enccomb, adscomb, infcomb), id.vars = c("lat", "lon", "depthf", "entity"), value.name = "value", variable.name = "step")

melted_NAdata.betas_comb_30m$step <-  reorder.factor (melted_NAdata.betas_comb_30m$step, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 

melted_NAdata.betas_comb_30m$step2<- factor (melted_NAdata.betas_comb_30m$step, levels= c("abundance", "enccomb", "adscomb", "infcomb"), labels = c("abundance~mL^{-1}", "encounters~day^{-1}~mL^{-1}", "adsorptions~day^{-1}~mL^{-1}", "lytic~infections~\n~day^{-1}~mL^{-1}"))

resize.win(18,6)
Cc_sum30.png <- ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=melted_NAdata.betas_comb_30m %>% filter (entity=="Cc"), aes(x = lon, y = lat, color = log10(value)))+ scale_color_viridis() +
  #scale_color_viridis(breaks = c(seq(-5, 9, 2))) + expand_limits(colour = c(seq(-5, 9, 2))) + 
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(),  legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + facet_grid(~step2, labeller = label_parsed) 
ggsave(filename = "Cc_sum30.png", width = 18, height = 5, dpi = 600, device="png", units="in")

resize.win(14, 6)
Li_sum30.png <- ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=melted_NAdata.betas_comb_30m %>% filter (entity=="Li") %>% filter (!(step=="infcomb")),
             aes(x = lon, y = lat, color = log10(value)))+ scale_color_viridis() +
  #scale_color_viridis(breaks = c(seq(-5, 9, 2))) + expand_limits(colour = c(seq(-5, 9, 2))) + 
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(),  legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + facet_grid(~step2, labeller = label_parsed)  

ggsave(filename = "Li_sum30.png", width = 14, height = 5, dpi = 600, device="png", units="in")


#make a plot like NA-VICE but density plot
resize.win(10,5)
sum30.png <- ggplot (data=melted_NAdata.betas_comb_30m, aes(x=log10(value), fill=entity)) + 
  geom_density(position = "identity", alpha=0.5) + scale_color_manual (values=c("#377eb8", "#4daf4a")) + 
  scale_fill_manual (values=c("#377eb8", "#4daf4a")) + geom_vline(xintercept = log10(1), linetype="dashed") +
  scale_x_continuous(breaks=seq(-5, 9, 4)) +
  labs (x="log10") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines")) + facet_grid(~step2, labeller = label_parsed)

ggsave(filename = "sum30.png", width = 17, height = 5, dpi = 600, device="png", units="in")
