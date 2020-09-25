#Note: this is the merged data set from Ali and Toby, where waterdepth<100m and abundance<10 were eliminated from the dataset. This is a melted dataset where entities (i.e., Cc and Li) are just in one column
NAdata_merge <- read.csv("D:/Users/karengb/Exported Tables/NAdata_merge_masked.csv")
source("inspack_map.R")

#density plots #change color schemes to colorblind friendly
resize.win (10, 5)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

#get data out of each histogram, and make an excel file out of it (saved in onedrive: density raw.
#used density_merged, saved in CSV files)

#tried getting data directly from R
Cc.shelf <- ggplot (data=NAdata_merge %>% filter (depthf=="<200 m") %>% filter (entity=="Cc"), 
                    aes(x=log10(abundance),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)*10))  + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

Cc.shelf <- ggplot_build(Cc.shelf)$data[[1]] %>% select (x, y)
Cc.shelf$entity <- "Cc"
Cc.shelf$depthf <- "<200 m"

Cc.open <- ggplot (data=NAdata_merge %>% filter (depthf==">200 m") %>% filter (entity=="Cc"), 
                   aes(x=log10(abundance),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)*10))  + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

Cc.open <- ggplot_build(Cc.open)$data[[1]] %>% select (x, y)
Cc.open$entity <- "Cc"
Cc.open$depthf <- ">200 m"

Li.shelf <- ggplot (data=NAdata_merge %>% filter (depthf=="<200 m") %>% filter (entity=="Li"), 
                   aes(x=log10(abundance),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)*10))  + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

Li.shelf <- ggplot_build(Li.shelf)$data[[1]] %>% select (x, y)
Li.shelf$entity <- "Li"
Li.shelf$depthf <- "<200 m"

Li.open <- ggplot (data=NAdata_merge %>% filter (depthf==">200 m") %>% filter (entity=="Li"), 
             aes(x=log10(abundance),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)*10))  + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

Li.open <- ggplot_build(Li.open)$data[[1]] %>% select (x, y)
Li.open$entity <- "Li"
Li.open$depthf <- ">200 m"

density_merged <- rbind (Cc.shelf, Cc.open, Li.shelf, Li.open)

#save data
write.table (density_merged, "Exported Tables/density_merged.csv", sep=",", col.names=T, row.names=F)

#get data from csv file 
#density_merged <- read.csv("D:/Users/karengb/CSV Files/density_all_merged.csv")

density_merged$depthf <- factor (density_merged$depthf, levels = c("<200 m", ">200 m"), labels= c("shelf slope", "open ocean"))

ggplot (data=density_merged, aes(x=x,  y= y, color=depthf, fill=depthf)) + 
  geom_area(position = "identity", alpha=0.3) +
  geom_line(size=0.7) + 
  geom_vline(xintercept = 1, linetype="dashed") +
  scale_x_continuous(breaks = c(-1:6)) +
  facet_grid(~entity) + theme_Publication2() + labs (x = expression(log[10]~"concentration"~mL^-1), y= "density probability") +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

#calling world
NAdata_merge$depthf <- factor (NAdata_merge$depthf, levels = c("<200 m", ">200 m"), labels= c("shelf slope", "open ocean"))

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#color scheme
BlRed <- colorRampPalette((c('#00429d', '#2b57a7', '#426cb0', '#5681b9', '#6997c2', '#7daeca', '#93c4d2', '#abdad9', '#caefdf', '#ffe2ca', '#ffc4b4', '#ffa59e', '#f98689', '#ed6976', '#dd4c65', '#ca2f55', '#b11346', '#93003a')))


#ggplot version of abundance
resize.win(10,5)
abundance.png <- ggplot(data = world)+ geom_sf(color="darkgray", fill="gainsboro") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+   geom_point(data=NAdata_merge %>%  filter(entitycode %in% c("Ncells_max", "Nliths_max")), 
             aes(x = lon, y = lat, color = log10(abundance)))+ 
  scale_colour_gradientn(colours = BlRed(100)) +    theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + facet_grid(~entity)

ggsave(filename = "abundance.png", width = 10, height = 5, dpi = 600, device="png", units="in")

##wind speed
maxwind.png <- ggplot(data = world)+ geom_sf(color="darkgray", fill="gainsboro") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=NAdata_merge, aes(x = lon, y = lat, color = max_wind_speed)) +
  scale_colour_gradientn(colours = BlRed(100)) + 
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + labs (title= expression("Wind speed("~m~s^-1~")"))

ggsave(filename = "maxwind.png", width = 6, height = 5, dpi = 600, device="png", units="in")

resize.win(6,5)
#disrate_upper1m
#make color legend same for 1m and 30m (main data is 30m)
disrate_1m.png <- ggplot(data = world)+ geom_sf(color="darkgray", fill="gainsboro") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=NAdata_merge %>% filter (entity=="Cc"), aes(x = lon, y = lat, color = log10(disrate_1m)))+
  scale_colour_gradientn(colours = BlRed(100)) +
  #, limits = c(min(log10(NAdata_merge$disrate_30m)), max (log10(NAdata_merge$disrate_30m)))) + 
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + labs (title= expression("Maximum dissipation rate"~m^2~s^-3~"(1m depth)"))

ggsave(filename = "disrate_1m.png", width = 6, height = 5, dpi = 600, device="png", units="in")

#disrate_30m
disrate_30m.png <- ggplot(data = world)+ geom_sf(color="darkgray", fill="gainsboro") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=NAdata_merge, aes(x = lon, y = lat, color = log10(disrate_30m)))+
  scale_colour_gradientn(colours = BlRed(100)) + 
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + labs (title= expression(log[10]~epsilon~(m^2~s^-3)))

ggsave(filename = "disrate_30m.png", width = 6, height = 5, dpi = 600, device="png", units="in")


##betas
betas_BM_DS <- read.csv("D:/Users/karengb/CSV Files/betas_BM_DS.txt") #radius are in m

#calculate beta turb
NAdata.betas <- left_join(NAdata_merge, betas_BM_DS)
v= 1.099*(10)^-6 #m2/s kinematic viscosity in 18C
Rehv= 90*(10)^-9 #in m radius virus

#calculate viral abundance, Cc:virus is 30
NAdata.betas$virnum <- NAdata.betas$abundance * 30

#split 1m and 30m
NAdata.betas_1m = NAdata.betas %>% select(!(disrate_30m))
#1m
NAdata.betas_1m$beta_turb <- (4.2*pi*((NAdata.betas_1m$disrate_1m/(v*100^2))^0.5)*(((NAdata.betas_1m$rad+Rehv)*100)^3))*86400 
NAdata.betas_1m$beta_all <- NAdata.betas_1m$beta_BM + NAdata.betas_1m$beta_DS + NAdata.betas_1m$beta_turb

#probabilities
probs <- as.data.frame(list (entity = as.factor (rep(c("Cc","Li"), 2)), virus = rep(c("high", "low"), 1, each=8), propvir= rep(c(0.33, 0.67), 1, each=8), ads = rep(c(0.0243, 0.0302), 4), inf = rep(c(0.3, NA,0.3, NA, 0.3, NA, 0.3, NA, 0.06, NA, 0.06, NA, 0.06, NA, 0.06, NA), 1, each=1)))

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

##percentage of parameters
NAdata.betas_comb_1m$perencounters <- (NAdata.betas_comb_1m$enccomb/NAdata.betas_comb_1m$abundance)*100
NAdata.betas_comb_1m$peradsorbed <- (NAdata.betas_comb_1m$adscomb/NAdata.betas_comb_1m$abundance)*100
NAdata.betas_comb_1m$perinf <- (NAdata.betas_comb_1m$infcomb/NAdata.betas_comb_1m$abundance)*100
NAdata.betas_comb_1m <- NAdata.betas_comb_1m %>% mutate(perencounters= if_else(perencounters > 1, 1, perencounters)) %>% mutate(peradsorbed= if_else(perencounters > 1, 1, peradsorbed)) %>% mutate(perinf= if_else(perinf > 1, 1, perinf)) 

#melt dataset
melted_NAdata.betas_comb_1m <- reshape2::melt (NAdata.betas_comb_1m %>% select (lat, lon, entity, depthf, abundance, enccomb, adscomb, infcomb), id.vars = c("lat", "lon", "depthf", "entity"), value.name = "value", variable.name = "step")

melted_NAdata.betas_comb_1m$step <-  reorder.factor (melted_NAdata.betas_comb_1m$step, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 

variable_labs <- c(
  `abundance` = 'concentration~(mL^{-1})',
  `enccomb` = 'encounter~rate~(mL^{-1}~d^{-1})',
  `adscomb` = 'adsorption~rate~(mL^{-1}~d^{-1})',
  `infcomb` = 'infection~rate~(mL^{-1}~d^{-1})'
)

#resize.win(18,6)
Cc_sum.png <- ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=melted_NAdata.betas_comb_1m %>% filter (entity=="Cc"), aes(x = lon, y = lat, color = log10(value)))+ scale_color_viridis() +
  #scale_color_viridis(breaks = c(seq(-5, 9, 2))) + expand_limits(colour = c(seq(-5, 9, 2))) + 
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(),  legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "Cc_sum.png", width = 18, height = 5, dpi = 600, device="png", units="in")

resize.win(14, 6)
Li_sum.png <- ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=melted_NAdata.betas_comb_1m %>% filter (entity=="Li") %>% filter (!(step=="infcomb")),
             aes(x = lon, y = lat, color = log10(value)))+ scale_color_viridis() +
  #scale_color_viridis(breaks = c(seq(-5, 9, 2))) + expand_limits(colour = c(seq(-5, 9, 2))) + 
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(),  legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "Li_sum.png", width = 14, height = 5, dpi = 600, device="png", units="in")


#make a plot like NA-VICE but density plot
resize.win(17,5)
sum1.png <- ggplot (data=melted_NAdata.betas_comb_1m, aes(x=log10(value), fill=entity)) + 
  geom_histogram(binwidth=0.5, aes(y=..density..), alpha=0.5) +
  geom_density(position = "stack", alpha=0.5) + scale_color_manual (values=c("#377eb8", "#4daf4a")) + 
  scale_fill_manual (values=c("#377eb8", "#4daf4a")) + geom_vline(xintercept = log10(1), linetype="dashed") +
  scale_x_continuous(breaks=seq(-5, 9, 4)) +
  labs (x="log10") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines")) + facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

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

#resize.win(18,6)
Cc_sum30.png <- ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=melted_NAdata.betas_comb_30m %>% filter (entity=="Cc"), aes(x = lon, y = lat, color = log10(value)))+ scale_color_viridis() +
  #scale_color_viridis(breaks = c(seq(-5, 9, 2))) + expand_limits(colour = c(seq(-5, 9, 2))) + 
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(),  legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + 
  facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))
ggsave(filename = "Cc_sum30.png", width = 18, height = 5, dpi = 600, device="png", units="in")

resize.win(14, 5)
Li_sum30.png <- ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=melted_NAdata.betas_comb_30m %>% filter (entity=="Li") %>% filter (!(step=="infcomb")),
             aes(x = lon, y = lat, color = log10(value)))+ scale_color_viridis() +
  #scale_color_viridis(breaks = c(seq(-5, 9, 2))) + expand_limits(colour = c(seq(-5, 9, 2))) + 
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(),  legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + 
  facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "Li_sum30.png", width = 14, height = 5, dpi = 600, device="png", units="in")


#make a plot like NA-VICE but density plot
#resize.win(10,5)
sum30.png <- ggplot (data=melted_NAdata.betas_comb_30m, aes(x=log10(value), fill=entity)) + 
  geom_histogram(binwidth=0.5, aes(y=..density..), alpha=0.5) +
  geom_density(position = "stack", alpha=0.5) + scale_color_manual (values=c("#377eb8", "#4daf4a")) +   scale_fill_manual (values=c("#377eb8", "#4daf4a")) + geom_vline(xintercept = log10(1), linetype="dashed") +
  scale_x_continuous(breaks=seq(-5, 9, 4)) +
  labs (x="log10") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines")) +facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "sum30.png", width = 17, height = 5, dpi = 600, device="png", units="in")

#plotly code, change file 1m and 30 m, also the steps but you could also run the summaries instead, just that resolution is harder
library (plotly)
ggplotly(ggplot (data=melted_NAdata.betas_comb_1m %>% filter (step=="infcomb"), aes(x=log10(value), fill=entity)) + 
           geom_density(position = "identity", alpha=0.5) )

ggplotly (sum1.png)
ggplotly (sum30.png)

wind.speed.sum <- summarySE (data=NAdata_merge %>% filter (!(max_wind_speed == "NA")), measurevar = "max_wind_speed", groupvars = c("depthf"))

ggplot(data=NAdata_merge, aes(x=max_wind_speed, fill=depthf)) + geom_histogram(bins=100, position="identity", alpha=0.5) +
  facet_grid(~depthf) +
  scale_y_log10()
  #geom_histogram(binwidth=0.5, aes(y=..density..), alpha=0.5) + geom_density(position = "stack", alpha=0.25)
