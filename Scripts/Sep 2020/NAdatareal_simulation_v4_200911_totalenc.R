#Note: this is the merged data set from Ali and Toby, where waterdepth<100m and abundance<10 were eliminated from the dataset. This is a melted dataset where entities (i.e., Cc and Li) are just in one column
NAdata_merge <- read.csv("D:/Users/karengb/Exported Tables/NAdata_merge_masked.csv")
source("inspack_map.R")

#density plots #change color schemes to colorblind friendly
resize.win (10, 5)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")

#get data out of each histogram, and make an excel file out of it (saved in onedrive: density raw.
#used density_merged, saved in CSV files)

#tried getting data directly from R
Cc.shelf <- ggplot (data=NAdata_merge %>% filter (depthf=="<200 m") %>% filter (entity=="Cc"), 
                    aes(x=log10(abundance),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)*10))  + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

Cc.shelf <- ggplot_build(Cc.shelf)$data[[1]] %>% select (x, y)
Cc.shelf$entity <- "calcified"
Cc.shelf$depthf <- "<200 m"

Cc.open <- ggplot (data=NAdata_merge %>% filter (depthf==">200 m") %>% filter (entity=="Cc"), 
                   aes(x=log10(abundance),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)*10))  + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

Cc.open <- ggplot_build(Cc.open)$data[[1]] %>% select (x, y)
Cc.open$entity <- "calcified"
Cc.open$depthf <- ">200 m"

Li.shelf <- ggplot (data=NAdata_merge %>% filter (depthf=="<200 m") %>% filter (entity=="Li"), 
                   aes(x=log10(abundance),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)*10))  + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

Li.shelf <- ggplot_build(Li.shelf)$data[[1]] %>% select (x, y)
Li.shelf$entity <- "lith"
Li.shelf$depthf <- "<200 m"

Li.open <- ggplot (data=NAdata_merge %>% filter (depthf==">200 m") %>% filter (entity=="Li"), 
             aes(x=log10(abundance),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)*10))  + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

Li.open <- ggplot_build(Li.open)$data[[1]] %>% select (x, y)
Li.open$entity <- "lith"
Li.open$depthf <- ">200 m"

density_merged <- rbind (Cc.shelf, Cc.open, Li.shelf, Li.open)


#save data
write.table (density_merged, "Exported Tables/density_merged.csv", sep=",", col.names=T, row.names=F)

#get data from csv file 
#density_merged <- read.csv("D:/Users/karengb/CSV Files/density_all_merged.csv")

density_merged$depthf <- factor (density_merged$depthf, levels = c("<200 m", ">200 m"), labels= c("shelf/slope", "open ocean"))

resize.win(8,5)

hist_abundance_ent.png <- ggplot (data=density_merged, aes(x=x,  y= y, color=depthf, fill=depthf)) + 
  geom_area(position = "identity", alpha=0.25) +
  geom_line(size=1) + 
  geom_vline(xintercept = 1, linetype="dashed") +
  scale_x_continuous(breaks = c(-1:6)) +
  scale_color_manual (values= c("#f1a340","#998ec3")) + scale_fill_manual (values= c("#f1a340","#998ec3")) +
  facet_grid(~entity) + theme_Publication2() + labs (x = expression(log[10]~"concentration"~mL^-1), y= "density probability") +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

ggsave(filename = "hist_abundance_ent.png", width = 10, height = 5, dpi = 600, device="png", units="in")

resize.win(10,5)
#not needed
hist_abundance_depth.png <-  ggplot (data=density_merged, aes(x=x,  y= y, color=entity, fill=entity)) + 
  geom_area(position = "identity", alpha=0.3) +
  geom_line(size=0.7) + 
  geom_vline(xintercept = 1, linetype="dashed") +
  scale_x_continuous(breaks = c(-1:6)) +
  facet_grid(~depthf) + theme_Publication2() + labs (x = expression(log[10]~"concentration"~mL^-1), y= "density probability") +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

#ggsave(filename = "hist_abundance_depth.png", width = 10, height = 5, dpi = 600, device="png", units="in")

#calling world
NAdata_merge$depthf <- factor (NAdata_merge$depthf, levels = c("<200 m", ">200 m"), labels= c("shelf/slope", "open ocean"))

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#color scheme
BlRed <- colorRampPalette((c('#00429d', '#2b57a7', '#426cb0', '#5681b9', '#6997c2', '#7daeca', '#93c4d2', '#abdad9', '#caefdf', '#ffe2ca', '#ffc4b4', '#ffa59e', '#f98689', '#ed6976', '#dd4c65', '#ca2f55', '#b11346', '#93003a')))


#ggplot version of abundance
resize.win(10,5)

NAdata_merge$entity <- factor (NAdata_merge$entity,labels= c("calcified", "lith"))

abundance.png <- ggplot(data = world)+ geom_sf(color="darkgray", fill="gainsboro") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+   geom_point(data=NAdata_merge %>%  filter(entitycode %in% c("Ncells_max", "Nliths_max")), 
             aes(x = lon, y = lat, color = log10(abundance)))+ 
  scale_colour_gradientn(colours = BlRed(100)) +    theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + facet_grid(~entity) + labs (x="", y="") 

ggsave(filename = "abundance.png", width = 10, height = 5, dpi = 600, device="png", units="in")

##wind speed
maxwind.png <- ggplot(data = world)+ geom_sf(color="darkgray", fill="gainsboro") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=NAdata_merge, aes(x = lon, y = lat, color = max_wind_speed)) +
  scale_colour_gradientn(colours = BlRed(100)) + 
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + 
  labs (title= expression("wind speed("~m~s^-1~")"))  + labs (x="", y="") 

ggsave(filename = "maxwind.png", width = 6, height = 5, dpi = 600, device="png", units="in")

#resize.win(6,5)
#disrate_upper1m
#make color legend same for 1m and 30m (main data is 30m)
#, limits = c(min(log10(NAdata_merge$disrate_30)), max (log10(NAdata_merge$disrate_1m)))) +   
disrate_1m.png <- ggplot(data = world)+ geom_sf(color="darkgray", fill="gainsboro") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=NAdata_merge %>% filter (entity=="calcified"), aes(x = lon, y = lat, color = log10(disrate_1m)))+
  scale_colour_gradientn(colours = BlRed(100)) +
theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + labs (title= expression(log[10]~epsilon~(m^2~s^-3))) + labs (x="", y="") 

ggsave(filename = "disrate_1m.png", width = 6, height = 5, dpi = 600, device="png", units="in")

#disrate_30m
disrate_30m.png <- ggplot(data = world)+ geom_sf(color="darkgray", fill="gainsboro") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=NAdata_merge, aes(x = lon, y = lat, color = log10(disrate_30m)))+
  scale_colour_gradientn(colours = BlRed(100)) +  
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + labs (title= expression(log[10]~epsilon~(m^2~s^-3))) + labs (x="", y="") 

ggsave(filename = "disrate_30m.png", width = 6, height = 5, dpi = 600, device="png", units="in")

##betas
betas_BM_DS <- read.csv("D:/Users/karengb/CSV Files/betas_BM_DS.txt") #radius are in m

#remove <10 cells
NAdata_merge_raw <- NAdata_merge
NAdata_merge <- NAdata_merge_raw %>% filter (!(abundance<10)) #removed all abundances of <10 for cells
NAdata_merge$entity <- factor (NAdata_merge$entity,labels= c("calcified", "lith"))

#calculate beta turb
NAdata.betas <- left_join(NAdata_merge, betas_BM_DS)
v= 1.099*(10)^-6 #m2/s kinematic viscosity in 18C
Rehv= 90*(10)^-9 #in m radius virus

#split 1m and 30m
NAdata.betas_1m = NAdata.betas %>% select(!(disrate_30m))

#1m
NAdata.betas_1m$beta_turb <- (4.2*pi*((NAdata.betas_1m$disrate_1m/(v))^0.5)*((NAdata.betas_1m$rad+Rehv)^3))*86400*10^6 

NAdata.betas_1m$beta_all <- NAdata.betas_1m$beta_BM + NAdata.betas_1m$beta_DS + NAdata.betas_1m$beta_turb

#probabilities
probs <- as.data.frame(list (entity = as.factor (rep(c("calcified","lith"), 2)), virus = rep(c("high", "low"), 1, each=8), propvir= rep(c(0.33, 0.67), 1, each=8), ads = rep(c(0.0095, 0.0134), 4), inf = rep(c(0.3, NA,0.3, NA, 0.3, NA, 0.3, NA, 0.06, NA, 0.06, NA, 0.06, NA, 0.06, NA), 1, each=1)))

NAdata.betas_1m$entity <- as.factor(NAdata.betas_1m$entity)

#join NAdata and probs
NAdata.betas_1m <- left_join(NAdata.betas_1m, probs)

#calculate propEhV
NAdata.betas_1m$propEhV <- NAdata.betas_1m$virnum* NAdata.betas_1m$propvir

#calculate encounters fast slow
NAdata.betas_1m$encounters <- NAdata.betas_1m$beta_all*NAdata.betas_1m$abundance #entity sense

#calculate total adsorption by virus props
NAdata.betas_1m$adstot <- NAdata.betas_1m$encounters*NAdata.betas_1m$ads

#calculate total successful infections
NAdata.betas_1m$sucinf <- NAdata.betas_1m$encounters*NAdata.betas_1m$ads*NAdata.betas_1m$inf

#add everything
high_NAdata.betas_1m <- NAdata.betas_1m %>% filter (virus=="high")
low_NAdata.betas_1m <- NAdata.betas_1m %>% filter (virus=="low")

NAdata.betas_comb_1m <- NAdata.betas_1m %>% filter (virus=="high") %>% select (c(lat, lon, entity, depthf, abundance, virnum, disrate_1m))
NAdata.betas_comb_1m$enccomb <- high_NAdata.betas_1m$encounters + low_NAdata.betas_1m$encounters
NAdata.betas_comb_1m$adscomb <- high_NAdata.betas_1m$adstot + low_NAdata.betas_1m$adstot
NAdata.betas_comb_1m$infcomb <- high_NAdata.betas_1m$sucinf + low_NAdata.betas_1m$sucinf
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
  `enccomb` = 'encounters~entity^{-1}~d^{-1}',
  `adscomb` = 'adsorptions~entity^{-1}~d^{-1}',
  `infcomb` = 'infections~entity^{-1}~d^{-1}'
)

#change breaks
all_1m.png<- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data= melted_NAdata.betas_comb_1m %>% filter (! ((entity=="lith") & (step=="infcomb"))), 
             aes(x = lon, y = lat, color = log10(value)))+
  scale_colour_gradientn(colours = BlRed(100), breaks = c(seq (-8, 7, 2)), limits=c(-8, 7))  + 
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(),  legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + facet_grid(entity~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed))) + labs (x="", y="") 

ggsave(filename = "all_1m.png", width = 18, height = 10, dpi = 600, device="png", units="in")

#make a plot like NA-VICE but density plot
resize.win(17,5)

Cc.abundance <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (entity=="calcified") %>% filter (step=="abundance"), 
                        aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Cc.abundance <- ggplot_build(Cc.abundance)$data[[1]] %>% select (x, y)
Cc.abundance$entity <- "calcified"
Cc.abundance$step <- "abundance"

Cc.enccomb <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (entity=="calcified") %>% filter (step=="enccomb"), 
                      aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Cc.enccomb <- ggplot_build(Cc.enccomb)$data[[1]] %>% select (x, y)
Cc.enccomb$entity <- "calcified"
Cc.enccomb$step <- "enccomb"

Cc.adscomb <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (entity=="calcified") %>% filter (step=="adscomb"), 
                      aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Cc.adscomb <- ggplot_build(Cc.adscomb)$data[[1]] %>% select (x, y)
Cc.adscomb$entity <- "calcified"
Cc.adscomb$step <- "adscomb"

Cc.infcomb <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (entity=="calcified") %>% filter (step=="infcomb"), 
                      aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Cc.infcomb <- ggplot_build(Cc.infcomb)$data[[1]] %>% select (x, y)
Cc.infcomb$entity <- "calcified"
Cc.infcomb$step <- "infcomb"

Li.abundance <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (entity=="lith") %>% filter (step=="abundance"), 
                        aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Li.abundance <- ggplot_build(Li.abundance)$data[[1]] %>% select (x, y)
Li.abundance$entity <- "lith"
Li.abundance$step <- "abundance"

Li.enccomb <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (entity=="lith") %>% filter (step=="enccomb"), 
                      aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Li.enccomb <- ggplot_build(Li.enccomb)$data[[1]] %>% select (x, y)
Li.enccomb$entity <- "lith"
Li.enccomb$step <- "enccomb"

Li.adscomb <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (entity=="lith") %>% filter (step=="adscomb"), 
                      aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Li.adscomb <- ggplot_build(Li.adscomb)$data[[1]] %>% select (x, y)
Li.adscomb$entity <- "lith"
Li.adscomb$step <- "adscomb"

sum_1m <- rbind (Cc.abundance, Cc.enccomb, Cc.adscomb, Cc.infcomb, Li.abundance, Li.enccomb, Li.adscomb)

sum_1m$step <-  reorder.factor (sum_1m$step, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 

sum1.png <- ggplot (data=sum_1m, aes(x=x, y=y, fill=entity, color=entity)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=0.7) + 
  scale_color_manual (values=c("#377eb8", "#4daf4a")) + 
  scale_fill_manual (values=c("#377eb8", "#4daf4a")) + geom_vline(xintercept = 1, linetype="dashed") +
  scale_x_continuous(breaks=seq(-5, 9, 4)) +
  labs (x="log10", y="density probability") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines")) + facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "sum1.png", width = 17, height = 5, dpi = 600, device="png", units="in")

###-----------------------------30m--------------------------##
#split 1m and 30m
NAdata.betas_30m = NAdata.betas %>% select(!(disrate_1m))

Cc.abundance.30m <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="calcified") %>% filter (step=="abundance"), 
                            aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Cc.abundance.30m <- ggplot_build(Cc.abundance.30m)$data[[1]] %>% select (x, y)
Cc.abundance.30m$entity <- "calcified"
Cc.abundance.30m$step <- "abundance"

Cc.enccomb.30m <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="calcified") %>% filter (step=="enccomb"), 
                          aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Cc.enccomb.30m <- ggplot_build(Cc.enccomb.30m)$data[[1]] %>% select (x, y)
Cc.enccomb.30m$entity <- "calcified"
Cc.enccomb.30m$step <- "enccomb"

Cc.adscomb.30m <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="calcified") %>% filter (step=="adscomb"), 
                          aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Cc.adscomb.30m <- ggplot_build(Cc.adscomb.30m)$data[[1]] %>% select (x, y)
Cc.adscomb.30m$entity <- "calcified"
Cc.adscomb.30m$step <- "adscomb"

Cc.infcomb.30m <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="calcified") %>% filter (step=="infcomb"), 
                          aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Cc.infcomb.30m <- ggplot_build(Cc.infcomb.30m)$data[[1]] %>% select (x, y)
Cc.infcomb.30m$entity <- "calcified"
Cc.infcomb.30m$step <- "infcomb"

Li.abundance.30m <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="lith") %>% filter (step=="abundance"), 
                            aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Li.abundance.30m <- ggplot_build(Li.abundance.30m)$data[[1]] %>% select (x, y)
Li.abundance.30m$entity <- "lith"
Li.abundance.30m$step <- "abundance"

Li.enccomb.30m <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="lith") %>% filter (step=="enccomb"), 
                          aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Li.enccomb.30m <- ggplot_build(Li.enccomb.30m)$data[[1]] %>% select (x, y)
Li.enccomb.30m$entity <- "lith"
Li.enccomb.30m$step <- "enccomb"

Li.adscomb.30m <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="lith") %>% filter (step=="adscomb"), 
                          aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Li.adscomb.30m <- ggplot_build(Li.adscomb.30m)$data[[1]] %>% select (x, y)
Li.adscomb.30m$entity <- "lith"
Li.adscomb.30m$step <- "adscomb"

sum_30m <- rbind (Cc.abundance.30m, Cc.enccomb.30m, Cc.adscomb.30m, Cc.infcomb.30m, Li.abundance.30m, Li.enccomb.30m, Li.adscomb.30m)

sum_30m$step <-  reorder.factor (sum_30m$step, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 

sum30.png <- ggplot (data=sum_30m, aes(x=x, y=y, fill=entity, color=entity)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) + 
  scale_color_manual (values=c("#377eb8", "#4daf4a")) + 
  scale_fill_manual (values=c("#377eb8", "#4daf4a")) + geom_vline(xintercept = 1, linetype="dashed") +
  scale_x_continuous(breaks=seq(-5, 9, 4)) +
  labs (x="log10", y="density probability") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines")) + facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

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


###navice slice
##filter NA-VICE sites
navice <- melted_NAdata.betas_comb_30m %>% filter (between (lat, 50, 70)) %>% filter (between (lon, -40, -20))

##all steps
navice.png<- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-40, -20), ylim = c(50, 70 ) , expand = FALSE) +
  geom_point(data= navice %>% filter (! ((entity=="Li") & (step=="infcomb"))), 
             aes(x = lon, y = lat, color = log10(value)))+
  scale_colour_gradientn(colours = BlRed(100), breaks = c(seq (-5,8, 2)), limits=c(-5, 8)) +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(),  legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + facet_grid(entity~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed))) + labs (x="", y="") 

ggsave(filename = "navice.png", width = 12, height = 10, dpi = 600, device="png", units="in")

#filter infection and Cc (panel A), navice slice
resize.win (5,5)
ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-40, -20), ylim = c(50, 70 ) , expand = FALSE) +
  geom_point(data= navice %>% filter (! (entity=="coccolith"))%>% filter (step=="infcomb"),  
             aes(x = lon, y = lat, color = log10(value)))+
  scale_colour_gradientn(colours = BlRed(100), breaks = c(seq (-5,8, 2)), limits=c(-5, 8)) +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(),  legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10))

##put coords on the navice slice
coords <- read.csv("D:/Users/karengb/CSV Files/coords.txt", sep=";")

resize.win(5,5)
ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-40, -20), ylim = c(50, 70 ) , expand = FALSE) +
  geom_point(data= navice %>% filter (! (entity=="coccolith"))%>% filter (step=="infcomb"),  
             aes(x = lon, y = lat, color = log10(value)))+
  scale_colour_gradientn(colours = BlRed(100), breaks = c(seq (-5,8, 2)), limits=c(-5, 8)) +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), 
                               legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), 
                               axis.title = element_blank(),  legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) +
  labs (x="", y="") + 
  geom_point(data=coords, aes(x=lon, y=lat), color="black", size=4)

