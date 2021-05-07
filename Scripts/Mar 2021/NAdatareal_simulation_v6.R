#Note: this is the merged data set from Ali and Toby, where waterdepth<100m and abundance<10 were eliminated from the dataset. This is a melted dataset where entities (i.e., Cc and Li) are just in one column
NAdata_merge <- read.csv("D:/Users/karengb/Exported Tables/NAdata_merge_masked_withNcandEhux.csv")

source("inspack_map.R")

#density plots #change color schemes to colorblind friendly
resize.win (10, 5)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("between", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("first", "dplyr")
conflict_prefer("last", "dplyr")


#just choose E. huxleyi and cells
#NAdata_merge <- NAdata_merge2 %>% filter (entitycode %in% c("Ncells_total", "Nliths_max"))
#NAdata_merge$entity <- factor (NAdata_merge$entitycode, levels = c("Ncells_total", "Nliths_max"), labels= c("E. huxleyi", "Coccoliths"))

#get data out of each histogram, and make an excel file out of it (saved in onedrive: density raw.
#used density_merged, saved in CSV files)

#tried getting data directly from R
Ehux.shelf <- ggplot (data=NAdata_merge %>% filter (depthf=="shelf/slope") %>% 
                        filter (entitycode=="Ncells_total"), 
                      aes(x=log10(abundance),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)*10))  + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

Ehux.shelf <- ggplot_build(Ehux.shelf)$data[[1]] %>% select (x, y)
Ehux.shelf$entity <- "E. huxleyi"
Ehux.shelf$depthf <- "<200 m"

Ehux.open <- ggplot (data=NAdata_merge %>% filter (depthf=="open ocean") %>% 
                       filter (entitycode=="Ncells_total"), 
                     aes(x=log10(abundance),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)*10))  + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

Ehux.open <- ggplot_build(Ehux.open)$data[[1]] %>% select (x, y)
Ehux.open$entity <- "E. huxleyi"
Ehux.open$depthf <- ">200 m"

Li.shelf <- ggplot (data=NAdata_merge %>% filter (depthf=="shelf/slope") %>% filter (entity=="lith"), 
                    aes(x=log10(abundance),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)*10))  + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

Li.shelf <- ggplot_build(Li.shelf)$data[[1]] %>% select (x, y)
Li.shelf$entity <- "lith"
Li.shelf$depthf <- "<200 m"

Li.open <- ggplot (data=NAdata_merge %>% filter (depthf=="open ocean") %>% filter (entity=="lith"), 
                   aes(x=log10(abundance),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)*10))  + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

Li.open <- ggplot_build(Li.open)$data[[1]] %>% select (x, y)
Li.open$entity <- "lith"
Li.open$depthf <- ">200 m"

density_merged <- rbind (Ehux.shelf, Ehux.open, Li.shelf, Li.open)

#save data
write.table (density_merged, "Exported Tables/density_merged_allcells.csv", sep=",", col.names=T, row.names=F)

density_merged$depthf <- factor (density_merged$depthf, levels = c("<200 m", ">200 m"), labels= c("shelf/slope", "open ocean"))

resize.win(10, 5)

hist_abundance_ent.png <- ggplot (data=density_merged, aes(x=x,  y= y, color=depthf, fill=depthf)) + 
  geom_area(position = "identity", alpha=0.25) +
  geom_line(size=1) + 
  geom_vline(xintercept = 1, linetype="dashed") +
  scale_x_continuous(breaks = c(-1:6)) +
  scale_color_manual (values= c("#f1a340","#998ec3")) + scale_fill_manual (values= c("#f1a340","#998ec3")) +
  facet_grid(~entity) + theme_Publication2() + labs (x = expression(log[10]~"concentration"~mL^-1), y= "probability density") +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

ggsave(filename = "hist_abundance_ent.png", width = 10, height = 5, dpi = 600, device="png", units="in")

resize.win(10,5)

##density_merged (for discussion)
density_merged$xexp <- (10)^(density_merged$x)
dens.freq <- density_merged %>% group_by (depthf, entity) %>% filter (y>0.039)  %>% 
  mutate (xexp=(10)^x, percent=(10^(x)*100))

density_merged %>% group_by (depthf, entity ) %>% filter (y>0.04) %>%  summarise (freq=sum(y*100))


#calling world
NAdata_merge$depthf <- factor (NAdata_merge$depthf, levels = c("<200 m", ">200 m"), labels= c("shelf/slope", "open ocean"))

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#color scheme
BlRed <- colorRampPalette((c('#00429d', '#2b57a7', '#426cb0', '#5681b9', '#6997c2', '#7daeca', '#93c4d2', '#abdad9', '#caefdf', '#ffe2ca', '#ffc4b4', '#ffa59e', '#f98689', '#ed6976', '#dd4c65', '#ca2f55', '#b11346', '#93003a')))

#ggplot version of abundance
resize.win(10,5)

abundance.png <- ggplot(data = world)+ geom_sf(color="darkgray", fill="gainsboro") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+   geom_point(data=NAdata_merge %>%  filter(entitycode %in% c("Ncells_total", "Nliths_max")), aes(x = lon, y = lat, color = log10(abundance)))+ 
  scale_colour_gradientn(colours = BlRed(100)) +  
  scale_x_continuous(breaks=seq(-70, 10, 20)) +
  scale_y_continuous(breaks=seq(30, 75, 20)) +
  theme_Publication2() +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) + 
  labs (color= expression(log[10]~"concentration"~mL^-1), x="", y="") +
  guides(color = guide_colorbar(title.position = "right")) + facet_grid (~entity)

ggsave(filename = "abundance.png", width = 12, height = 6, dpi = 600, device="png", units="in")

write.table (NAdata_merge, "Exported Tables/density_merged_allcells.csv", sep=",", col.names=T, row.names=F)


##wind speed
maxwind.png <- ggplot(data = world)+ geom_sf(color="darkgray", fill="gainsboro") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=NAdata_merge, aes(x = lon, y = lat, color = max_wind_speed)) +
  scale_colour_gradientn(colours = BlRed(100), breaks = c(seq (10, 50, 10)), limits=c(10, 50)) + 
  theme_Publication2() + 
  scale_x_continuous(breaks=seq(-70, 10, 20)) +
  scale_y_continuous(breaks=seq(30, 75, 20)) +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) + 
  labs (color= expression("wind speed ("~m~s^-1~")"), x="", y="") +
  guides(color = guide_colorbar(title.position = "right"))

ggsave(filename = "maxwind.png", width = 8, height = 6, dpi = 600, device="png", units="in")

#resize.win(6,5)
#disrate_upper1m
#make color legend same for 1m and 30m (main data is 30m)
#, limits = c(min(log10(NAdata_merge$disrate_30)), max (log10(NAdata_merge$disrate_1m)))) +   
disrate_1m.png <- ggplot(data = world)+ geom_sf(color="darkgray", fill="gainsboro") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=NAdata_merge %>% filter (entity=="calcified"), aes(x = lon, y = lat, color = log10(disrate_1m)))+
  scale_colour_gradientn(colours = BlRed(100), breaks = c(seq (-8, -3, 2)), limits=c(-8, -3)) + theme_Publication2() + 
  scale_x_continuous(breaks=seq(-70, 10, 20)) +
  scale_y_continuous(breaks=seq(30, 75, 20)) +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10), plot.title = element_blank()) +  
  labs (color= expression(log[10]~epsilon~(m^2~s^-3)), x="", y="") +
  guides(color = guide_colorbar(title.position = "right"))  

ggsave(filename = "disrate_1m.png", width = 8, height = 6, dpi = 600, device="png", units="in")

#disrate_30m
disrate_30m.png <- ggplot(data = world)+ geom_sf(color="darkgray", fill="gainsboro") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=NAdata_merge, aes(x = lon, y = lat, color = log10(disrate_30m)))+
  scale_colour_gradientn(colours = BlRed(100), breaks = c(seq (-8, -3, 2)), limits=c(-8, -3)) + theme_Publication2() + 
  theme_Publication2() + 
  scale_x_continuous(breaks=seq(-70, 10, 20)) +
  scale_y_continuous(breaks=seq(30, 75, 20)) +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10), plot.title = element_blank()) +  
  labs (color= expression(log[10]~epsilon~(m^2~s^-3)), x="", y="") +
  guides(color = guide_colorbar(title.position = "right"))  


ggsave(filename = "disrate_30m.png", width = 8, height = 6, dpi = 600, device="png", units="in")

##betas
library(readr)
betas_BM_DS <- read_csv("CSV Files/betas_BM_DS_withNC.txt") #radius are in m

#calculate beta turb
NAdata.betas <- left_join(NAdata_merge, betas_BM_DS)
v= 1.099*(10)^-6 #m2/s kinematic viscosity in 18C
Rehv= 90*(10)^-9 #in m radius virus

#split 1m and 30m
NAdata.betas_1m = NAdata.betas %>% select(!(disrate_30m)) %>% filter (!(entitycode=="Ncells_total"))

#1m
NAdata.betas_1m$beta_turb <- (4.2*pi*((NAdata.betas_1m$disrate_1m/(v))^0.5)*((NAdata.betas_1m$rad+Rehv)^3))*86400*10^6 

NAdata.betas_1m$beta_all <- NAdata.betas_1m$beta_BM + NAdata.betas_1m$beta_DS + NAdata.betas_1m$beta_turb

#probabilities
probs <- as.data.frame(list (entity = as.factor (rep(c("calcified","lith", "naked"), 2)), virus = rep(c("high", "low"), 1, each=3),propvir= rep(c(0.33, 0.67), 1, each=3), ads = rep(c(0.20, 0.15, 0.72), 2), inf = rep(c(0.3, NA, 0.3,  0.06, NA, 0.06))))

NAdata.betas_1m$entity <- as.factor(NAdata.betas_1m$entity)

#join NAdata and probs
NAdata.betas_1m <- left_join(NAdata.betas_1m, probs)

#calculate propEhV
NAdata.betas_1m$propEhV <- NAdata.betas_1m$virnum* NAdata.betas_1m$propvir

#calculate encounters fast slow
NAdata.betas_1m$encounters <- NAdata.betas_1m$beta_all*NAdata.betas_1m$propEhV #virus sense

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
NAdata.betas_comb_1m$perencounters <- NAdata.betas_comb_1m$enccomb*100
NAdata.betas_comb_1m$peradsorbed <- NAdata.betas_comb_1m$adscomb*100
NAdata.betas_comb_1m$perinf <- NAdata.betas_comb_1m$infcomb*100
#NAdata.betas_comb_1m <- NAdata.betas_comb_1m %>% mutate(perencounters= if_else(perencounters > 1, 1, perencounters)) %>% mutate(peradsorbed= if_else(perencounters > 1, 1, peradsorbed)) %>% mutate(perinf= if_else(perinf > 1, 1, perinf)) 

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
  geom_point(data= melted_NAdata.betas_comb_1m %>% filter (! (entity=="lith")),  
             aes(x = lon, y = lat, color = log10(value)))+
  scale_colour_gradientn(colours = BlRed(100))  + 
  scale_x_continuous(breaks=seq(-70, 10, 20)) +
  scale_y_continuous(breaks=seq(30, 75, 20)) +
  theme_Publication2() +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) + 
  labs (color= expression(log[10]), x="", y="") +
  guides(color = guide_colorbar(title.position = "right")) +  facet_grid(entity~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "all_1m.png", width = 20, height = 10, dpi = 600, device="png", units="in")

#make a plot like NA-VICE but density plot
#####calcified and naked

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

Nc.abundance <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (entity=="naked") %>% filter (step=="abundance"), 
                        aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Nc.abundance <- ggplot_build(Nc.abundance)$data[[1]] %>% select (x, y)
Nc.abundance$entity <- "naked"
Nc.abundance$step <- "abundance"

Nc.enccomb <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (entity=="naked") %>% filter (step=="enccomb"), 
                      aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Nc.enccomb <- ggplot_build(Nc.enccomb)$data[[1]] %>% select (x, y)
Nc.enccomb$entity <- "naked"
Nc.enccomb$step <- "enccomb"

Nc.adscomb <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (entity=="naked") %>% filter (step=="adscomb"), 
                      aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Nc.adscomb <- ggplot_build(Nc.adscomb)$data[[1]] %>% select (x, y)
Nc.adscomb$entity <- "naked"
Nc.adscomb$step <- "adscomb"

Nc.infcomb <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (entity=="naked") %>% filter (step=="infcomb"), 
                      aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Nc.infcomb <- ggplot_build(Nc.infcomb)$data[[1]] %>% select (x, y)
Nc.infcomb$entity <- "naked"
Nc.infcomb$step <- "infcomb"

sum_1m <- rbind (Cc.abundance, Cc.enccomb, Cc.adscomb, Cc.infcomb, Nc.abundance, Nc.enccomb, Nc.adscomb, Nc.infcomb)

sum_1m$step <-  reorder.factor (sum_1m$step, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 

sum1.png <- ggplot (data=sum_1m, aes(x=x, y=y, fill=entity, color=entity)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) + 
  scale_color_manual (values=c("#377eb8", "#e41a1c")) + 
  scale_fill_manual (values=c("#377eb8", "#e41a1c")) + geom_vline(xintercept = 0, linetype="dashed") +
  labs (x=expression(log[10]), y="probability density") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines")) + 
  facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "sum1.png", width = 17, height = 5, dpi = 600, device="png", units="in")

sum_1m$xexp <- 10^(sum_1m$x)

#split 30m and 1n
NAdata.betas_30m = NAdata.betas %>% select(!(disrate_1m)) %>% filter (!(entitycode=="Ncells_total"))

#30m
NAdata.betas_30m$beta_turb <- (4.2*pi*((NAdata.betas_30m$disrate_30m/(v))^0.5)*((NAdata.betas_30m$rad+Rehv)^3))*86400*10^6 

NAdata.betas_30m$beta_all <- NAdata.betas_30m$beta_BM + NAdata.betas_30m$beta_DS + NAdata.betas_30m$beta_turb

NAdata.betas_30m$entity <- as.factor(NAdata.betas_30m$entity)

#join NAdata and probs
NAdata.betas_30m <- left_join(NAdata.betas_30m, probs)

#calculate propEhV
NAdata.betas_30m$propEhV <- NAdata.betas_30m$virnum* NAdata.betas_30m$propvir

#calculate encounters fast slow
NAdata.betas_30m$encounters <- NAdata.betas_30m$beta_all*NAdata.betas_30m$propEhV #virus sense

#calculate total adsorption by virus props
NAdata.betas_30m$adstot <- NAdata.betas_30m$encounters*NAdata.betas_30m$ads

#calculate total successful infections
NAdata.betas_30m$sucinf <- NAdata.betas_30m$encounters*NAdata.betas_30m$ads*NAdata.betas_30m$inf

#add everything
high_NAdata.betas_30m <- NAdata.betas_30m %>% filter (virus=="high")
low_NAdata.betas_30m <- NAdata.betas_30m %>% filter (virus=="low")

NAdata.betas_comb_30m <- NAdata.betas_30m %>% filter (virus=="high") %>% select (c(lat, lon, entity, depthf, abundance, virnum, disrate_30m))
NAdata.betas_comb_30m$enccomb30 <- high_NAdata.betas_30m$encounters + low_NAdata.betas_30m$encounters
NAdata.betas_comb_30m$adscomb30 <- high_NAdata.betas_30m$adstot + low_NAdata.betas_30m$adstot
NAdata.betas_comb_30m$infcomb30 <- high_NAdata.betas_30m$sucinf + low_NAdata.betas_30m$sucinf
#NAdata.betas_comb <- NAdata.betas_comb %>% mutate(adscomb302= if_else(adscomb30 > enccomb30, adscomb30, adscomb30))

##percentage of parameters
NAdata.betas_comb_30m$perencounters <- NAdata.betas_comb_30m$enccomb*100
NAdata.betas_comb_30m$peradsorbed <- (NAdata.betas_comb_30m$adscomb)*100
NAdata.betas_comb_30m$perinf <- (NAdata.betas_comb_30m$infcomb)*100
#NAdata.betas_comb_30m <- NAdata.betas_comb_30m %>% mutate(perencounters= if_else(perencounters > 1, 1, perencounters)) %>% mutate(peradsorbed= if_else(perencounters > 1, 1, peradsorbed)) %>% mutate(perinf= if_else(perinf > 1, 1, perinf)) 

##analysis
NAdata.betas_comb_30m$absenc <- NAdata.betas_comb_30m$perencounters*NAdata.betas_comb_30m$abundance
NAdata.betas_comb_30m$absads <- NAdata.betas_comb_30m$peradsorbed*NAdata.betas_comb_30m$abundance
NAdata.betas_comb_1m$absenc <- NAdata.betas_comb_1m$perencounters*NAdata.betas_comb_1m$abundance
NAdata.betas_comb_1m$absads <- NAdata.betas_comb_1m$peradsorbed*NAdata.betas_comb_1m$abundance


#melt dataset
melted_NAdata.betas_comb_30m <- reshape2::melt (NAdata.betas_comb_30m %>% select (lat, lon, entity, depthf, abundance, enccomb30, adscomb30, infcomb30), id.vars = c("lat", "lon", "depthf", "entity"), value.name = "value", variable.name = "step")

melted_NAdata.betas_comb_30m$step <-  reorder.factor (melted_NAdata.betas_comb_30m$step, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 

variable_labs <- c(
  `abundance` = 'concentration~(mL^{-1})',
  `enccomb` = 'encounters~entity^{-1}~d^{-1}',
  `adscomb` = 'adsorptions~entity^{-1}~d^{-1}',
  `infcomb` = 'infections~entity^{-1}~d^{-1}'
)

#change breaks
all_30m.png<- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data= melted_NAdata.betas_comb_30m %>% filter (! (entity=="lith")),  
             aes(x = lon, y = lat, color = log10(value)))+
  scale_colour_gradientn(colours = BlRed(100))  + 
  scale_x_continuous(breaks=seq(-70, 10, 20)) +
  scale_y_continuous(breaks=seq(30, 75, 20)) +
  theme_Publication2() +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) + 
  labs (color= expression(log[10]), x="", y="") +
  guides(color = guide_colorbar(title.position = "right")) +  facet_grid(entity~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "all_30m.png", width = 20, height = 10, dpi = 600, device="png", units="in")

#make a plot like NA-VICE but density plot
#####calcified and naked

resize.win(17,5)

Cc.abundance30 <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="calcified") %>% filter (step=="abundance"), 
                          aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Cc.abundance30 <- ggplot_build(Cc.abundance30)$data[[1]] %>% select (x, y)
Cc.abundance30$entity <- "calcified"
Cc.abundance30$step <- "abundance"

Cc.enccomb30 <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="calcified") %>% filter (step=="enccomb"), 
                        aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Cc.enccomb30 <- ggplot_build(Cc.enccomb30)$data[[1]] %>% select (x, y)
Cc.enccomb30$entity <- "calcified"
Cc.enccomb30$step <- "enccomb"

Cc.adscomb30 <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="calcified") %>% filter (step=="adscomb"), 
                        aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Cc.adscomb30 <- ggplot_build(Cc.adscomb30)$data[[1]] %>% select (x, y)
Cc.adscomb30$entity <- "calcified"
Cc.adscomb30$step <- "adscomb"

Cc.infcomb30 <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="calcified") %>% filter (step=="infcomb"), 
                        aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Cc.infcomb30 <- ggplot_build(Cc.infcomb30)$data[[1]] %>% select (x, y)
Cc.infcomb30$entity <- "calcified"
Cc.infcomb30$step <- "infcomb"

Nc.abundance30 <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="naked") %>% filter (step=="abundance"), 
                          aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Nc.abundance30 <- ggplot_build(Nc.abundance30)$data[[1]] %>% select (x, y)
Nc.abundance30$entity <- "naked"
Nc.abundance30$step <- "abundance"

Nc.enccomb30 <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="naked") %>% filter (step=="enccomb"), 
                        aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Nc.enccomb30 <- ggplot_build(Nc.enccomb30)$data[[1]] %>% select (x, y)
Nc.enccomb30$entity <- "naked"
Nc.enccomb30$step <- "enccomb"

Nc.adscomb30 <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="naked") %>% filter (step=="adscomb"), 
                        aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Nc.adscomb30 <- ggplot_build(Nc.adscomb30)$data[[1]] %>% select (x, y)
Nc.adscomb30$entity <- "naked"
Nc.adscomb30$step <- "adscomb"

Nc.infcomb30 <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="naked") %>% filter (step=="infcomb"), 
                        aes(x=log10(value))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Nc.infcomb30 <- ggplot_build(Nc.infcomb30)$data[[1]] %>% select (x, y)
Nc.infcomb30$entity <- "naked"
Nc.infcomb30$step <- "infcomb"

sum_30m <- rbind (Cc.abundance30, Cc.enccomb30, Cc.adscomb30, Cc.infcomb30, Nc.abundance30, Nc.enccomb30, Nc.adscomb30, Nc.infcomb30)

sum_30m$step <-  reorder.factor (sum_30m$step, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 

sum30.png <- ggplot (data=sum_30m, aes(x=x, y=y, fill=entity, color=entity)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) + 
  scale_color_manual (values=c("#377eb8", "#e41a1c")) + 
  scale_fill_manual (values=c("#377eb8", "#e41a1c")) + geom_vline(xintercept = 0, linetype="dashed") +
  labs (x=expression(log[10]), y="probability density") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines")) + facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "sum30.png", width = 17, height = 5, dpi = 600, device="png", units="in")

sum_30m$xexp <- 10^(sum_30m$x)

##try to combine naked and calcified

#add everything
calc_NAdata.betas_1m <- melted_NAdata.betas_comb_1m %>% filter (entity=="calcified")
calc_NAdata.betas_1m$calc.value <- calc_NAdata.betas_1m$value
calc_NAdata.betas_1m <- calc_NAdata.betas_1m %>% select (! (c(value, entity)))
naked_NAdata.betas_1m <- melted_NAdata.betas_comb_1m %>% filter (entity=="naked")
naked_NAdata.betas_1m$naked.value <- naked_NAdata.betas_1m$value*0.1
naked_NAdata.betas_1m <- naked_NAdata.betas_1m %>% select (! (c(value, entity)))

NAdata.betas_comb_1m_cn <- left_join(calc_NAdata.betas_1m, naked_NAdata.betas_1m)
#change NA to 0
NAdata.betas_comb_1m_cn[is.na(NAdata.betas_comb_1m_cn)]<- 0
NAdata.betas_comb_1m_cn$combvalue <- NAdata.betas_comb_1m_cn$calc.value + NAdata.betas_comb_1m_cn$naked.value

resize.win (17, 5)
comb_1m.png<- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data= NAdata.betas_comb_1m_cn, 
             aes(x = lon, y = lat, color = log10(combvalue)))+
  scale_colour_gradientn(colours = BlRed(100))  + 
  scale_x_continuous(breaks=seq(-70, 10, 20)) +
  scale_y_continuous(breaks=seq(30, 75, 20)) +
  theme_Publication2() +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) + 
  labs (color= expression(log[10]), x="", y="") +
  guides(color = guide_colorbar(title.position = "right")) +  facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "comb_1m.png", width = 20, height = 5, dpi = 600, device="png", units="in")

Ehux.abundance <- ggplot (data=NAdata.betas_comb_1m_cn %>% filter (step=="abundance"), 
                          aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.abundance <- ggplot_build(Ehux.abundance)$data[[1]] %>% select (x, y)
Ehux.abundance$entity <- "E. huxleyi"
Ehux.abundance$step <- "abundance"

Ehux.enccomb <- ggplot (data=NAdata.betas_comb_1m_cn %>% filter (step=="enccomb"), 
                        aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.enccomb <- ggplot_build(Ehux.enccomb)$data[[1]] %>% select (x, y)
Ehux.enccomb$entity <- "E. huxleyi"
Ehux.enccomb$step <- "enccomb"

Ehux.adscomb <- ggplot (data=NAdata.betas_comb_1m_cn  %>% filter (step=="adscomb"), 
                        aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.adscomb <- ggplot_build(Ehux.adscomb)$data[[1]] %>% select (x, y)
Ehux.adscomb$entity <- "E. huxleyi"
Ehux.adscomb$step <- "adscomb"

Ehux.infcomb <- ggplot (data=NAdata.betas_comb_1m_cn %>% filter (step=="infcomb"), 
                        aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Ehux.infcomb <- ggplot_build(Ehux.infcomb)$data[[1]] %>% select (x, y)
Ehux.infcomb$entity <- "E. huxleyi"
Ehux.infcomb$step <- "infcomb"

Ehux_1m <- rbind (Ehux.abundance, Ehux.enccomb, Ehux.adscomb, Ehux.infcomb)

Ehux_1m$step <-  reorder.factor (Ehux_1m$step, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 
Ehux1.png <- ggplot (data=Ehux_1m, aes(x=x, y=y, fill=entity, color=entity)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) + 
  scale_color_manual (values=c("#c51b8a")) + 
  scale_fill_manual (values=c("#c51b8a")) + geom_vline(xintercept = 0, linetype="dashed") +
  labs (x=expression(log[10]), y="probability density") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), legend.position = "none") + 
  facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

Ehux_1m$xexp <- 10^ (Ehux_1m$x)

ggsave(filename = "Ehux1.png", width = 17, height = 5, dpi = 600, device="png", units="in")

#add everything
calc_NAdata.betas_30m <- melted_NAdata.betas_comb_30m %>% filter (entity=="calcified")
calc_NAdata.betas_30m$calc.value <- calc_NAdata.betas_30m$value
calc_NAdata.betas_30m <- calc_NAdata.betas_30m %>% select (! (c(value, entity)))
naked_NAdata.betas_30m <- melted_NAdata.betas_comb_30m %>% filter (entity=="naked")
naked_NAdata.betas_30m$naked.value <- naked_NAdata.betas_30m$value*0.1
naked_NAdata.betas_30m <- naked_NAdata.betas_30m %>% select (! (c(value, entity)))

NAdata.betas_comb_30m_cn <- left_join(calc_NAdata.betas_30m, naked_NAdata.betas_30m)
#change NA to 0
NAdata.betas_comb_30m_cn[is.na(NAdata.betas_comb_30m_cn)]<- 0
NAdata.betas_comb_30m_cn$combvalue <- NAdata.betas_comb_30m_cn$calc.value + NAdata.betas_comb_30m_cn$naked.value

comb_30m.png<- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data= NAdata.betas_comb_30m_cn, 
             aes(x = lon, y = lat, color = log10(combvalue)))+
  scale_colour_gradientn(colours = BlRed(100))  + 
  scale_x_continuous(breaks=seq(-70, 10, 20)) +
  scale_y_continuous(breaks=seq(30, 75, 20)) +
  theme_Publication2() +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) + 
  labs (color= expression(log[10]), x="", y="") +
  guides(color = guide_colorbar(title.position = "right")) +  facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "comb_30m.png", width = 20, height = 5, dpi = 600, device="png", units="in")

Ehux.abundance30 <- ggplot (data=NAdata.betas_comb_30m_cn %>% filter (step=="abundance"), 
                            aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity")
Ehux.abundance30 <- ggplot_build(Ehux.abundance30)$data[[1]] %>% select (x,y)
Ehux.abundance30$entity <- "E. huxleyi"
Ehux.abundance30$step <- "abundance"

Ehux.enccomb30 <- ggplot (data=NAdata.betas_comb_30m_cn %>% filter (step=="enccomb30"), 
                          aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.enccomb30 <- ggplot_build(Ehux.enccomb30)$data[[1]] %>% select (x, y)
Ehux.enccomb30$entity <- "E. huxleyi"
Ehux.enccomb30$step <- "enccomb"

Ehux.adscomb30 <- ggplot (data=NAdata.betas_comb_30m_cn  %>% filter (step=="adscomb30"), 
                          aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.adscomb30 <- ggplot_build(Ehux.adscomb30)$data[[1]] %>% select (x, y)
Ehux.adscomb30$entity <- "E. huxleyi"
Ehux.adscomb30$step <- "adscomb"

Ehux.infcomb30 <- ggplot (data=NAdata.betas_comb_30m_cn %>% filter (step=="infcomb30"), 
                          aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Ehux.infcomb30 <- ggplot_build(Ehux.infcomb30)$data[[1]] %>% select (x, y)
Ehux.infcomb30$entity <- "E. huxleyi"
Ehux.infcomb30$step <- "infcomb"

Ehux_30m <- rbind (Ehux.abundance30, Ehux.enccomb30, Ehux.adscomb30, Ehux.infcomb30)

Ehux_30m$step <-  reorder.factor (Ehux_30m$step, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 
Ehux30.png <- ggplot (data=Ehux_30m, aes(x=x, y=y, fill=entity, color=entity)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) + 
  scale_color_manual (values=c("#c51b8a")) + 
  scale_fill_manual (values=c("#c51b8a")) + geom_vline(xintercept = 0, linetype="dashed") +
  labs (x=expression(log[10]), y="probability density") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), legend.position = "none") + 
  facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "Ehux30.png", width = 17, height = 5, dpi = 600, device="png", units="in")

Ehux_30m$xexp <- 10^ (Ehux_30m$x)

##liths
#1m
lith_1m.png<- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data= melted_NAdata.betas_comb_1m %>% filter (entity=="lith") %>% filter (!(step=="infcomb")),               aes(x = lon, y = lat, color = log10(value)))+
  scale_colour_gradientn(colours = BlRed(100), breaks = c(seq (-7, 7, 2)), limits=c(-7, 7))   +
  scale_x_continuous(breaks=seq(-70, 10, 20)) +
  scale_y_continuous(breaks=seq(30, 75, 20)) +
  theme_Publication2() +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) + 
  labs (color= expression(log[10]), x="", y="") +
  guides(color = guide_colorbar(title.position = "right")) +  facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "lith_1m.png", width = 15, height = 5, dpi = 600, device="png", units="in")

lith_30m.png<- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data= melted_NAdata.betas_comb_30m %>% filter (entity=="lith") %>% filter (!(step=="infcomb")),  
             aes(x = lon, y = lat, color = log10(value)))+
  scale_colour_gradientn(colours = BlRed(100), breaks = c(seq (-7, 7, 2)), limits=c(-7, 7)) +
  scale_x_continuous(breaks=seq(-70, 10, 20)) +
  scale_y_continuous(breaks=seq(30, 75, 20)) +
  theme_Publication2() +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) + 
  labs (color= expression(log[10]), x="", y="") +
  guides(color = guide_colorbar(title.position = "right")) +  facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "lith_30m.png", width = 15, height = 5, dpi = 600, device="png", units="in")

##density probability plots

#1m
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

lith_1m <- rbind (Li.abundance, Li.enccomb, Li.adscomb)

lith_1m$step <-  reorder.factor (lith_1m$step, new.order = c("abundance", 'enccomb', "adscomb")) 

lith1.png <- ggplot (data=lith_1m, aes(x=x, y=y, fill=entity, color=entity)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=0.7) + 
  scale_color_manual (values=c("#4daf4a")) + 
  scale_fill_manual (values=c("#4daf4a")) + geom_vline(xintercept = 0, linetype="dashed") +
  scale_x_continuous(breaks=seq(-7, 7, 2)) +
  labs (x=expression(log[10]), y="density probability") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), legend.position = "none") + 
  facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "lith1.png", width = 13, height = 5, dpi = 600, device="png", units="in")

lith_1m$xexp <- 10^(lith_1m$x)


#30 m
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

lith_30m <- rbind (Li.abundance.30m, Li.enccomb.30m, Li.adscomb.30m)

lith_30m$step <-  reorder.factor (lith_30m$step, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 
lith30.png <- ggplot (data=lith_30m, aes(x=x, y=y, fill=entity, color=entity)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) + 
  scale_color_manual (values=c("#4daf4a")) + 
  scale_fill_manual (values=c( "#4daf4a")) + geom_vline(xintercept = 0, linetype="dashed") +
  scale_x_continuous(breaks=seq(-7, 7, 2)) +
  labs (x=expression (log[10]), y="density probability") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), legend.position = "none") + 
  facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "lith30.png", width = 13, height = 5, dpi = 600, device="png", units="in")

lith_30m$xexp <- 10^(lith_30m$x)

##-------------------------------another iteration, split shelf/slope and open ocean---------------------------###

##density plots
#shelf
Ehux.abundance30.shelf <- ggplot (data=NAdata.betas_comb_30m_cn %>% filter (step=="abundance") %>% filter (depthf=="<200 m"), 
                                  aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity")
Ehux.abundance30.shelf <- ggplot_build(Ehux.abundance30.shelf)$data[[1]] %>% select (x,y)
Ehux.abundance30.shelf$entity <- "E. huxleyi"
Ehux.abundance30.shelf$step <- "abundance"
Ehux.abundance30.shelf$depthf <- "shelf/slope"

Ehux.enccomb30.shelf <- ggplot (data=NAdata.betas_comb_30m_cn %>% filter (step=="enccomb30") %>% filter (depthf=="<200 m"),  
                                aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.enccomb30.shelf <- ggplot_build(Ehux.enccomb30.shelf)$data[[1]] %>% select (x,y)
Ehux.enccomb30.shelf$entity <- "E. huxleyi"
Ehux.enccomb30.shelf$step <- "enccomb"
Ehux.enccomb30.shelf$depthf <- "shelf/slope"


Ehux.adscomb30.shelf <- ggplot (data=NAdata.betas_comb_30m_cn  %>% filter (step=="adscomb30") %>% filter (depthf=="<200 m"), 
                                aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.adscomb30.shelf <- ggplot_build(Ehux.adscomb30.shelf)$data[[1]] %>% select (x, y)
Ehux.adscomb30.shelf$entity <- "E. huxleyi"
Ehux.adscomb30.shelf$step <- "adscomb"
Ehux.adscomb30.shelf$depthf <- "shelf/slope"

Ehux.infcomb30.shelf <- ggplot (data=NAdata.betas_comb_30m_cn %>% filter (step=="infcomb30") %>% filter (depthf=="<200 m"),
                                aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Ehux.infcomb30.shelf <- ggplot_build(Ehux.infcomb30.shelf)$data[[1]] %>% select (x, y)
Ehux.infcomb30.shelf$entity <- "E. huxleyi"
Ehux.infcomb30.shelf$step <- "infcomb"
Ehux.infcomb30.shelf$depthf <- "shelf/slope"

#open ocean
Ehux.abundance30.open <- ggplot (data=NAdata.betas_comb_30m_cn %>% filter (step=="abundance") %>% filter (depthf==">200 m"), 
                                 aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity")
Ehux.abundance30.open <- ggplot_build(Ehux.abundance30.open)$data[[1]] %>% select (x, y)
Ehux.abundance30.open$entity <- "E. huxleyi"
Ehux.abundance30.open$step <- "abundance"
Ehux.abundance30.open$depthf <- "open ocean"

Ehux.enccomb30.open <- ggplot (data=NAdata.betas_comb_30m_cn %>% filter (step=="enccomb30") %>% filter (depthf==">200 m"),  
                               aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.enccomb30.open <- ggplot_build(Ehux.enccomb30.open)$data[[1]] %>% select (x,y)
Ehux.enccomb30.open$entity <- "E. huxleyi"
Ehux.enccomb30.open$step <- "enccomb"
Ehux.enccomb30.open$depthf <- "open ocean"


Ehux.adscomb30.open <- ggplot (data=NAdata.betas_comb_30m_cn  %>% filter (step=="adscomb30") %>% filter (depthf==">200 m"), 
                               aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.adscomb30.open <- ggplot_build(Ehux.adscomb30.open)$data[[1]] %>% select (x, y)
Ehux.adscomb30.open$entity <- "E. huxleyi"
Ehux.adscomb30.open$step <- "adscomb"
Ehux.adscomb30.open$depthf <- "open ocean"

Ehux.infcomb30.open <- ggplot (data=NAdata.betas_comb_30m_cn %>% filter (step=="infcomb30") %>% filter (depthf==">200 m"),
                               aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Ehux.infcomb30.open <- ggplot_build(Ehux.infcomb30.open)$data[[1]] %>% select (x, y)
Ehux.infcomb30.open$entity <- "E. huxleyi"
Ehux.infcomb30.open$step <- "infcomb"
Ehux.infcomb30.open$depthf <- "open ocean"

Ehux_30m_sep <- rbind (Ehux.abundance30.shelf, Ehux.enccomb30.shelf, Ehux.adscomb30.shelf, Ehux.infcomb30.shelf, 
                       Ehux.abundance30.open, Ehux.enccomb30.open, Ehux.adscomb30.open, Ehux.infcomb30.open)

Ehux_30m_sep$step <-  reorder.factor (Ehux_30m_sep$step, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 
Ehux_30m_sep$depthf <-  reorder.factor (Ehux_30m_sep$depthf, new.order = c("shelf/slope", "open ocean")) 

Ehux30_sep.png <- ggplot (data=Ehux_30m_sep, aes(x=x, y=y, fill=depthf, color=depthf)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) + 
  scale_color_manual (values= c("#f1a340","#998ec3")) + scale_fill_manual (values= c("#f1a340","#998ec3")) + 
  geom_vline(xintercept = 0, linetype="dashed") +
  labs (x=expression(log[10]), y="probability density") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), legend.position = "bottom") + 
  facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "Ehux30_sep.png", width = 17, height = 6, dpi = 600, device="png", units="in")

Ehux_30m_sep$xexp <- 10^(Ehux_30m_sep$x)

##based on this, an infection step

forinfcomb30 <- Ehux_30m_sep %>% filter (step=="infcomb")
forinfcomb30$percent <- (10^(forinfcomb30$x))*100

nrow(forinfcomb30[forinfcomb30$percent<1, ]) #121
nrow(forinfcomb30[forinfcomb30$percent>1, ]) #110

resize.win (6, 6)

ggplot (data=Ehux_30m_sep %>% filter (step=="infcomb"), aes(x=log10((10^x)*100), y=y, fill=depthf, color=depthf)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) +   geom_vline(xintercept = 1, linetype="dashed") +
  scale_color_manual (values= c("#f1a340","#998ec3")) + scale_fill_manual (values= c("#f1a340","#998ec3")) + 
  scale_x_continuous(breaks = c(2, 1, 0, -1, -2, -3),label = c(100, 10, 1, 0.1, 0.01, 0.001)) +
  labs (x=expression("% population infected "~d^-1), y="probability density") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), legend.position = "bottom")


####1m
##density plots
#shelf
Ehux.abundance1.shelf <- ggplot (data=NAdata.betas_comb_1m_cn %>% filter (step=="abundance") %>% filter (depthf=="<200 m"), 
                                 aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity")
Ehux.abundance1.shelf <- ggplot_build(Ehux.abundance1.shelf)$data[[1]] %>% select (x,y)
Ehux.abundance1.shelf$entity <- "E. huxleyi"
Ehux.abundance1.shelf$step <- "abundance"
Ehux.abundance1.shelf$depthf <- "shelf/slope"

Ehux.enccomb1.shelf <- ggplot (data=NAdata.betas_comb_1m_cn %>% filter (step=="enccomb") %>% filter (depthf=="<200 m"),  
                               aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.enccomb1.shelf <- ggplot_build(Ehux.enccomb1.shelf)$data[[1]] %>% select (x,y)
Ehux.enccomb1.shelf$entity <- "E. huxleyi"
Ehux.enccomb1.shelf$step <- "enccomb"
Ehux.enccomb1.shelf$depthf <- "shelf/slope"


Ehux.adscomb1.shelf <- ggplot (data=NAdata.betas_comb_1m_cn  %>% filter (step=="adscomb") %>% filter (depthf=="<200 m"), 
                               aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.adscomb1.shelf <- ggplot_build(Ehux.adscomb1.shelf)$data[[1]] %>% select (x, y)
Ehux.adscomb1.shelf$entity <- "E. huxleyi"
Ehux.adscomb1.shelf$step <- "adscomb"
Ehux.adscomb1.shelf$depthf <- "shelf/slope"

Ehux.infcomb1.shelf <- ggplot (data=NAdata.betas_comb_1m_cn %>% filter (step=="infcomb") %>% filter (depthf=="<200 m"),
                               aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Ehux.infcomb1.shelf <- ggplot_build(Ehux.infcomb1.shelf)$data[[1]] %>% select (x, y)
Ehux.infcomb1.shelf$entity <- "E. huxleyi"
Ehux.infcomb1.shelf$step <- "infcomb"
Ehux.infcomb1.shelf$depthf <- "shelf/slope"

#open ocean
Ehux.abundance1.open <- ggplot (data=NAdata.betas_comb_1m_cn %>% filter (step=="abundance") %>% filter (depthf==">200 m"), 
                                aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity")
Ehux.abundance1.open <- ggplot_build(Ehux.abundance1.open)$data[[1]] %>% select (x, y)
Ehux.abundance1.open$entity <- "E. huxleyi"
Ehux.abundance1.open$step <- "abundance"
Ehux.abundance1.open$depthf <- "open ocean"

Ehux.enccomb1.open <- ggplot (data=NAdata.betas_comb_1m_cn %>% filter (step=="enccomb") %>% filter (depthf==">200 m"),  
                              aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.enccomb1.open <- ggplot_build(Ehux.enccomb1.open)$data[[1]] %>% select (x,y)
Ehux.enccomb1.open$entity <- "E. huxleyi"
Ehux.enccomb1.open$step <- "enccomb"
Ehux.enccomb1.open$depthf <- "open ocean"


Ehux.adscomb1.open <- ggplot (data=NAdata.betas_comb_1m_cn  %>% filter (step=="adscomb") %>% filter (depthf==">200 m"), 
                              aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.adscomb1.open <- ggplot_build(Ehux.adscomb1.open)$data[[1]] %>% select (x, y)
Ehux.adscomb1.open$entity <- "E. huxleyi"
Ehux.adscomb1.open$step <- "adscomb"
Ehux.adscomb1.open$depthf <- "open ocean"

Ehux.infcomb1.open <- ggplot (data=NAdata.betas_comb_1m_cn %>% filter (step=="infcomb") %>% filter (depthf==">200 m"),
                              aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Ehux.infcomb1.open <- ggplot_build(Ehux.infcomb1.open)$data[[1]] %>% select (x, y)
Ehux.infcomb1.open$entity <- "E. huxleyi"
Ehux.infcomb1.open$step <- "infcomb"
Ehux.infcomb1.open$depthf <- "open ocean"

Ehux_1m_sep <- rbind (Ehux.abundance1.shelf, Ehux.enccomb1.shelf, Ehux.adscomb1.shelf, Ehux.infcomb1.shelf, 
                      Ehux.abundance1.open, Ehux.enccomb1.open, Ehux.adscomb1.open, Ehux.infcomb1.open)

Ehux_1m_sep$step <-  reorder.factor (Ehux_1m_sep$step, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 
Ehux_1m_sep$depthf <-  reorder.factor (Ehux_1m_sep$depthf, new.order = c("shelf/slope", "open ocean")) 

Ehux1_sep.png <- ggplot (data=Ehux_1m_sep, aes(x=x, y=y, fill=depthf, color=depthf)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) + 
  scale_color_manual (values= c("#f1a340","#998ec3")) + scale_fill_manual (values= c("#f1a340","#998ec3")) + 
  geom_vline(xintercept = 0, linetype="dashed") +
  labs (x=expression(log[10]), y="probability density") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), legend.position = "bottom") + 
  facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "Ehux1_sep.png", width = 17, height = 6, dpi = 600, device="png", units="in")

Ehux_1m_sep$xexp <- 10^(Ehux_1m_sep$x)


##based on this, an infection step

forinfcomb <- Ehux_1m_sep %>% filter (step=="infcomb")
forinfcomb$percent <- (10^(forinfcomb$x))*100

nrow(forinfcomb[forinfcomb$percent<1, ]) #98
nrow(forinfcomb[forinfcomb$percent>1, ]) #137

resize.win (6, 6)

ggplot (data=Ehux_1m_sep %>% filter (step=="infcomb"), aes(x=log10((10^x)*100), y=y, fill=depthf, color=depthf)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) +   geom_vline(xintercept = 1, linetype="dashed") +
  scale_color_manual (values= c("#f1a340","#998ec3")) + scale_fill_manual (values= c("#f1a340","#998ec3")) + 
  scale_x_continuous(breaks = c(2, 1, 0, -1, -2, -3),label = c(100, 10, 1, 0.1, 0.01, 0.001)) +
  labs (x=expression("% population infected "~d^-1), y="probability density") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), legend.position = "bottom")

##check rates
sum.inf30 <- forinfcomb30 %>% group_by (depthf) %>% filter (percent>9.9) %>%
  summarise (freq=sum(y*100))

sum.inf <- forinfcomb %>% group_by (depthf) %>% filter (percent>9.9) %>%
  summarise (freq=sum(y*100))

forinfcomb$percent <- (10^(forinfcomb$x))*100

sum.all <- Ehux_1m_sep %>% filter (!(step=="abundance")) %>% group_by (step, depthf) %>% 
  mutate (percent=(10^(x)*100)) %>%
  filter (percent>9.9) %>%
  summarise (freq=sum(y*100))

Ehux_30m_sep %>% filter (step=="abundance") %>% group_by (depthf) %>% filter (x>2.9) %>%  summarise (freq=sum(y*100))


##to get the most freq observed, most observed freq
Ehux_30m_sep.sum <- Ehux_30m_sep %>% filter (!(step=="abundance")) %>% group_by (depthf) %>% filter (y>0.02)  %>%
    mutate (percent=(10^(x)*100))

lith_30m_sep.sum <- lith_30m_sep %>% filter (!(step=="abundance")) %>% group_by (depthf) %>% filter (y>0.02)  %>%
  mutate (percent=(10^(x)*100))

Ehux_1m_sep %>% filter (step=="adscomb") %>% group_by (depthf) %>% filter (xexp>0.99) %>%  summarise (freq=sum(y*100))

lith_30m_sep %>% filter (step=="enccomb") %>% group_by (depthf) %>% filter (y>0.02) %>%  summarise (freq=sum(y*100))

Ehux_30m_sep %>% filter (step=="infcomb") %>% group_by (depthf) %>% filter (y>0.02) %>%  summarise (freq=sum(y*100))
Ehux_1m_sep %>% filter (step=="infcomb") %>% group_by (depthf) %>% filter (y>0.02) %>%  summarise (freq=sum(y*100))


Ehux_30m_sep %>% filter (step=="enccomb") %>% group_by (depthf) %>% filter (xexp>0.99) %>%  summarise (freq=sum(y*100))

Ehux_30m_sep %>% filter (step=="infcomb") %>% group_by (step, depthf) %>% 
  mutate (percent=(10^(x)*100)) %>%
  filter (percent>28) %>%
  summarise (freq=sum(y*100))

Ehux_30m_sep %>% filter (step=="abundance") %>% group_by (depthf) %>% filter (x>4.9)  %>% 
  mutate (percent=(10^(x)*100)) %>%
  summarise (freq=sum(y*100))

Ehux_1m_sep.sum <- Ehux_1m_sep %>% filter (!(step=="abundance")) %>% group_by (depthf) %>% filter (y>0.02)  %>%
  mutate (percent=(10^(x)*100))

Ehux_30m_sep %>% filter (!(step=="enccomb")) %>% group_by (depthf) %>% filter (y>0.02)  %>%
  mutate (percent=(10^(x)*100))

lith_30m_sep.sum <- lith_30m_sep %>% filter (!(step=="abundance")) %>% group_by (depthf) %>% filter (y>0.02)  %>%
  mutate (percent=(10^(x)*100))

Ehux_30m_sep$percent <- (10^(Ehux_30m_sep$x)*100)
Ehux_1m_sep$percent <- (10^(Ehux_1m_sep$x)*100)



###### for liths
##density plots
#shelf
lith.abundance30.shelf <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="lith") %>% filter (step=="abundance") %>% 
                                    filter (depthf=="shelf/slope"), 
                                  aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity")
lith.abundance30.shelf <- ggplot_build(lith.abundance30.shelf)$data[[1]] %>% select (x,y)
lith.abundance30.shelf$entity <- "lith"
lith.abundance30.shelf$step <- "abundance"
lith.abundance30.shelf$depthf <- "shelf/slope"

lith.enccomb30.shelf <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="lith") %>% filter (step=="enccomb") %>% filter (depthf=="shelf/slope"),  
                                aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
lith.enccomb30.shelf <- ggplot_build(lith.enccomb30.shelf)$data[[1]] %>% select (x,y)
lith.enccomb30.shelf$entity <- "lith"
lith.enccomb30.shelf$step <- "enccomb"
lith.enccomb30.shelf$depthf <- "shelf/slope"

lith.adscomb30.shelf <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="lith")  %>% filter (step=="adscomb") %>% filter (depthf=="shelf/slope"), 
                                aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
lith.adscomb30.shelf <- ggplot_build(lith.adscomb30.shelf)$data[[1]] %>% select (x, y)
lith.adscomb30.shelf$entity <- "lith"
lith.adscomb30.shelf$step <- "adscomb"
lith.adscomb30.shelf$depthf <- "shelf/slope"

#open ocean

lith.abundance30.open <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="lith") %>% filter (step=="abundance") %>% filter (depthf=="open ocean"), 
                                 aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity")
lith.abundance30.open <- ggplot_build(lith.abundance30.open)$data[[1]] %>% select (x, y)
lith.abundance30.open$entity <- "lith"
lith.abundance30.open$step <- "abundance"
lith.abundance30.open$depthf <- "open ocean"

lith.enccomb30.open <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="lith") %>%  filter (step=="enccomb") %>% 
                                 filter (depthf=="open ocean"),  
                               aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
lith.enccomb30.open <- ggplot_build(lith.enccomb30.open)$data[[1]] %>% select (x,y)
lith.enccomb30.open$entity <- "lith"
lith.enccomb30.open$step <- "enccomb"
lith.enccomb30.open$depthf <- "open ocean"

lith.adscomb30.open <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (entity=="lith")  %>% filter (step=="adscomb") %>% filter (depthf=="open ocean"), 
                               aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
lith.adscomb30.open <- ggplot_build(lith.adscomb30.open)$data[[1]] %>% select (x, y)
lith.adscomb30.open$entity <- "lith"
lith.adscomb30.open$step <- "adscomb"
lith.adscomb30.open$depthf <- "open ocean"

lith_30m_sep <- rbind (lith.abundance30.shelf, lith.enccomb30.shelf, lith.adscomb30.shelf, 
                       lith.abundance30.open, lith.enccomb30.open, lith.adscomb30.open)

lith_30m_sep$perpop <- (10^(lith_30m_sep$x))*100 #80% of the population will have adsorbed EhVs
lith_30m_sep$xexp <- 10^(lith_30m_sep$x)

resize.win (6, 6)

ggplot (data=lith_30m_sep %>% filter (step=="adscomb"), aes(x=log10((10^x)*100), y=y, fill=depthf, color=depthf)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) + 
  scale_color_manual (values= c("#f1a340","#998ec3")) + scale_fill_manual (values= c("#f1a340","#998ec3")) + 
  scale_x_continuous(breaks = c(2, 1, 0, -1, -2, -3),label = c(100, 10, 1, 0.1, 0.01, 0.001)) +
  labs (x=expression("% population with\nadsorbed EhVs"~d^-1), y="probability density") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), legend.position = "bottom", 
        axis.title.y = element_text(vjust=0.001))

lith_30m_sep$step <-  reorder.factor (lith_30m_sep$step, new.order = c("abundance", 'enccomb', "adscomb")) 
lith_30m_sep$depthf <-  reorder.factor (lith_30m_sep$depthf, new.order = c("shelf/slope", "open ocean")) 

lith30_sep.png <- ggplot (data=lith_30m_sep, aes(x=x, y=y, fill=depthf, color=depthf)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) + 
  scale_color_manual (values= c("#f1a340","#998ec3")) + scale_fill_manual (values= c("#f1a340","#998ec3")) + 
  geom_vline(xintercept = 0, linetype="dashed") +
  labs (x=expression(log[10]), y="probability density") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), legend.position = "bottom") + 
  facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "lith30_sep.png", width = 13, height = 6, dpi = 600, device="png", units="in")


###navice slice
##filter NA-VICE sites
navice <- NAdata.betas_comb_30m_cn %>% filter (between (lat, 50, 70)) %>% filter (between (lon, -40, -20))
navice$daysto <- 1/navice$combvalue
navice$percent <- 100*navice$combvalue

#colors not adjusted
#filter infection and Cc (panel A), navice slice
navice_slice.png <- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-40, -20), ylim = c(50, 70 ) , expand = FALSE) +
  geom_point(data= navice %>% filter (step=="infcomb30"),  aes(x = lon, y = lat, color = log10(combvalue)))+
  scale_colour_gradientn(colours = BlRed(100)) +
  scale_x_continuous(breaks=seq(-40, -20, 10)) +
  scale_y_continuous(breaks=seq(50, 70, 5)) +
  theme_Publication2() +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) + 
  labs (color= expression(log[10]), x="", y="") +
  guides(color = guide_colorbar(title.position = "right"))


ggsave(filename = "navice_slice.png", width = 5, height = 5, dpi = 600, device="png", units="in")

##put coords on 'the navice slice
coords <- read.csv("D:/Users/karengb/CSV Files/coords.txt", sep=";")

naviceslice_coords_origcolor.png <- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-40, -20), ylim = c(50, 70 ) , expand = FALSE) +
  geom_point(data= navice %>% filter (step=="infcomb30") %>% filter (combvalue>0.099),  aes(x = lon, y = lat, color = log10(combvalue)))+
  scale_colour_gradientn(colours = BlRed(100)) +
  scale_x_continuous(breaks=seq(-40, -20, 10)) +
  scale_y_continuous(breaks=seq(50, 70, 5)) +
  theme_Publication2() +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) + 
  labs (color= expression(log[10]~"infections"~entity^-1~d^-1), x="", y="") +
  guides(color = guide_colorbar(title.position = "right")) +
  geom_point(data=coords, aes(x=lon, y=lat), color="black", size=4)

ggsave(filename = "naviceslice_coords_origcolor.png", width = 5, height = 5, dpi = 600, device="png", units="in")


naviceslice_coords_origcolor_percent.png <- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-40, -20), ylim = c(50, 70 ) , expand = FALSE) +
  geom_point(data= navice %>% filter (step=="infcomb30"),  aes(x = lon, y = lat, color = log10(combvalue*100)))+
  scale_colour_gradientn(colours = BlRed(100)) +
  scale_x_continuous(breaks=seq(-40, -20, 10)) +
  scale_y_continuous(breaks=seq(50, 70, 5)) +
  theme_Publication2() +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) + 
  labs (color= expression("log10 percent infected"~d^-1), x="", y="") +
  guides(color = guide_colorbar(title.position = "right")) +
  geom_point(data=coords, aes(x=lon, y=lat), color="black", size=4)

ggsave(filename = "naviceslice_coords_origcolor_percent.png", width = 5, height = 5, dpi = 600, device="png", units="in")

navice.inf <- navice %>% filter (step=="infcomb30") %>% group_by (depthf) %>% filter (percent>9.9)


##check disrates
library (plotrix)
cols <- c("max_wind_speed", "disrate_1m", "disrate_30m")

sum_all <- NAdata_merge %>% group_by(depthf) %>% 
  summarise_at(.vars = cols,
               funs(max, min, mean), na.rm=TRUE)

ggplotly(ggplot (data=NAdata_merge, aes(x=log10(disrate_30m))) +
           geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") + facet_grid (~depthf))


##whole NA infection
NA_infection_coords_1m <- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data= NAdata.betas_comb_1m_cn %>% filter(step=="infcomb") %>% filter (combvalue>0.099), 
             aes(x = lon, y = lat, color = log10(combvalue)))+
  scale_colour_gradientn(colours = BlRed(100))  + 
  scale_x_continuous(breaks=seq(-70, 10, 20)) +
  scale_y_continuous(breaks=seq(30, 75, 20)) +
  theme_Publication2() +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) + 
  labs (color= expression(log[10]~"infections"~entity^-1~d^-1), x="", y="") +
  guides(color = guide_colorbar(title.position = "right")) +
  geom_point(data=coords, aes(x=lon, y=lat), color="black", size=4)


ggsave(filename = "NA_infection_coords_1m.png", width = 7.5, height = 5, dpi = 600, device="png", units="in")
