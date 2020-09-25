#Note: this is the merged data set from Ali and Toby, where waterdepth<100m and abundance<10 were eliminated from the dataset. This is a melted dataset where entities (i.e., Cc and Li) are just in one column
NAdata_merge <- read.csv("D:/Users/karengb/Exported Tables/NAdata_merge_masked_withNcandEhux.csv")
source("inspack_map.R")

#density plots #change color schemes to colorblind friendly
resize.win (10, 5)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")

#just choose E. huxleyi and cells
#NAdata_merge <- NAdata_merge2 %>% filter (entitycode %in% c("Ncells_total", "Nliths_max"))
#NAdata_merge$entity <- factor (NAdata_merge$entitycode, levels = c("Ncells_total", "Nliths_max"), labels= c("E. huxleyi", "Coccoliths"))

#get data out of each histogram, and make an excel file out of it (saved in onedrive: density raw.
#used density_merged, saved in CSV files)

#tried getting data directly from R
Ehux.shelf <- ggplot (data=NAdata_merge %>% filter (depthf=="<200 m") %>% 
                        filter (entitycode=="Ncells_total"), 
                    aes(x=log10(abundance),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)*10))  + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

Ehux.shelf <- ggplot_build(Ehux.shelf)$data[[1]] %>% select (x, y)
Ehux.shelf$entity <- "E. huxleyi"
Ehux.shelf$depthf <- "<200 m"

Ehux.open <- ggplot (data=NAdata_merge %>% filter (depthf==">200 m") %>% 
                       filter (entitycode=="Ncells_total"), 
                   aes(x=log10(abundance),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)*10))  + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

Ehux.open <- ggplot_build(Ehux.open)$data[[1]] %>% select (x, y)
Ehux.open$entity <- "E. huxleyi"
Ehux.open$depthf <- ">200 m"

Li.shelf <- ggplot (data=NAdata_merge %>% filter (depthf=="<200 m") %>% filter (entity=="lith"), 
                   aes(x=log10(abundance),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)*10))  + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

Li.shelf <- ggplot_build(Li.shelf)$data[[1]] %>% select (x, y)
Li.shelf$entity <- "lith"
Li.shelf$depthf <- "<200 m"

Li.open <- ggplot (data=NAdata_merge %>% filter (depthf==">200 m") %>% filter (entity=="lith"), 
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
  scale_colour_gradientn(colours = BlRed(100)) +
theme_Publication2() + 
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
  scale_colour_gradientn(colours = BlRed(100)) +  
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
probs <- as.data.frame(list (entity = as.factor (rep(c("calcified","lith", "naked"), 2)), virus = rep(c("high", "low"), 1, each=3),propvir= rep(c(0.33, 0.67), 1, each=3), ads = rep(c(0.0095, 0.0134, 0.0305), 2), inf = rep(c(0.3, NA, 0.3,  0.06, NA, 0.06))))

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
  scale_fill_manual (values=c("#377eb8", "#e41a1c")) + geom_vline(xintercept = 1, linetype="dashed") +
  labs (x="log10", y="density probability") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines")) + facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "sum1.png", width = 17, height = 5, dpi = 600, device="png", units="in")

#split 30m and 30m
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

NAdata.betas_comb_30m <- NAdata.betas_30m %>% filter (virus=="high") %>% select (c(lat, lon, entity, depthf, abundance30, virnum, disrate_30m))
NAdata.betas_comb_30m$enccomb30 <- high_NAdata.betas_30m$encounters + low_NAdata.betas_30m$encounters
NAdata.betas_comb_30m$adscomb30 <- high_NAdata.betas_30m$adstot + low_NAdata.betas_30m$adstot
NAdata.betas_comb_30m$infcomb30 <- high_NAdata.betas_30m$sucinf + low_NAdata.betas_30m$sucinf
#NAdata.betas_comb <- NAdata.betas_comb %>% mutate(adscomb302= if_else(adscomb30 > enccomb30, adscomb30, adscomb30))

##percentage of parameters
NAdata.betas_comb_30m$perencounters <- (NAdata.betas_comb_30m$enccomb30/NAdata.betas_comb_30m$abundance30)*100
NAdata.betas_comb_30m$peradsorbed <- (NAdata.betas_comb_30m$adscomb30/NAdata.betas_comb_30m$abundance30)*100
NAdata.betas_comb_30m$perinf <- (NAdata.betas_comb_30m$infcomb30/NAdata.betas_comb_30m$abundance30)*100
NAdata.betas_comb_30m <- NAdata.betas_comb_30m %>% mutate(perencounters= if_else(perencounters > 1, 1, perencounters)) %>% mutate(peradsorbed= if_else(perencounters > 1, 1, peradsorbed)) %>% mutate(perinf= if_else(perinf > 1, 1, perinf)) 

#melt dataset
melted_NAdata.betas_comb_30m <- reshape2::melt (NAdata.betas_comb_30m %>% select (lat, lon, entity, depthf, abundance30, enccomb30, adscomb30, infcomb30), id.vars = c("lat", "lon", "depthf", "entity"), value.name = "value", variable.name = "step")

melted_NAdata.betas_comb_30m$step <-  reorder.factor (melted_NAdata.betas_comb_30m$step, new.order = c("abundance30", 'enccomb30', "adscomb30", "infcomb30")) 

variable_labs <- c(
  `abundance30` = 'concentration~(mL^{-1})',
  `enccomb30` = 'encounters~entity^{-1}~d^{-1}',
  `adscomb30` = 'adsorptions~entity^{-1}~d^{-1}',
  `infcomb30` = 'infections~entity^{-1}~d^{-1}'
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
  scale_fill_manual (values=c("#377eb8", "#e41a1c")) + geom_vline(xintercept = 1, linetype="dashed") +
  labs (x="log10", y="density probability") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines")) + facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "sum30.png", width = 17, height = 5, dpi = 600, device="png", units="in")

##try to combine naked and calcified

#add everything
calc_NAdata.betas_1m <- melted_NAdata.betas_comb_1m %>% filter (entity=="calcified")
calc_NAdata.betas_1m$calc.value <- calc_NAdata.betas_1m$value
calc_NAdata.betas_1m <- calc_NAdata.betas_1m %>% select (! (c(value, entity)))
naked_NAdata.betas_1m <- melted_NAdata.betas_comb_1m %>% filter (entity=="naked")
naked_NAdata.betas_1m$naked.value <- naked_NAdata.betas_1m$value
naked_NAdata.betas_1m <- naked_NAdata.betas_1m %>% select (! (c(value, entity)))

NAdata.betas_comb_1m_cn <- left_join(calc_NAdata.betas_1m, naked_NAdata.betas_1m)
#change NA to 0
NAdata.betas_comb_1m_cn[is.na(NAdata.betas_comb_1m_cn)]<- 0
NAdata.betas_comb_1m_cn$combvalue <- NAdata.betas_comb_1m_cn$calc.value + NAdata.betas_comb_1m_cn$naked.value

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
  scale_fill_manual (values=c("#c51b8a")) + geom_vline(xintercept = 1, linetype="dashed") +
  labs (x="log10", y="density probability") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines")) + facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "Ehux1.png", width = 17, height = 5, dpi = 600, device="png", units="in")

#add everything
calc_NAdata.betas_30m <- melted_NAdata.betas_comb_30m %>% filter (entity=="calcified")
calc_NAdata.betas_30m$calc.value <- calc_NAdata.betas_30m$value
calc_NAdata.betas_30m <- calc_NAdata.betas_30m %>% select (! (c(value, entity)))
naked_NAdata.betas_30m <- melted_NAdata.betas_comb_30m %>% filter (entity=="naked")
naked_NAdata.betas_30m$naked.value <- naked_NAdata.betas_30m$value
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
Ehux.abundance30 <- ggplot_build(Ehux.abundance30)$data[[1]] %>% select (x, y)
Ehux.abundance30$entity <- "E. huxleyi"
Ehux.abundance30$step <- "abundance"

Ehux.enccomb30 <- ggplot (data=NAdata.betas_comb_30m_cn %>% filter (step=="enccomb"), 
                          aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.enccomb30 <- ggplot_build(Ehux.enccomb30)$data[[1]] %>% select (x, y)
Ehux.enccomb30$entity <- "E. huxleyi"
Ehux.enccomb30$step <- "enccomb"

Ehux.adscomb30 <- ggplot (data=NAdata.betas_comb_30m_cn  %>% filter (step=="adscomb"), 
                          aes(x=log10(combvalue))) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.adscomb30 <- ggplot_build(Ehux.adscomb30)$data[[1]] %>% select (x, y)
Ehux.adscomb30$entity <- "E. huxleyi"
Ehux.adscomb30$step <- "adscomb"

Ehux.infcomb30 <- ggplot (data=NAdata.betas_comb_30m_cn %>% filter (step=="infcomb"), 
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
  scale_fill_manual (values=c("#c51b8a")) + geom_vline(xintercept = 1, linetype="dashed") +
  labs (x="log10", y="density probability") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines")) + facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "Ehux30.png", width = 17, height = 5, dpi = 600, device="png", units="in")

##liths
#1m
lith_1m.png<- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data= melted_NAdata.betas_comb_1m %>% filter (entity=="lith"),  
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
  guides(color = guide_colorbar(title.position = "right")) +  facet_grid(~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "lith_1m.png", width = 10, height = 10, dpi = 600, device="png", units="in")

###navice slice
##filter NA-VICE sites
navice <- melted_NAdata.betas_comb_30m %>% filter (between (lat, 50, 70)) %>% filter (between (lon, -40, -20))

##all steps
navice.png<- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-40, -20), ylim = c(50, 70 ) , expand = FALSE) +
  geom_point(data= navice %>% filter (! ((entity=="Li") & (step=="infcomb"))), 
             aes(x = lon, y = lat, color = log10(value)))+
  scale_colour_gradientn(colours = BlRed(100), breaks = c(seq (-7,7,2)), limits=c(-8, 7)) +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(),  legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + facet_grid(entity~step, labeller = labeller(step = as_labeller(variable_labs, label_parsed))) + labs (x="", y="") 

ggsave(filename = "navice.png", width = 12, height = 10, dpi = 600, device="png", units="in")

#filter infection and Cc (panel A), navice slice
resize.win (5,5)
navice_slice.png <- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-40, -20), ylim = c(50, 70 ) , expand = FALSE) +
  geom_point(data= navice %>% filter (! (entity=="lith"))%>% filter (step=="infcomb"),  
             aes(x = lon, y = lat, color = log10(value)))+
  scale_colour_gradientn(colours = BlRed(100), breaks = c(seq (-7,7, 2)), limits=c(-8, 7)) +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(),  legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10))

ggsave(filename = "navice_slice.png", width = 5, height = 5, dpi = 600, device="png", units="in")


#colors not adjusted
#filter infection and Cc (panel A), navice slice
resize.win (5,5)
ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-40, -20), ylim = c(50, 70 ) , expand = FALSE) +
  geom_point(data= navice %>% filter (! (entity=="lith"))%>% filter (step=="infcomb"),  
             aes(x = lon, y = lat, color = log10(value)))+
  scale_colour_gradientn(colours = BlRed(100)) +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(),  legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10))

##put coords on the navice slice
coords <- read.csv("D:/Users/karengb/CSV Files/coords.txt", sep=";")

resize.win(5,5)

naviceslice_coords.png <- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-40, -20), ylim = c(50, 70 ) , expand = FALSE) +
  geom_point(data= navice %>% filter (! (entity=="lith"))%>% filter (step=="infcomb"),  
             aes(x = lon, y = lat, color = log10(value)))+
  scale_colour_gradientn(colours = BlRed(100), breaks = c(seq (-7,7, 2)), limits=c(-8, 7)) +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", 
                               legend.key.height = unit (4, "line"), 
                               legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), 
                               panel.spacing = unit(3, "lines"), 
                               axis.title = element_blank(),  legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + labs (x="", y="") + 
  geom_point(data=coords, aes(x=lon, y=lat), color="black", size=4)

ggsave(filename = "naviceslice_coords.png", width = 5, height = 5, dpi = 600, device="png", units="in")


naviceslice_coords_origcolor.png <- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-40, -20), ylim = c(50, 70 ) , expand = FALSE) +
  geom_point(data= navice %>% filter (! (entity=="lith"))%>% filter (step=="infcomb"),  
             aes(x = lon, y = lat, color = log10(value)))+
  scale_colour_gradientn(colours = BlRed(100)) +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), 
                               legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), 
                               axis.title = element_blank(),  legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) +
  labs (x="", y="") + 
  geom_point(data=coords, aes(x=lon, y=lat), color="black", size=4)

ggsave(filename = "naviceslice_coords_origcolor.png", width = 5, height = 5, dpi = 600, device="png", units="in")

