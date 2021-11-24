#Note: this is the merged data set from Ali and Toby, where waterdepth<100m and abundance<10 were eliminated from the dataset. This is a melted dataset where entities (i.e., Cc and Li) are just in one column
NAdata_merge <- read.csv("D:/Users/karengb/Exported Tables/NAdata_merge_mar2021.csv")
NAdata_merge$group <- NAdata_merge$entity
NAdata_merge$entity <- factor (NAdata_merge$entitycode, levels = c("Ncells_max", "Nliths_max"), 
                               labels= c("E. huxleyi", "coccolith"))
source("inspack_map.R")
resize.win (10, 5)

##-----------------------------------------------------------------------------------------
#probability densities
Cc.shelf <- ggplot (data=NAdata_merge %>% filter (depthf=="shelf/slope") %>% filter (group=="Cc"), 
                    aes(x=log10(abundance),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)*10))  + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

Cc.shelf <- ggplot_build(Cc.shelf)$data[[1]] %>% select (x, y)
Cc.shelf$entity <- "E. huxleyi"
Cc.shelf$depthf <- "shelf/slope"

Cc.open <- ggplot (data=NAdata_merge %>% filter (depthf=="open ocean") %>% filter (group=="Cc"), 
                   aes(x=log10(abundance),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)*10))  + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

Cc.open <- ggplot_build(Cc.open)$data[[1]] %>% select (x, y)
Cc.open$entity <- "E. huxleyi"
Cc.open$depthf <- "open ocean"

Li.shelf <- ggplot (data=NAdata_merge %>% filter (depthf=="shelf/slope") %>% filter (group=="Li"), 
                    aes(x=log10(abundance),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)*10))  + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

Li.shelf <- ggplot_build(Li.shelf)$data[[1]] %>% select (x, y)
Li.shelf$entity <- "coccolith"
Li.shelf$depthf <- "shelf/slope"

Li.open <- ggplot (data=NAdata_merge %>% filter (depthf=="open ocean") %>% filter (group=="Li"), 
                   aes(x=log10(abundance),  color=depthf, fill=depthf)) +
  geom_histogram(binwidth = 0.1, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") +
  geom_density(position = "identity", alpha=0.25, aes (y = ..count../sum(..count..)*10))  + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1)) +  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

Li.open <- ggplot_build(Li.open)$data[[1]] %>% select (x, y)
Li.open$entity <- "coccolith"
Li.open$depthf <- "open ocean"

density_merged <- rbind (Cc.shelf, Cc.open, Li.shelf, Li.open)

density_merged$entity <-  reorder.factor (density_merged$entity, new.order = c("E. huxleyi", "coccolith")) 

#save data
write.table (density_merged, "Exported Tables/density_merged_NAdata_v7.csv", sep=",", col.names=T, row.names=F)

resize.win(10, 5)

#plot
hist_abundance_ent.png <- ggplot (data=density_merged, aes(x=x,  y= y, color=depthf, fill=depthf)) + 
  geom_area(position = "identity", alpha=0.25) +
  geom_line(size=1) + geom_vline(xintercept = 1, linetype="dashed") + scale_x_continuous(breaks = c(-1:6)) +
  scale_color_manual (values= c("#998ec3", "#f1a340")) + scale_fill_manual (values= c("#998ec3", "#f1a340")) +
  facet_grid(~entity) + theme_Publication2() + 
  labs (x = expression(log[10]~"concentration"~mL^-1), y= "probability density") +  
  theme(legend.title = element_blank(), legend.key.width = unit (1, "cm")) 

ggsave(filename = "hist_abundance_ent.png", width = 10, height = 5, dpi = 600, device="png", units="in")

resize.win(10,5)
hist_abundance_ent.png

###plot map of abundance
#calling world
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#color scheme
BlRed <- colorRampPalette((c('#00429d', '#2b57a7', '#426cb0', '#5681b9', '#6997c2', '#7daeca', '#93c4d2', '#abdad9', '#caefdf', '#ffe2ca', '#ffc4b4', '#ffa59e', '#f98689', '#ed6976', '#dd4c65', '#ca2f55', '#b11346', '#93003a')))

abundance.png <- ggplot(data = world)+ geom_sf(color="darkgray", fill="gainsboro") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+   geom_point(data=NAdata_merge, aes(x = lon, y = lat, color = log10(abundance)))+ 
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

##-----------------------------------------------------------------------------------------
######map wind speed and dissipation rates

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

##cannot use the same log scale label for dissipation rates because differences cannot be seen
#disrate_upper1m
disrate_1m.png <-  ggplot(data = world)+ geom_sf(color="darkgray", fill="gainsboro") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=NAdata_merge, aes(x = lon, y = lat, color = log10(disrate_1m)))+
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

##-----------------------------------------------------------------------------------------
##add betas to dataframe
BM <- read.csv("D:/Users/karengb/CSV Files/beta_master8_BM.csv", sep=";")
DS <- read.csv("D:/Users/karengb/CSV Files/beta_master8_DS.csv", sep=";")
NAdata.betas <- left_join (NAdata_merge, BM) %>% left_join(., DS %>% select (group, beta_DS))

##-----------------------------------------------------------------------------------------
###simulation for 30m

#calculate beta turb
v= 1.099*(10)^-6 #m2/s kinematic viscosity in 18C
Rehv= 90*(10)^-9 #in m radius virus

#probabilities
probs <- as.data.frame(list (group = as.factor(c("Cc", "Li","Cc", "Li")), virus = c("high","high", "low", "low"), 
                             propvir= rep(c(0.33, 0.67), 1, each=2),
                             ads= c(0.63, 0.63, 0.63, 0.63),  inf = c(0.3, NA, 0.06, NA)))

#split 30m 
NAdata.betas_30m = NAdata.betas %>% select(!(disrate_1m)) 
NAdata.betas_30m$beta_turb <- (4.2*pi*((NAdata.betas_30m$disrate_30m/(v))^0.5)*((NAdata.betas_30m$rad+Rehv)^3))*86400*10^6 
NAdata.betas_30m$beta_all <- NAdata.betas_30m$beta_BM + NAdata.betas_30m$beta_DS + NAdata.betas_30m$beta_turb

#join NAdata and probs
NAdata.betas_30m <- left_join(NAdata.betas_30m, probs)

#calculate propEhV
NAdata.betas_30m$propEhV <- NAdata.betas_30m$virnum* NAdata.betas_30m$propvir

#calculate encounters fast slow
NAdata.betas_30m$encounters <- NAdata.betas_30m$beta_all*NAdata.betas_30m$propEhV 

#calculate total adsorption by virus props
NAdata.betas_30m$adstot <- NAdata.betas_30m$encounters*NAdata.betas_30m$ads

#calculate total successful infections
NAdata.betas_30m$sucinf <- NAdata.betas_30m$encounters*NAdata.betas_30m$ads*NAdata.betas_30m$inf

#add everything
high_NAdata.betas_30m <- NAdata.betas_30m %>% filter (virus=="high")
low_NAdata.betas_30m <- NAdata.betas_30m %>% filter (virus=="low")

NAdata.betas_comb_30m <- NAdata.betas_30m %>% filter (virus=="high") %>% select (c(lat, lon, group, depthf, abundance, virnum, disrate_30m))
NAdata.betas_comb_30m$enccomb <- high_NAdata.betas_30m$encounters + low_NAdata.betas_30m$encounters
NAdata.betas_comb_30m$adscomb <- high_NAdata.betas_30m$adstot + low_NAdata.betas_30m$adstot
NAdata.betas_comb_30m$infcomb <- high_NAdata.betas_30m$sucinf + low_NAdata.betas_30m$sucinf

#melt dataset
melted_NAdata.betas_comb_30m <- reshape2::melt (NAdata.betas_comb_30m %>% select (lat, lon, group, depthf, abundance, enccomb, adscomb, infcomb), id.vars = c("lat", "lon", "depthf", "group"), value.name = "value", variable.name = "parameter")

melted_NAdata.betas_comb_30m$parameter <-  reorder.factor (melted_NAdata.betas_comb_30m$parameter, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 

variable_labs <- c(
  `abundance` = 'concentration~(mL^{-1})',
  `enccomb` = 'encounters~entity^{-1}~d^{-1}',
  `adscomb` = 'adsorptions~entity^{-1}~d^{-1}',
  `infcomb` = 'infections~entity^{-1}~d^{-1}'
)

melted_NAdata.betas_comb_30m$entity <- factor (melted_NAdata.betas_comb_30m$group, levels = c("Cc", "Li"), 
                               labels= c("E. huxleyi", "coccolith"))

#plot map Ehux
all_30m_Ehux.png<- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data= melted_NAdata.betas_comb_30m %>% filter (group=="Cc"),
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
  guides(color = guide_colorbar(title.position = "right")) +  
  facet_grid(~parameter, labeller = labeller(parameter = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "all_30m_Ehux.png", width = 17, height = 6, dpi = 600, device="png", units="in")

#plot map coccolith
all_30m_lith.png<- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data= melted_NAdata.betas_comb_30m %>% filter (group=="Li") %>% filter (!parameter=="infcomb"),
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
  guides(color = guide_colorbar(title.position = "right")) +  
  facet_grid(~parameter, labeller = labeller(parameter = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "all_30m_lith.png", width = 13, height = 6, dpi = 600, device="png", units="in")

#plot probability density
#Ehux shelf
Ehux.abundance30.shelf <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (group=="Cc") %>% 
                                    filter (parameter=="abundance") %>% filter (depthf=="shelf/slope"), 
                                  aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity")
Ehux.abundance30.shelf <- ggplot_build(Ehux.abundance30.shelf)$data[[1]] %>% select (x,y)
Ehux.abundance30.shelf$entity <- "E. huxleyi"
Ehux.abundance30.shelf$parameter <- "abundance"
Ehux.abundance30.shelf$depthf <- "shelf/slope"

Ehux.enccomb30.shelf <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (group=="Cc") %>% 
                                  filter (parameter=="enccomb") %>% filter (depthf=="shelf/slope"),  
                                aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.enccomb30.shelf <- ggplot_build(Ehux.enccomb30.shelf)$data[[1]] %>% select (x,y)
Ehux.enccomb30.shelf$entity <- "E. huxleyi"
Ehux.enccomb30.shelf$parameter <- "enccomb"
Ehux.enccomb30.shelf$depthf <- "shelf/slope"

Ehux.adscomb30.shelf <- ggplot (data=melted_NAdata.betas_comb_30m  %>% filter (group=="Cc")
                                %>% filter (parameter=="adscomb") %>% filter (depthf=="shelf/slope"), 
                                aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.adscomb30.shelf <- ggplot_build(Ehux.adscomb30.shelf)$data[[1]] %>% select (x, y)
Ehux.adscomb30.shelf$entity <- "E. huxleyi"
Ehux.adscomb30.shelf$parameter <- "adscomb"
Ehux.adscomb30.shelf$depthf <- "shelf/slope"

Ehux.infcomb30.shelf <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (group=="Cc") %>% 
                                  filter (parameter=="infcomb") %>% filter (depthf=="shelf/slope"),
                                aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Ehux.infcomb30.shelf <- ggplot_build(Ehux.infcomb30.shelf)$data[[1]] %>% select (x, y)
Ehux.infcomb30.shelf$entity <- "E. huxleyi"
Ehux.infcomb30.shelf$parameter <- "infcomb"
Ehux.infcomb30.shelf$depthf <- "shelf/slope"

#Ehux open ocean
Ehux.abundance30.open <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (group=="Cc") 
                                 %>% filter (parameter=="abundance") %>% filter (depthf=="open ocean"), 
                                 aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity")
Ehux.abundance30.open <- ggplot_build(Ehux.abundance30.open)$data[[1]] %>% select (x, y)
Ehux.abundance30.open$entity <- "E. huxleyi"
Ehux.abundance30.open$parameter <- "abundance"
Ehux.abundance30.open$depthf <- "open ocean"

Ehux.enccomb30.open <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (group=="Cc") %>% 
                                 filter (parameter=="enccomb") %>% filter (depthf=="open ocean"),  
                               aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.enccomb30.open <- ggplot_build(Ehux.enccomb30.open)$data[[1]] %>% select (x,y)
Ehux.enccomb30.open$entity <- "E. huxleyi"
Ehux.enccomb30.open$parameter <- "enccomb"
Ehux.enccomb30.open$depthf <- "open ocean"

Ehux.adscomb30.open <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (group=="Cc") %>% 
                                 filter (parameter=="adscomb") %>% filter (depthf=="open ocean"), 
                               aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.adscomb30.open <- ggplot_build(Ehux.adscomb30.open)$data[[1]] %>% select (x, y)
Ehux.adscomb30.open$entity <- "E. huxleyi"
Ehux.adscomb30.open$parameter <- "adscomb"
Ehux.adscomb30.open$depthf <- "open ocean"

Ehux.infcomb30.open <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (group=="Cc")%>% 
                                 filter (parameter=="infcomb") %>% filter (depthf=="open ocean"),
                               aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Ehux.infcomb30.open <- ggplot_build(Ehux.infcomb30.open)$data[[1]] %>% select (x, y)
Ehux.infcomb30.open$entity <- "E. huxleyi"
Ehux.infcomb30.open$parameter <- "infcomb"
Ehux.infcomb30.open$depthf <- "open ocean"

##coccoliths shelf
lith.abundance30.shelf <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (group=="Li") %>% 
                                    filter (parameter=="abundance") %>% 
                                    filter (depthf=="shelf/slope"), 
                                  aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity")
lith.abundance30.shelf <- ggplot_build(lith.abundance30.shelf)$data[[1]] %>% select (x,y)
lith.abundance30.shelf$entity <- "coccolith"
lith.abundance30.shelf$parameter <- "abundance"
lith.abundance30.shelf$depthf <- "shelf/slope"

lith.enccomb30.shelf <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (group=="Li") %>% 
                                  filter (parameter=="enccomb") %>% filter (depthf=="shelf/slope"),  
                                aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
lith.enccomb30.shelf <- ggplot_build(lith.enccomb30.shelf)$data[[1]] %>% select (x,y)
lith.enccomb30.shelf$entity <- "coccolith"
lith.enccomb30.shelf$parameter <- "enccomb"
lith.enccomb30.shelf$depthf <- "shelf/slope"

lith.adscomb30.shelf <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (group=="Li") %>% 
                                  filter (parameter=="adscomb") %>% filter (depthf=="shelf/slope"), 
                                aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
lith.adscomb30.shelf <- ggplot_build(lith.adscomb30.shelf)$data[[1]] %>% select (x, y)
lith.adscomb30.shelf$entity <- "coccolith"
lith.adscomb30.shelf$parameter <- "adscomb"
lith.adscomb30.shelf$depthf <- "shelf/slope"

#coccoliths open ocean

lith.abundance30.open <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (group=="Li") %>% 
                                   filter (parameter=="abundance") %>% filter (depthf=="open ocean"), 
                                 aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity")
lith.abundance30.open <- ggplot_build(lith.abundance30.open)$data[[1]] %>% select (x, y)
lith.abundance30.open$entity <- "coccolith"
lith.abundance30.open$parameter <- "abundance"
lith.abundance30.open$depthf <- "open ocean"

lith.enccomb30.open <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (group=="Li") %>%  
                                 filter (parameter=="enccomb") %>% 
                                 filter (depthf=="open ocean"),  
                               aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
lith.enccomb30.open <- ggplot_build(lith.enccomb30.open)$data[[1]] %>% select (x,y)
lith.enccomb30.open$entity <- "coccolith"
lith.enccomb30.open$parameter <- "enccomb"
lith.enccomb30.open$depthf <- "open ocean"

lith.adscomb30.open <- ggplot (data=melted_NAdata.betas_comb_30m %>% filter (group=="Li") %>% 
                                 filter (parameter=="adscomb") %>% filter (depthf=="open ocean"), 
                               aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
lith.adscomb30.open <- ggplot_build(lith.adscomb30.open)$data[[1]] %>% select (x, y)
lith.adscomb30.open$entity <- "coccolith"
lith.adscomb30.open$parameter <- "adscomb"
lith.adscomb30.open$depthf <- "open ocean"

#binding data
density_30m <- rbind (Ehux.abundance30.shelf, Ehux.enccomb30.shelf, Ehux.adscomb30.shelf, Ehux.infcomb30.shelf, 
                      Ehux.abundance30.open, Ehux.enccomb30.open, Ehux.adscomb30.open, Ehux.infcomb30.open, 
                      lith.abundance30.shelf, lith.enccomb30.shelf, lith.adscomb30.shelf, 
                      lith.abundance30.open, lith.enccomb30.open, lith.adscomb30.open)

density_30m$entity <-  reorder.factor (density_30m$entity, new.order = c("E. huxleyi", "coccolith")) 
density_30m$parameter <-  reorder.factor (density_30m$parameter, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 

#plotting Ehux
density30m_plot_Ehux.png <- ggplot (data=density_30m %>% filter (entity=="E. huxleyi"), 
                                    aes(x=x, y=y, fill=depthf, color=depthf)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) + 
  scale_color_manual (values= c("#998ec3", "#f1a340")) + scale_fill_manual (values= c("#998ec3", "#f1a340")) + 
  geom_vline(xintercept = 0, linetype="dashed") +
  labs (x=expression(log[10]), y="probability density") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), 
        legend.position = "bottom") + 
  facet_grid(~parameter, labeller = labeller(parameter = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "density30m_plot_Ehux.png", width = 17, height = 6, dpi = 600, device="png", units="in")

#plotting Ehux
density30m_plot_lith.png <- ggplot (data=density_30m %>% filter (entity=="coccolith"), 
                                    aes(x=x, y=y, fill=depthf, color=depthf)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) + 
  scale_color_manual (values= c("#998ec3", "#f1a340")) + scale_fill_manual (values= c("#998ec3", "#f1a340")) + 
  geom_vline(xintercept = 0, linetype="dashed") +
  labs (x=expression(log[10]), y="probability density") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), 
        legend.position = "bottom") + 
  facet_grid(~parameter, labeller = labeller(parameter = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "density30m_plot_lith.png", width = 13, height = 6, dpi = 600, device="png", units="in")

##-----------------------------------------------------------------------------------------
###simulation for 1m

#split 1m 
NAdata.betas_1m = NAdata.betas %>% select(!(disrate_30m)) 
NAdata.betas_1m$beta_turb <- (4.2*pi*((NAdata.betas_1m$disrate_1m/(v))^0.5)*((NAdata.betas_1m$rad+Rehv)^3))*86400*10^6 
NAdata.betas_1m$beta_all <- NAdata.betas_1m$beta_BM + NAdata.betas_1m$beta_DS + NAdata.betas_1m$beta_turb

#join NAdata and probs
NAdata.betas_1m <- left_join(NAdata.betas_1m, probs)

#calculate propEhV
NAdata.betas_1m$propEhV <- NAdata.betas_1m$virnum* NAdata.betas_1m$propvir

#calculate encounters fast slow
NAdata.betas_1m$encounters <- NAdata.betas_1m$beta_all*NAdata.betas_1m$propEhV 

#calculate total adsorption by virus props
NAdata.betas_1m$adstot <- NAdata.betas_1m$encounters*NAdata.betas_1m$ads

#calculate total successful infections
NAdata.betas_1m$sucinf <- NAdata.betas_1m$encounters*NAdata.betas_1m$ads*NAdata.betas_1m$inf

#add everything
high_NAdata.betas_1m <- NAdata.betas_1m %>% filter (virus=="high")
low_NAdata.betas_1m <- NAdata.betas_1m %>% filter (virus=="low")

NAdata.betas_comb_1m <- NAdata.betas_1m %>% filter (virus=="high") %>% select (c(lat, lon, group, depthf, abundance, virnum, disrate_1m))
NAdata.betas_comb_1m$enccomb <- high_NAdata.betas_1m$encounters + low_NAdata.betas_1m$encounters
NAdata.betas_comb_1m$adscomb <- high_NAdata.betas_1m$adstot + low_NAdata.betas_1m$adstot
NAdata.betas_comb_1m$infcomb <- high_NAdata.betas_1m$sucinf + low_NAdata.betas_1m$sucinf

#melt dataset
melted_NAdata.betas_comb_1m <- reshape2::melt (NAdata.betas_comb_1m %>% select (lat, lon, group, depthf, abundance, enccomb, adscomb, infcomb), id.vars = c("lat", "lon", "depthf", "group"), value.name = "value", variable.name = "parameter")

melted_NAdata.betas_comb_1m$parameter <-  reorder.factor (melted_NAdata.betas_comb_1m$parameter, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 

variable_labs <- c(
  `abundance` = 'concentration~(mL^{-1})',
  `enccomb` = 'encounters~entity^{-1}~d^{-1}',
  `adscomb` = 'adsorptions~entity^{-1}~d^{-1}',
  `infcomb` = 'infections~entity^{-1}~d^{-1}'
)

melted_NAdata.betas_comb_1m$entity <- factor (melted_NAdata.betas_comb_1m$group, levels = c("Cc", "Li"), 
                                              labels= c("E. huxleyi", "coccolith"))

#plot map Ehux
all_1m_Ehux.png<- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data= melted_NAdata.betas_comb_1m %>% filter (group=="Cc"),
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
  guides(color = guide_colorbar(title.position = "right")) +  
  facet_grid(~parameter, labeller = labeller(parameter = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "all_1m_Ehux.png", width = 17, height = 6, dpi = 600, device="png", units="in")

#plot map coccolith
all_1m_lith.png<- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data= melted_NAdata.betas_comb_1m %>% filter (group=="Li") %>% filter (!parameter=="infcomb"),
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
  guides(color = guide_colorbar(title.position = "right")) +  
  facet_grid(~parameter, labeller = labeller(parameter = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "all_1m_lith.png", width = 13, height = 6, dpi = 600, device="png", units="in")


#plot probability density
#Ehux shelf
Ehux.abundance1.shelf <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (group=="Cc") %>% 
                                   filter (parameter=="abundance") %>% filter (depthf=="shelf/slope"), 
                                 aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity")
Ehux.abundance1.shelf <- ggplot_build(Ehux.abundance1.shelf)$data[[1]] %>% select (x,y)
Ehux.abundance1.shelf$entity <- "E. huxleyi"
Ehux.abundance1.shelf$parameter <- "abundance"
Ehux.abundance1.shelf$depthf <- "shelf/slope"

Ehux.enccomb1.shelf <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (group=="Cc") %>% 
                                 filter (parameter=="enccomb") %>% filter (depthf=="shelf/slope"),  
                               aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.enccomb1.shelf <- ggplot_build(Ehux.enccomb1.shelf)$data[[1]] %>% select (x,y)
Ehux.enccomb1.shelf$entity <- "E. huxleyi"
Ehux.enccomb1.shelf$parameter <- "enccomb"
Ehux.enccomb1.shelf$depthf <- "shelf/slope"

Ehux.adscomb1.shelf <- ggplot (data=melted_NAdata.betas_comb_1m  %>% filter (group=="Cc")
                               %>% filter (parameter=="adscomb") %>% filter (depthf=="shelf/slope"), 
                               aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.adscomb1.shelf <- ggplot_build(Ehux.adscomb1.shelf)$data[[1]] %>% select (x, y)
Ehux.adscomb1.shelf$entity <- "E. huxleyi"
Ehux.adscomb1.shelf$parameter <- "adscomb"
Ehux.adscomb1.shelf$depthf <- "shelf/slope"

Ehux.infcomb1.shelf <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (group=="Cc") %>% 
                                 filter (parameter=="infcomb") %>% filter (depthf=="shelf/slope"),
                               aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Ehux.infcomb1.shelf <- ggplot_build(Ehux.infcomb1.shelf)$data[[1]] %>% select (x, y)
Ehux.infcomb1.shelf$entity <- "E. huxleyi"
Ehux.infcomb1.shelf$parameter <- "infcomb"
Ehux.infcomb1.shelf$depthf <- "shelf/slope"

#Ehux open ocean
Ehux.abundance1.open <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (group=="Cc") 
                                %>% filter (parameter=="abundance") %>% filter (depthf=="open ocean"), 
                                aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity")
Ehux.abundance1.open <- ggplot_build(Ehux.abundance1.open)$data[[1]] %>% select (x, y)
Ehux.abundance1.open$entity <- "E. huxleyi"
Ehux.abundance1.open$parameter <- "abundance"
Ehux.abundance1.open$depthf <- "open ocean"

Ehux.enccomb1.open <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (group=="Cc") %>% 
                                filter (parameter=="enccomb") %>% filter (depthf=="open ocean"),  
                              aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.enccomb1.open <- ggplot_build(Ehux.enccomb1.open)$data[[1]] %>% select (x,y)
Ehux.enccomb1.open$entity <- "E. huxleyi"
Ehux.enccomb1.open$parameter <- "enccomb"
Ehux.enccomb1.open$depthf <- "open ocean"

Ehux.adscomb1.open <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (group=="Cc") %>% 
                                filter (parameter=="adscomb") %>% filter (depthf=="open ocean"), 
                              aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
Ehux.adscomb1.open <- ggplot_build(Ehux.adscomb1.open)$data[[1]] %>% select (x, y)
Ehux.adscomb1.open$entity <- "E. huxleyi"
Ehux.adscomb1.open$parameter <- "adscomb"
Ehux.adscomb1.open$depthf <- "open ocean"

Ehux.infcomb1.open <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (group=="Cc")%>% 
                                filter (parameter=="infcomb") %>% filter (depthf=="open ocean"),
                              aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 

Ehux.infcomb1.open <- ggplot_build(Ehux.infcomb1.open)$data[[1]] %>% select (x, y)
Ehux.infcomb1.open$entity <- "E. huxleyi"
Ehux.infcomb1.open$parameter <- "infcomb"
Ehux.infcomb1.open$depthf <- "open ocean"

##coccoliths shelf
lith.abundance1.shelf <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (group=="Li") %>% 
                                   filter (parameter=="abundance") %>% 
                                   filter (depthf=="shelf/slope"), 
                                 aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity")
lith.abundance1.shelf <- ggplot_build(lith.abundance1.shelf)$data[[1]] %>% select (x,y)
lith.abundance1.shelf$entity <- "coccolith"
lith.abundance1.shelf$parameter <- "abundance"
lith.abundance1.shelf$depthf <- "shelf/slope"

lith.enccomb1.shelf <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (group=="Li") %>% 
                                 filter (parameter=="enccomb") %>% filter (depthf=="shelf/slope"),  
                               aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
lith.enccomb1.shelf <- ggplot_build(lith.enccomb1.shelf)$data[[1]] %>% select (x,y)
lith.enccomb1.shelf$entity <- "coccolith"
lith.enccomb1.shelf$parameter <- "enccomb"
lith.enccomb1.shelf$depthf <- "shelf/slope"

lith.adscomb1.shelf <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (group=="Li") %>% 
                                 filter (parameter=="adscomb") %>% filter (depthf=="shelf/slope"), 
                               aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
lith.adscomb1.shelf <- ggplot_build(lith.adscomb1.shelf)$data[[1]] %>% select (x, y)
lith.adscomb1.shelf$entity <- "coccolith"
lith.adscomb1.shelf$parameter <- "adscomb"
lith.adscomb1.shelf$depthf <- "shelf/slope"

#coccoliths open ocean

lith.abundance1.open <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (group=="Li") %>% 
                                  filter (parameter=="abundance") %>% filter (depthf=="open ocean"), 
                                aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity")
lith.abundance1.open <- ggplot_build(lith.abundance1.open)$data[[1]] %>% select (x, y)
lith.abundance1.open$entity <- "coccolith"
lith.abundance1.open$parameter <- "abundance"
lith.abundance1.open$depthf <- "open ocean"

lith.enccomb1.open <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (group=="Li") %>%  
                                filter (parameter=="enccomb") %>% 
                                filter (depthf=="open ocean"),  
                              aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
lith.enccomb1.open <- ggplot_build(lith.enccomb1.open)$data[[1]] %>% select (x,y)
lith.enccomb1.open$entity <- "coccolith"
lith.enccomb1.open$parameter <- "enccomb"
lith.enccomb1.open$depthf <- "open ocean"

lith.adscomb1.open <- ggplot (data=melted_NAdata.betas_comb_1m %>% filter (group=="Li") %>% 
                                filter (parameter=="adscomb") %>% filter (depthf=="open ocean"), 
                              aes(x=log10(value))) +
  geom_histogram(binwidth = 0.05, aes (y = ..count../sum(..count..)), alpha=0.25, position="identity") 
lith.adscomb1.open <- ggplot_build(lith.adscomb1.open)$data[[1]] %>% select (x, y)
lith.adscomb1.open$entity <- "coccolith"
lith.adscomb1.open$parameter <- "adscomb"
lith.adscomb1.open$depthf <- "open ocean"

#binding data
density_1m <- rbind (Ehux.abundance1.shelf, Ehux.enccomb1.shelf, Ehux.adscomb1.shelf, Ehux.infcomb1.shelf, 
                     Ehux.abundance1.open, Ehux.enccomb1.open, Ehux.adscomb1.open, Ehux.infcomb1.open, 
                     lith.abundance1.shelf, lith.enccomb1.shelf, lith.adscomb1.shelf, 
                     lith.abundance1.open, lith.enccomb1.open, lith.adscomb1.open)

density_1m$entity <-  reorder.factor (density_1m$entity, new.order = c("E. huxleyi", "coccolith")) 
density_1m$parameter <-  reorder.factor (density_1m$parameter, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 

#plotting Ehux
density1m_plot_Ehux.png <- ggplot (data=density_1m %>% filter (entity=="E. huxleyi"), 
                                   aes(x=x, y=y, fill=depthf, color=depthf)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) + 
  scale_color_manual (values= c("#998ec3", "#f1a340")) + scale_fill_manual (values= c("#998ec3", "#f1a340")) + 
  geom_vline(xintercept = 0, linetype="dashed") +
  labs (x=expression(log[10]), y="probability density") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), 
        legend.position = "bottom") + 
  facet_grid(~parameter, labeller = labeller(parameter = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "density1m_plot_Ehux.png", width = 17, height = 6, dpi = 600, device="png", units="in")

#plotting Ehux
density1m_plot_lith.png <- ggplot (data=density_1m %>% filter (entity=="coccolith"), 
                                   aes(x=x, y=y, fill=depthf, color=depthf)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) + 
  scale_color_manual (values= c("#998ec3", "#f1a340")) + scale_fill_manual (values= c("#998ec3", "#f1a340")) + 
  geom_vline(xintercept = 0, linetype="dashed") +
  labs (x=expression(log[10]), y="probability density") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), 
        legend.position = "bottom") + 
  facet_grid(~parameter, labeller = labeller(parameter = as_labeller(variable_labs, label_parsed)))

ggsave(filename = "density1m_plot_lith.png", width = 13, height = 6, dpi = 600, device="png", units="in")


##-----------------------------------------------------------------------------------------
#get percent infected for 1m and 30m

#30m
infcomb_30m<- density_30m %>% filter (entity=="E. huxleyi") %>% filter (parameter=="infcomb")
infcomb_30m$percent <- (10^(infcomb_30m$x))*100

#percentage of oceans that have 10% lysis rate
infcomb_30m.sum <- infcomb_30m %>% mutate(grouppercent = ifelse(percent<1, "below1",  "above1")) %>%
  group_by(depthf, grouppercent) %>%
  summarise (total=sum(y*100))

resize.win (6, 6)

infcomb30m_plot.png <- ggplot (data=infcomb_30m, aes(x=log10(percent), y=y, fill=depthf, color=depthf)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) +   geom_vline(xintercept = 1, linetype="dashed") +
  scale_color_manual (values= c("#f1a340","#998ec3")) + scale_fill_manual (values= c("#f1a340","#998ec3")) + 
  scale_x_continuous(breaks = c(2, 1, 0, -1, -2, -3),label = c(100, 10, 1, 0.1, 0.01, 0.001)) +
  labs (x=expression("% population infected "~d^-1), y="probability density") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), legend.position = "bottom")

ggsave(filename = "infcomb30m_plot.png", width = 6, height = 6, dpi = 600, device="png", units="in")

##1m
infcomb_1m<- density_1m %>% filter (entity=="E. huxleyi") %>% filter (parameter=="infcomb")
infcomb_1m$percent <- (10^(infcomb_1m$x))*100

#percentage of oceans that have 10% lysis rate
infcomb_1m.sum <- infcomb_1m %>% mutate(grouppercent = ifelse(percent<1, "below1",  "above1")) %>%
  group_by(depthf, grouppercent) %>%
  summarise (total=sum(y*100))

resize.win (6, 6)

infcomb1m_plot.png <- ggplot (data=infcomb_1m, aes(x=log10(percent), y=y, fill=depthf, color=depthf)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) +   geom_vline(xintercept = 1, linetype="dashed") +
  scale_color_manual (values= c("#f1a340","#998ec3")) + scale_fill_manual (values= c("#f1a340","#998ec3")) + 
  scale_x_continuous(breaks = c(2, 1, 0, -1, -2, -3),label = c(100, 10, 1, 0.1, 0.01, 0.001)) +
  labs (x=expression("% population infected "~d^-1), y="probability density") +   coord_flip() + theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), legend.position = "bottom")

ggsave(filename = "infcomb1m_plot.png", width = 6, height = 6, dpi = 600, device="png", units="in")

##percent adsorbed
#30m
resize.win(6,6)
ggplot (data=density_30m %>% filter (entity=="coccolith") %>% filter (parameter=="adscomb"), 
        aes(x=log10((10^x)*100), y=y, fill=depthf, color=depthf)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) + 
  scale_color_manual (values= c("#f1a340","#998ec3")) + scale_fill_manual (values= c("#f1a340","#998ec3")) + 
  scale_x_continuous(breaks = c(2, 1, 0, -1, -2, -3),label = c(100, 10, 1, 0.1, 0.01, 0.001)) +
  labs (x=expression("% population with\nadsorbed EhVs"~d^-1), y="probability density") +   coord_flip() + 
  theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), 
        legend.position = "bottom", axis.title.y = element_text(vjust=0.001))

ggplot (data=density_1m %>% filter (entity=="coccolith") %>% filter (parameter=="adscomb"), 
        aes(x=log10((10^x)*100), y=y, fill=depthf, color=depthf)) +
  geom_area(position = "identity", alpha=0.3) +   geom_line(size=1) + 
  scale_color_manual (values= c("#f1a340","#998ec3")) + scale_fill_manual (values= c("#f1a340","#998ec3")) + 
  scale_x_continuous(breaks = c(2, 1, 0, -1, -2, -3),label = c(100, 10, 1, 0.1, 0.01, 0.001)) +
  labs (x=expression("% population with\nadsorbed EhVs"~d^-1), y="probability density") +   coord_flip() + 
  theme_Publication2() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(3, "lines"), 
        legend.position = "bottom", axis.title.y = element_text(vjust=0.001))

##-----------------------------------------------------------------------------------------
##whole NA infection

coords <- read.csv("D:/Users/karengb/CSV Files/coords.txt", sep=";")

#1m
NA_infection_coords_1m <-ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+
  geom_point(data= melted_NAdata.betas_comb_1m %>% filter(parameter=="infcomb") %>% filter (group=="Cc"),
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
  guides(color = guide_colorbar(title.position = "right")) +
  geom_point(data=coords, aes(x=lon, y=lat), color="black", size=2)

ggsave(filename = "NA_infection_coords_1m.png", width = 7.5, height = 5, dpi = 600, device="png", units="in")

#30m
NA_infection_coords_30m <-ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+
  geom_point(data= melted_NAdata.betas_comb_30m %>% filter(parameter=="infcomb") %>% filter (group=="Cc"),
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
  guides(color = guide_colorbar(title.position = "right")) +
  geom_point(data=coords, aes(x=lon, y=lat), color="black", size=2)

ggsave(filename = "NA_infection_coords_30m.png", width = 7.5, height = 5, dpi = 600, device="png", units="in")

##-----------------------------------------------------------------------------------------
#navice slice

##filter NA-VICE sites 30m
navice_30m <- melted_NAdata.betas_comb_30m %>% filter (between (lat, 50, 70)) %>% filter (between (lon, -40, -20))

naviceslice_coords_origcolor_percent30m.png <- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-40, -20), ylim = c(50, 70 ) , expand = FALSE) +
  geom_point(data= navice_30m %>% filter (parameter=="infcomb") %>% filter (group=="Cc"),  
             aes(x = lon, y = lat, color = log10(value*100)))+
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

ggsave(filename = "naviceslice_coords_origcolor_percent30m.png", width = 5, height = 5, dpi = 600, device="png", units="in")

##filter NA-VICE sites 1m
navice_1m <- melted_NAdata.betas_comb_1m %>% filter (between (lat, 50, 70)) %>% filter (between (lon, -40, -20))

naviceslice_coords_origcolor_percent1m.png <- ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-40, -20), ylim = c(50, 70 ) , expand = FALSE) +
  geom_point(data= navice_1m %>% filter (parameter=="infcomb") %>% filter (group=="Cc"),  
             aes(x = lon, y = lat, color = log10(value*100)))+
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

ggsave(filename = "naviceslice_coords_origcolor_percent1m.png", width = 5, height = 5, dpi = 600, device="png", units="in")
