#hoping this wouldn't break R attempt lol
source("inspack_map.R")

#toby's data
NA_abundancedata<- read.csv("D:/Postdoc/theoretical/NA simulation data/cocco_work4Karen/cocco_work4Karen_withrownames.txt", sep="")

#filter data out
NA_abundancedata <- NA_abundancedata %>% filter (!(waterdepth_m < 100))  %>%  filter (!(waterdepth_m == "NA"))
NA_abundancedata$depthf <- if_else(NA_abundancedata$waterdepth_m <200, "<200 m", ">200 m")
#NA_abundancedata$depthf100 <- if_else(NA_abundancedata$waterdepth_m<100, "<100 m", ">100 m")

#melt data (wide to long)
melted_NAabundancedata <- reshape2::melt (NA_abundancedata, id.vars = c("lat", "lon", "waterdepth_m", "depthf"), value.name = "abundance", variable.name = "entity")

melted_NAabundancedata <- melted_NAabundancedata %>% filter (!(abundance < 10)) %>%  filter (!(abundance == "NA"))

melted_NAabundancedata$entitycode <- melted_NAabundancedata$entity

melted_NAabundancedata$entity <- case_when(
  melted_NAabundancedata$entitycode == "Ncells_min"  ~ "Cc",
  melted_NAabundancedata$entitycode == "Ncells_max"  ~ "Cc",
  melted_NAabundancedata$entitycode == "Nliths_min"  ~ "Li",
  melted_NAabundancedata$entitycode == "Nliths_max"  ~ "Li",
  TRUE ~ as.character(melted_NAabundancedata$entity)
)

#density plots
resize.win (10, 5)

#200
ggplot (data=melted_NAabundancedata, aes(x=log10(abundance), color=depthf, fill=depthf)) + 
  geom_density(position = "stack", alpha=0.2) +
  theme_Publication2() + labs (x = expression("log abundance"~mL^-1)) +  theme(legend.title = element_blank()) + 
  facet_grid(~entity)

#all data
resize.win (5,5)
ggplot (data=melted_NAabundancedata, aes(x=log10(abundance), color=entity, fill=entity)) + 
  geom_density(position = "stack", alpha=0.2) +
  theme_Publication2() + labs (x = expression("log abundance"~mL^-1)) +  theme(legend.title = element_blank()) 

#histogram
ggplot (data=melted_NAabundancedata, aes(x=log10(abundance), color=entity, fill=entity)) +
  geom_histogram(binwidth = 0.1, aes(y=..density..), colour="black", position="stack" ) +
  geom_density(alpha=.2, positio="stack") + theme_Publication() + labs (x = expression("log abundance"~mL^-1)) +  theme(legend.title = element_blank()) 

##mapping
resize.win(12,6)
p.strip <- list(cex=1.2, lines=1.2, fontface='bold')

#max abundance
levelplot(log10(abundance) ~ lon * lat | entity, data=melted_NAabundancedata %>%  
            filter(entitycode %in% c("Ncells_max", "Nliths_max")),  
          pretty=TRUE, col.regions = viridis(256, begin=0, end=1), cuts=10,
          region=TRUE, par.strip.text=p.strip,
          scales=list(x=list(cex=1.2),y=list(cex=1.2)), xlab=list(cex=1.2), ylab=list(cex=1.2), 
          strip=strip.custom(factor.levels=c("E. huxleyi", "Free coccoliths"),strip.levels=rep(TRUE,2)))

#ggplot version
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=melted_NAabundancedata %>%  filter(entitycode %in% c("Ncells_max", "Nliths_max")), 
             aes(x = lon, y = lat, color = log10(abundance)))+ scale_color_viridis(breaks = c(seq(0, 7, 1))) + 
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (2, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank()) + facet_grid(~entity)

##try the data set
#get the max data
betas <- read.csv("D:/R program/Postdoc-R/CSV Files/betas all calmstormy.csv")
NAdata <- melted_NAabundancedata %>% filter (entitycode %in% c("Ncells_max", "Nliths_max")) 

NAdata$virnum <- NAdata$abundance*10

betas$beta_all <- betas$beta_BM + betas$beta_DS + betas$beta_turb

#join betas and NAdata
NAdata.betas <- left_join(NAdata, betas)

#probabilities
probs <- as.data.frame(list (entity = as.factor (rep(c("Cc","Li"), 2)), virus = rep(c("high", "low"), 1, each=8), propvir= rep(c(0.33, 0.67), 1, each=8), ads = rep(c(0.0169, 0.0293), 4), inf = rep(c(0.3, NA,0.3, NA, 0.3, NA, 0.3, NA, 0.06, NA, 0.06, NA, 0.06, NA, 0.06, NA), 1, each=1)))

NAdata.betas$entity <- as.factor(NAdata.betas$entity)

#NAdata_backup <- NAdata.betas
#NAdata.betas <- NAdata_backup

#join NAdata and probs
NAdata.betas <- left_join(NAdata.betas, probs)

#calculate propEhV
NAdata.betas$propEhV <- NAdata.betas$virnum* NAdata.betas$propvir

#calculate encounters fast slow
NAdata.betas$encounters_propEhV <- NAdata.betas$beta_all*NAdata.betas$propEhV*NAdata.betas$abundance #total enc

#calculate total adsorption by virus props
NAdata.betas$adstot_prop <- NAdata.betas$encounters_propEhV*NAdata.betas$ads

#calculate total successful infections
NAdata.betas$sucinf_prop <- NAdata.betas$encounters_propEhV*NAdata.betas$ads*NAdata.betas$inf

#add everything
high_NAdata.betas <- NAdata.betas %>% filter (virus=="high")
low_NAdata.betas <- NAdata.betas %>% filter (virus=="low")

NAdata.betas_comb <- NAdata.betas %>% filter (virus=="high") %>% select (c(lat, lon, entity, watcon, depthf, abundance, virnum))
NAdata.betas_comb$enccomb <- high_NAdata.betas$encounters_propEhV + low_NAdata.betas$encounters_propEhV
NAdata.betas_comb$adscomb <- high_NAdata.betas$adstot_prop + low_NAdata.betas$adstot_prop
NAdata.betas_comb$infcomb <- high_NAdata.betas$sucinf_prop + low_NAdata.betas$sucinf_prop
#NAdata.betas_comb <- NAdata.betas_comb %>% mutate(adscomb2= if_else(adscomb > enccomb, adscomb, adscomb))

##percentage of parameters
NAdata.betas_comb$perencounters <- (NAdata.betas_comb$enccomb/NAdata.betas_comb$abundance)*100
NAdata.betas_comb$peradsorbed <- (NAdata.betas_comb$adscomb/NAdata.betas_comb$abundance)*100
NAdata.betas_comb$perinf <- (NAdata.betas_comb$infcomb/NAdata.betas_comb$abundance)*100
NAdata.betas_comb <- NAdata.betas_comb %>% mutate(perencounters= if_else(perencounters > 1, 1, perencounters)) %>% mutate(peradsorbed= if_else(perencounters > 1, 1, peradsorbed)) %>% mutate(perinf= if_else(perinf > 1, 1, perinf)) 

##NAdata.betas_comb is the summary

#plotting combined high and low, with or without log+1
NAdata.betas_comb$entcon <- as.factor (paste (NAdata.betas_comb$entity, NAdata.betas_comb$watcon, sep="-"))

##check summaries
enccomb.sum <- summarySE (NAdata.betas_comb, measurevar = "enccomb", groupvars=c("entity", "watcon"))
adscomb.sum <- summarySE (NAdata.betas_comb, measurevar = "adscomb", groupvars=c("entity", "watcon"))
infcomb.sum <- summarySE (NAdata.betas_comb, measurevar = "infcomb", groupvars=c("entity", "watcon"))

#melt dataset
melted_NAdata.betas_comb <- reshape2::melt (NAdata.betas_comb %>% select (lat, lon, entity, watcon, depthf, abundance, enccomb, adscomb, infcomb, entcon), id.vars = c("lat", "lon", "depthf", "entity", "watcon", "entcon"), value.name = "value", variable.name = "step")
melted_NAdata.betas_comb$entconstep <- as.factor (paste(melted_NAdata.betas_comb$entcon, melted_NAdata.betas_comb$step, sep="-"))

melted_NAdata.betas_comb$step <-  reorder.factor (melted_NAdata.betas_comb$step, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 

melted_NAdata.betas_comb$step2<- factor (melted_NAdata.betas_comb$step, levels= c("abundance", "enccomb", "adscomb", "infcomb"), labels = c("abundance mL-1", "encounters day-1 mL-1", "with adsorbed EhVs\n day-1 mL-1", "lytic infections\n day-1 mL-1"))

melted_NAdata.betas_comb$entity <-  reorder.factor (melted_NAdata.betas_comb$entity, new.order = c("Li", "Cc")) 

resize.win(36,24)
q.strip <- list(cex=1.2, lines=2.5, fontface='bold')

levelplot(log10(value) ~ lon * lat | step2*watcon, data=melted_NAdata.betas_comb %>% filter (entity=="Cc"), pretty=TRUE, 
          col.regions = viridis(256, begin=0, end=1), cuts=10,
          region=TRUE, par.strip.text=q.strip,
          scales=list(x=list(cex=1.2),y=list(cex=1.2)), xlab=list(cex=1.2), ylab=list(cex=1.2))

resize.win(27,24)
levelplot(log10(value) ~ lon * lat | step2*watcon, data=melted_NAdata.betas_comb %>% filter (entity=="Li") %>%
            filter (!(step=="infcomb")), pretty=TRUE, 
          col.regions = viridis(256, begin=0, end=1), cuts=12,
          region=TRUE, par.strip.text=q.strip,
          scales=list(x=list(cex=1.2),y=list(cex=1.2)), xlab=list(cex=1.2), ylab=list(cex=1.2))

all.sum.withdepth <- summarySE (melted_NAdata.betas_comb, measurevar = "value", groupvars=c("entity", "watcon", "depthf", "step", 'step2'))

all.sum <- summarySE (melted_NAdata.betas_comb, measurevar = "value", groupvars=c("entity", "watcon", "step", 'step2'))

write.table (all.sum, "Postdoc-R/Exported Tables/NAsimulation_calmstormy_sum_masked100.csv", sep=",", col.names=T, row.names=F)
write.table (all.sum.withdepth, "Postdoc-R/Exported Tables/NAsimulation_calmstormy_sum__withdepth_masked100.csv", sep=",", col.names=T, row.names=F)


#ggplot attempt
#use all windows
ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=melted_NAdata.betas_comb %>% filter (entity=="Cc"), aes(x = lon, y = lat, color = log10(value)))+
  scale_color_viridis(breaks = c(seq(-5, 9, 1))) + expand_limits(colour = c(seq(-5, 9, 1))) + theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (8, "line"), legend.key.width = unit (2, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank()) + facet_grid(watcon~step2) 
#no cores 10min
#12 cores 11 min

ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=melted_NAdata.betas_comb %>% filter (entity=="Li") %>% filter (!(step=="infcomb")), 
             aes(x = lon, y = lat, color = log10(value)))+
  scale_color_viridis(breaks = c(seq(-5, 9, 1))) + expand_limits(colour = c(seq(-5, 9, 1))) + 
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (8, "line"), legend.key.width = unit (2, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank()) + facet_grid(watcon~step2) 

#plot results as like how you did it on NAVICE data
all.sum$entity <-  reorder.factor (all.sum$entity, new.order = c("Cc", "Li")) 

ggplot(data=all.sum, aes(x=entity, y=log10(value), colour=entity, shape=entity)) + geom_point(size=5) +
  geom_errorbar(aes(ymin=log10(value-se), ymax=log10(value+se), width=0.2)) + 
  scale_color_manual (values=c("#377eb8", "#4daf4a")) +  
  theme_Publication() + theme (legend.title = element_blank(), axis.title = element_blank(), legend.position = "none") +
  labs (y="log10") + geom_hline(yintercept = log10(1), linetype="dashed") + facet_grid (watcon~step2)

melted_NAdata.betas_comb$entity <-  reorder.factor (melted_NAdata.betas_comb$entity, new.order = c("Cc", "Li")) 

#make a plot like NA-VICE but density plot
resize.win(10.5,7)
ggplot (data=melted_NAdata.betas_comb, aes(x=log10(value), color=entity, fill=entity)) + 
  geom_density(position = "stack", alpha=0.2) + scale_color_manual (values=c("#377eb8", "#4daf4a")) + 
  scale_fill_manual (values=c("#377eb8", "#4daf4a")) + geom_vline(xintercept = log10(1), linetype="dashed") +
  scale_x_continuous(breaks=seq(-6, 10, 3)) +
  labs (x="log10") +   coord_flip() + theme_Publication() +  
  theme(legend.title = element_blank(), legend.key.width = unit (2, "line"), panel.spacing = unit(1.25, "lines")) + facet_grid(watcon~step2)

##filter NA-VICE sites
navice <- melted_NAdata.betas_comb %>% filter (between (lat, 53, 62)) %>% filter (between (lon, -34, -30))


#navice lattice
resize.win(16,12)
levelplot(log10(value) ~ lon * lat | step2*watcon, data=navice %>% filter (entity=="Cc"), pretty=TRUE, 
          col.regions = viridis(256, begin=0, end=1), cuts=10,
          region=TRUE, par.strip.text=q.strip,
          scales=list(x=list(cex=1.2),y=list(cex=1.2)), xlab=list(cex=1.2), ylab=list(cex=1.2))

resize.win (12,12)
levelplot(log10(value) ~ lon * lat | step2*watcon, data=navice %>% filter (entity=="Li") %>%
            filter (!(step=="infcomb")), pretty=TRUE, 
          col.regions = viridis(256, begin=0, end=1), cuts=12,
          region=TRUE, par.strip.text=q.strip,
          scales=list(x=list(cex=1.2),y=list(cex=1.2)), xlab=list(cex=1.2), ylab=list(cex=1.2))


##ggplot2    
resize.win(12,18)
ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-34, -30), ylim = c(53, 62) , expand = FALSE)+  geom_point(data=navice %>% filter (entity=="Li") %>% filter (!(step=="infcomb")), 
             aes(x = lon, y = lat, color = log10(value)), na.rm=TRUE)+
  scale_color_viridis(breaks = c(seq(-4, 6, 2))) + expand_limits(colour = c(seq(-6, 6, 2))) +
  scale_x_continuous(breaks = seq(-34, -30, by = 2)) +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (5, "line"), legend.key.width = unit (2, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(), plot.margin = margin(0, -20, 0, -20, "cm"), axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_grid(watcon~step)

resize.win(16,18)
ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-34, -30), ylim = c(53, 62) , expand = FALSE)+  geom_point(data=navice %>% filter (entity=="Cc"), aes(x = lon, y = lat, color = log10(value)), na.rm=TRUE)+
  scale_color_viridis(breaks = c(seq(-4, 6, 2))) + expand_limits(colour = c(seq(-6, 6, 2))) +
  scale_x_continuous(breaks = seq(-34, -30, by = 2)) +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (5, "line"), legend.key.width = unit (2, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(), plot.margin = margin(0, -20, 0, -20, "cm"), axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_grid(watcon~step)


# create graphing function
entity.graph <- function(df, na.rm = TRUE, ...){
  
  # create list of counties in data to loop over 
  entity_list <- unique(df$entity)
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(entity_list)) { 
    
    # create plot for each county in df 
    plot <- 
      ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-34, -30), ylim = c(53, 62) , expand = FALSE)+  geom_point(subset(df, df$entity==entity_list[i]), mapping=aes(x = lon, y = lat, color = log10(value)))+
      facet_grid (watcon~step2) + theme_Publication2() +
      ggtitle(paste(entity_list[i]))
    ggsave(filename = paste0(i, ".png"))
  }
}




