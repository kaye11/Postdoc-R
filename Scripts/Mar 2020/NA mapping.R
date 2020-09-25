#hoping this wouldn't break R attempt lol
source("inspack.R")

#toby's data
NA_abundancedata<- read.csv("D:/Postdoc/theoretical/NA simulation data/cocco_work4Karen/cocco_work4Karen_withrownames.txt", sep="")

#filter data out
NA_abundancedata <- NA_abundancedata %>% filter (!(waterdepth_m < 10)) 

#melt data (wide to long)
melted_NAabundancedata <- reshape2::melt (NA_abundancedata, id.vars = c("lat", "lon", "waterdepth_m"), value.name = "abundance", variable.name = "entity")

melted_NAabundancedata <- melted_NAabundancedata %>% filter (!(abundance < 1)) 

##histograms
resize.win(9,6)
ggplot (data=melted_NAabundancedata %>% filter (entity %in% c("Ncells_max", "Ncells_min")), aes(x=log10(abundance))) +
  geom_histogram(binwidth = 0.1, aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + theme_Publication() + labs (x="log10 abundance per mL")

ggplot (data=melted_NAabundancedata %>% filter (entity %in% c("Nliths_max", "Nliths_min")), aes(x=log10(abundance))) +
  geom_histogram(binwidth = 0.1, aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="darkblue") + theme_Publication() + labs (x="log10 abundance per mL")

ggplot (data=melted_NAabundancedata %>% filter (entity %in% c("Nliths_max", "Nliths_min"))  %>% 
          filter (!(abundance < 10)), aes(x=log10(abundance))) +
  geom_histogram(binwidth = 0.1, aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="darkblue") + theme_Publication() + labs (x="log10 abundance per mL")

#copy Toby's histograms
#cells
#histogram
ggplot (data=melted_NAabundancedata %>% filter (entity %in% c("Ncells_max", "Ncells_min"))%>% filter (waterdepth_m<200), aes(x=log10(abundance))) + geom_histogram(binwidth = 0.1, colour="black", fill="darkblue", alpha=0.2, position = "stack") +
  geom_histogram(data=melted_NAabundancedata %>% filter (entity %in% c("Ncells_max", "Ncells_min"))%>% filter (waterdepth_m>200), aes(x=log10 (abundance)), binwidth=0.1, colour="black", fill="orange", alpha=0.2, position="stack") +
  
#density plots
resize.win (6,5)

#removed less than 10 abundance or less than 1. change accordingly
ggplot (data=melted_NAabundancedata %>% filter (entity %in% c("Ncells_max", "Ncells_min"))%>% filter (waterdepth_m<200) %>% 
          filter (!(abundance < 1)), aes(x=log10(abundance))) + 
  geom_density(position = "stack", aes(fill="<200 m"), alpha=0.2) +
  geom_density(data=melted_NAabundancedata %>% filter (entity %in% c("Ncells_max", "Ncells_min"))%>% 
                 filter (waterdepth_m>200) %>% filter (!(abundance < 10)), 
               aes(x=log10 (abundance), fill=">200 m"), position="stack", alpha=0.2 ) + 
  theme_Publication2() + labs (x= "log10 abundance per mL", title ="Cc") +  theme(legend.title = element_blank())

#liths
ggplot (data=melted_NAabundancedata %>% filter (entity %in% c("Nliths_max", "Nliths_min"))%>% filter (waterdepth_m<200) %>% 
          filter (!(abundance < 10)), aes(x=log10(abundance))) +   
  geom_density(position = "stack", aes(fill="<200 m"), alpha=0.2) +
  geom_density(data=melted_NAabundancedata %>% filter (entity %in% c("Nliths_max", "Nliths_min"))%>% 
                 filter (waterdepth_m>200) %>% filter (!(abundance < 10)),                                                        aes(x=log10 (abundance), fill=">200 m"), position="stack", alpha=0.2 ) + theme_Publication2() + 
  labs (x= "log10 abundance per mL", title ="Li") +  theme(legend.title = element_blank())

#what if you remove all abundance below 10??


#one mapping method
ggplot(data = world)+ geom_sf()+ coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ geom_point(data=melted_NAabundancedata %>%  filter(entity %in% c("Ncells_max", "Nliths_max")), aes(x = lon, y = lat, color = log10(abundance)))+ scale_color_viridis() + labs(x = NULL)+  theme_Publication() + theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.height=unit(1,"line"), legend.key.width = unit (2, "line")) + facet_grid(~entity)

#fastermethod
resize.win(12,6)
p.strip <- list(cex=1.2, lines=1.2, fontface='bold')
#max, cells <10 are rmoved 
levelplot(log10(abundance) ~ lon * lat | entity, data=melted_NAabundancedata %>%  
            filter(entity %in% c("Ncells_max", "Nliths_max"))  %>% 
            filter (!(abundance < 10)), #remove this filter
          pretty=TRUE, col.regions = viridis(256, begin=1, end=0), cuts=7,
          region=TRUE, par.strip.text=p.strip,
          scales=list(x=list(cex=1.2),y=list(cex=1.2)), xlab=list(cex=1.2), ylab=list(cex=1.2), 
          strip=strip.custom(factor.levels=c("E. huxleyi", "Free coccoliths"),strip.levels=rep(TRUE,2)))
#min
levelplot(log10(abundance) ~ lon * lat | entity, data=melted_NAabundancedata  %>%  
            filter(entity %in% c("Ncells_min", "Nliths_min")),
          pretty=TRUE, col.regions = viridis(256, begin=1, end=0),
          region=TRUE, par.strip.text=p.strip,
          scales=list(x=list(cex=1.2),y=list(cex=1.2)), xlab=list(cex=1.2), ylab=list(cex=1.2), 
          strip=strip.custom(factor.levels=c("E. huxleyi", "Free coccoliths"),strip.levels=rep(TRUE,2)))

resize.win(18,6)
levelplot(log10(abundance) ~ lon * lat | entity, data=melted_NAabundancedata, pretty=TRUE, 
          col.regions = viridis(256, begin=1, end=0),
          region=TRUE, par.strip.text=p.strip,
          scales=list(x=list(cex=1.2),y=list(cex=1.2)), xlab=list(cex=1.2), ylab=list(cex=1.2), 
          strip=strip.custom(factor.levels=c("E. huxleyi min", "E. huxleyi max", "Free coccoliths min", "Free coccoliths max"),strip.levels=rep(TRUE,4)))

##try the data set
#get the max data
betas <- read.csv("D:/R program/Postdoc-R/CSV Files/betas all calmstormy.csv")
NAdata <- melted_NAabundancedata %>% filter (entity %in% c("Ncells_max", "Nliths_max")) 

NAdata$entity <- factor (NAdata$entity,levels= c("Ncells_max", "Nliths_max"), labels = c("Cc", "Li"))
NAdata$virnum <- NAdata$abundance*10

betas$beta_all <- betas$beta_BM + betas$beta_DS + betas$beta_turb

#join betas and NAdata
NAdata.betas <- left_join(NAdata, betas)

#probabilities
probs <- as.data.frame(list (entity = as.factor (rep(c("Cc","Li"), 2)), virus = rep(c("high", "low"), 1, each=8), propvir= rep(c(0.33, 0.67), 1, each=8), ads = rep(c(0.0130, 0.0226), 4), inf = rep(c(0.3, NA,0.3, NA, 0.3, NA, 0.3, NA, 0.06, NA, 0.06, NA, 0.06, NA, 0.06, NA), 1, each=1)))

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

#try plotting everything in one go
NAdata.betas$vircon <-as.factor(paste(NAdata.betas$virus, NAdata.betas$watcon, sep="-"))

levelplot(log10(encounters_propEhV) ~ lon * lat | vircon, data=NAdata.betas %>% filter (entity=="Cc"), pretty=TRUE,
          col.regions = viridis(256, begin=1, end=0.5),
          region=TRUE, par.strip.text=p.strip,
          scales=list(x=list(cex=1.2),y=list(cex=1.2)), xlab=list(cex=1.2), ylab=list(cex=1.2))

#add everything
high_NAdata.betas <- NAdata.betas %>% filter (virus=="high")
low_NAdata.betas <- NAdata.betas %>% filter (virus=="low")

NAdata.betas_comb <- NAdata.betas %>% filter (virus=="high") %>% select (c(lat, lon, entity, watcon, abundance, virnum))
NAdata.betas_comb$enccomb <- high_NAdata.betas$encounters_propEhV + low_NAdata.betas$encounters_propEhV
NAdata.betas_comb$adscomb <- high_NAdata.betas$adstot_prop + low_NAdata.betas$adstot_prop
NAdata.betas_comb$infcomb <- high_NAdata.betas$sucinf_prop + low_NAdata.betas$sucinf_prop

##percentage of parameters
NAdata.betas_comb$perencounters <- (NAdata.betas_comb$enccomb/NAdata.betas_comb$abundance)*100
NAdata.betas_comb$peradsorbed <- (NAdata.betas_comb$adscomb/NAdata.betas_comb$abundance)*100
NAdata.betas_comb$perinf <- (NAdata.betas_comb$infcomb/NAdata.betas_comb$abundance)*100
NAdata.betas_comb <- NAdata.betas_comb %>% mutate(perencounters= if_else(perencounters > 1, 1, perencounters)) %>% mutate(peradsorbed= if_else(perencounters > 1, 1, peradsorbed)) %>% mutate(perinf= if_else(perinf > 1, 1, perinf)) 

##NAdata.betas_comb is the summary

#plotting combined high and low, with or without log+1
NAdata.betas_comb$entcon <- as.factor (paste (NAdata.betas_comb$entity, NAdata.betas_comb$watcon, sep="-"))

#for incomb, facet only by watcon, if watcon resize to (12,6), facet to entcon for others
#%>% filter (entity=="Cc")
resize.win(12,6)
levelplot(log10(infcomb) ~ lon * lat | watcon, data=NAdata.betas_comb %>% 
            filter (!(abundance < 10)) %>% filter (entity=="Cc"), #delete this if you want the whole data
          pretty=TRUE, 
          col.regions = viridis(256, begin=1, end=0), cuts=7, 
          region=TRUE, par.strip.text=p.strip,
          scales=list(x=list(cex=1.2),y=list(cex=1.2)), xlab=list(cex=1.2), ylab=list(cex=1.2))

##check summaries
enccomb.sum <- summarySE (NAdata.betas_comb, measurevar = "enccomb", groupvars=c("entity", "watcon"))
adscomb.sum <- summarySE (NAdata.betas_comb, measurevar = "adscomb", groupvars=c("entity", "watcon"))
infcomb.sum <- summarySE (NAdata.betas_comb, measurevar = "infcomb", groupvars=c("entity", "watcon"))

#melt dataset
melted_NAdata.betas_comb <- reshape2::melt (NAdata.betas_comb %>% select (lat, lon, entity, watcon, abundance, enccomb, adscomb, infcomb, entcon), id.vars = c("lat", "lon", "entity", "watcon", "entcon"), value.name = "value", variable.name = "step")
melted_NAdata.betas_comb$entconstep <- as.factor (paste(melted_NAdata.betas_comb$entcon, melted_NAdata.betas_comb$step, sep="-"))

melted_NAdata.betas_comb$step <-  reorder.factor (melted_NAdata.betas_comb$step, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 

melted_NAdata.betas_comb$step2<- factor (melted_NAdata.betas_comb$step, levels= c("abundance", "enccomb", "adscomb", "infcomb"), labels = c("entity abundance\n mL-1", "total encounters\n day-1 mL-1", "total adsorbed\n EhVs day-1 mL-1", "total successful\n infection day-1 mL-1"))

melted_NAdata.betas_comb$entity <-  reorder.factor (melted_NAdata.betas_comb$entity, new.order = c("Li", "Cc")) 

resize.win(36,24)
q.strip <- list(cex=1.2, lines=2.5, fontface='bold')

#change watcon
levelplot(log10(value) ~ lon * lat | step2*entity, data=melted_NAdata.betas_comb %>% filter (watcon=="calm"), pretty=TRUE, 
          col.regions = viridis(256, begin=1, end=0), cuts=7,
          region=TRUE, par.strip.text=q.strip,
          scales=list(x=list(cex=1.2),y=list(cex=1.2)), xlab=list(cex=1.2), ylab=list(cex=1.2))

all.sum <- summarySE (melted_NAdata.betas_comb, measurevar = "value", groupvars=c("entity", "watcon", "step", 'step2'))

write.table (all.sum, "Postdoc-R/Exported Tables/NAsimulation_calmstormy_sum.csv", sep=",", col.names=T, row.names=F)

#make a geom_point with se or sd

resize.win(18,6)
ggplot(data=all.sum, aes(x=entity, y=log10(value), colour=entity, shape=entity)) + geom_point(size=5) +
  geom_errorbar(aes(ymin=log10(value-se), ymax=log10(value+se))) + 
  scale_color_manual (values=c("#377eb8", "#4daf4a")) +  
  theme_Publication() + theme (legend.title = element_blank(), axis.title.x = element_blank(), legend.position = "none") +
  labs (y="log10") + geom_hline(yintercept = log10(1), linetype="dashed") + facet_grid (watcon~step2)


#---------------------
#melt dataset and remove 10m
melted_NAdata.betas_comb_less10rem <- reshape2::melt (NAdata.betas_comb %>% select (lat, lon, entity, watcon, abundance, enccomb, adscomb, infcomb, entcon) %>% filter (!(abundance < 10)), id.vars = c("lat", "lon", "entity", "watcon", "entcon"), value.name = "value", variable.name = "step")
melted_NAdata.betas_comb_less10rem$entconstep <- as.factor (paste(melted_NAdata.betas_comb_less10rem$entcon, melted_NAdata.betas_comb_less10rem$step, sep="-"))

melted_NAdata.betas_comb_less10rem$step <-  reorder.factor (melted_NAdata.betas_comb_less10rem$step, new.order = c("abundance", 'enccomb', "adscomb", "infcomb")) 

melted_NAdata.betas_comb_less10rem$step2<- factor (melted_NAdata.betas_comb_less10rem$step, levels= c("abundance", "enccomb", "adscomb", "infcomb"), labels = c("abundance mL-1", "encounters\n day-1 mL-1", "with adsorbed EhVs\n day-1 mL-1", "successful infections\n day-1 mL-1"))

melted_NAdata.betas_comb_less10rem$watcon <-  reorder.factor (melted_NAdata.betas_comb_less10rem$watcon, new.order = c("stormy", "calm")) 

resize.win(36,24)
#change watcon
#use the whole window

levelplot(log10(value) ~ lon * lat | step2*watcon, data=melted_NAdata.betas_comb_less10rem %>% filter (entity=="Cc"), pretty=TRUE, 
          col.regions = viridis(256, begin=1, end=0), cuts=10,
          region=TRUE, par.strip.text=q.strip,
          scales=list(x=list(cex=1.2),y=list(cex=1.2)), xlab=list(cex=1.2), ylab=list(cex=1.2))

resize.win(27,24)
levelplot(log10(value) ~ lon * lat | step2*watcon, data=melted_NAdata.betas_comb_less10rem %>% filter (entity=="Li") %>%
            filter (!(step=="infcomb")), pretty=TRUE, 
          col.regions = viridis(256, begin=1, end=0), cuts=12,
          region=TRUE, par.strip.text=q.strip,
          scales=list(x=list(cex=1.2),y=list(cex=1.2)), xlab=list(cex=1.2), ylab=list(cex=1.2))

all.sum.less10rem <- summarySE (melted_NAdata.betas_comb, measurevar = "value", groupvars=c("entity", "watcon", "step", 'step2'))

write.table (all.sum.less10rem, "Postdoc-R/Exported Tables/NAsimulation_calmstormy_sum_less10rem.csv", sep=",", col.names=T, row.names=F)





