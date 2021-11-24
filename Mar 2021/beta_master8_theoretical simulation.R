

## ------------------------------------------------------------------------
#theroretical simulation with calm, moderate, and stormy conditions. Beta kernels were previously computed (beta_master8.R).
source("inspack.R")

#get data frame
all <- read.csv("D:/R program/Postdoc-R/Exported Tables/beta_master8_allbetas.csv", sep=";")

#merge 3 disrates (10e-3, 10e-5, 10e-8)
calm <- all %>% filter (disrate %in% c ("1e-08")) %>% select (group, beta_BM, beta_DS, disrate, beta_turb, beta_all)
calm$watcon <- "calm"

stormy <- all %>% filter (disrate %in% c ("0.001")) %>% select (group, beta_BM, beta_DS, disrate, beta_turb, beta_all)
stormy$watcon <- "stormy"

moderate <- all %>% filter (disrate %in% c ("1e-05")) %>% select (group, beta_BM, beta_DS, disrate, beta_turb, beta_all)
moderate$watcon <- "moderate"

calmstormy <- rbind (calm, stormy, moderate)

##probabilities check alpha v5 final
probs <- as.data.frame(list (group = as.factor (rep(c("Cc", "Li"), 4)), virus = rep(c("high", "low"), 1, each=4), condition=rep(c("open ocean", "shelf/slope"), 2, each=2), hostnum = rep(c(10^3, 10^3, 10^5, 10^5), 1),  virnum = rep(c(((10^3)*7), ((10^5)*7)), 2, each=2), prophost = rep(c(1, 26), 4), propvir= rep(c(0.33, 0.67), 1, each=4), ads = rep(c(0.63, 0.63), 4), inf = rep(c(0.3, NA,0.3, NA, 0.06, NA, 0.06, NA))))

#join calmstormy and probs
calmstormy <- left_join(calmstormy, probs)

#calculate propEhV
calmstormy$propEhV <- calmstormy$virnum* calmstormy$propvir

#calculate prophost
calmstormy$prophostnum <- calmstormy$hostnum* calmstormy$prophost

#calculate encounters fast slow
calmstormy$encounters <- calmstormy$beta_all*calmstormy$propEhV #encounters per entity per day

#calculate total adsorption by virus props
calmstormy$adstot <- calmstormy$encounters*calmstormy$ads

#calculate total successful infections
calmstormy$sucinf <- calmstormy$encounters*calmstormy$ads*calmstormy$inf

write.table(calmstormy, "Postdoc-R/Exported Tables/calmstormy_master8.csv", sep=";", col.names=T, row.names=F)

#make individual data table for getting the distilled plot
conc <- calmstormy[1:8, ] %>% select (group, virus, condition, prophostnum, propEhV)
conc$parameter="concentration"
conc$calm <- NA
conc$stormy <- NA
conc$moderate <-conc$prophostnum

enc <- reshape(calmstormy %>% select(group, watcon, virus, condition, prophostnum, propEhV, encounters), idvar=c("group", "virus", "condition", "prophostnum", "propEhV"), timevar="watcon", direction="wide")
enc$parameter= "encounters"
ads <- reshape(calmstormy %>% select(group, watcon, virus, condition, prophostnum, propEhV, adstot), idvar=c("group", "virus", "condition", "prophostnum", "propEhV"), timevar="watcon", direction="wide")
ads$parameter= "adsorption"
inf <- reshape(calmstormy %>% select(group, watcon, virus, condition, prophostnum, propEhV, sucinf), idvar=c("group", "virus", "condition", "prophostnum", "propEhV"), timevar="watcon", direction="wide")
inf$parameter= "infections"

#renaming
enc <- setnames (enc, c("encounters.calm", "encounters.stormy", "encounters.moderate"), c("calm", "stormy", "moderate"))
ads <- setnames (ads, c("adstot.calm", "adstot.stormy", "adstot.moderate"), c("calm", "stormy", "moderate"))
inf <- setnames (inf, c("sucinf.calm", "sucinf.stormy", "sucinf.moderate"), c("calm", "stormy", "moderate"))

#combined
allsteps <- rbind (conc, enc, ads, inf)

##combine high and low
high <- allsteps %>% filter (virus=="high") %>% filter (!(parameter=="concentration"))
low <- allsteps %>% filter (virus=="low")  %>% filter (!(parameter=="concentration"))
allsteps_comb <- high %>% select (c(group, condition, prophostnum, propEhV, parameter))
conc_allcond <- allsteps %>% filter (parameter=="concentration") %>% select ("group", "condition", "prophostnum", "propEhV", "parameter", "moderate")
allsteps_comb$calm <- high$calm + low$calm
allsteps_comb$moderate <- high$moderate + low$moderate
allsteps_comb$stormy <- high$stormy + low$stormy

##correct concentration 
allsteps_comb <- full_join(allsteps_comb, conc_allcond)

#edited according to the lab
library(lemon)
resize.win (11,6)

variable_labs <- c(
  `concentration` = 'concentration~(mL^{-1})',
  `encounters` = 'encounters~entity^{-1}~d^{-1}',
  `adsorption` = 'adsorptions~entity^{-1}~d^{-1}',
  `infections` = 'infections~entity^{-1}~d^{-1}'
)

allsteps_comb$parameter <- reorder.factor (allsteps_comb$parameter, new.order = c("concentration", "encounters", "adsorption", "infections"))

allsteps_comb$group2 <- factor (allsteps_comb$group,labels= c("E. huxleyi", "coccolith"))

ggplot(allsteps_comb, aes(x=group2, y=log10(stormy), color="stormy"))  + 
  geom_point (size=6) + 
  geom_point (data=allsteps_comb, aes (x=group2, y=log10(moderate), color="moderate"), size=6) +
  geom_point (data=allsteps_comb, aes (x=group2, y=log10(calm), color="calm"), size=6) +
  facet_rep_grid(condition~parameter, ##tick marks 
                 labeller = labeller(parameter = as_labeller(variable_labs, label_parsed))) +
  theme_Publication2() + theme (axis.title.x = element_blank(), 
                                strip.background.x  = element_blank(),axis.ticks.length = unit(5, "pt"),
                                panel.spacing = unit(1, "line"), legend.title = element_blank(),
                                axis.text.x = element_text(face="italic"),
                                panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                colour = "#D3D3D3")) + 
  labs (y = expression(log[10])) + 
  scale_color_manual (values=c('#2b57a7', '#ffc4b4', '#b11346')) 

## ------------------------------------------------------------------------
##pie charts

allsteps_comb.melt <- reshape2::melt(allsteps_comb %>% filter (!(parameter=="concentration")) %>% 
                                       select ("group", "condition", "parameter", "calm", "moderate", "stormy"), 
                                     id.vars=c("group", "condition", "parameter"))

allsteps_comb.melt$percentwhole <- allsteps_comb.melt$value*100
allsteps_comb.melt<- allsteps_comb.melt %>% mutate(percentwhole= if_else(percentwhole > 100, 100, percentwhole))  
allsteps_comb.melt$percentminus <- 100-allsteps_comb.melt$percentwhole
allsteps_comb.melt$turb <- allsteps_comb.melt$variable

allsteps_comb.melt2 <- reshape2::melt(allsteps_comb.melt %>% 
                                        select ("group", "condition", "parameter", "turb", "percentwhole", "percentminus"), 
                                      id.vars=c("group", "condition", "parameter", "turb"))

#to only have percentwhole labeled
allsteps_comb.melt2$perc <- ifelse(allsteps_comb.melt2$variable=="percentminus", allsteps_comb.melt2$perc=="NA", 
                                   allsteps_comb.melt2$value)
#for colors
allsteps_comb.melt2$piefill <- as.factor (paste (allsteps_comb.melt2$parameter, allsteps_comb.melt2$variable, sep="_"))

allsteps_comb.melt2$group2 <- factor (allsteps_comb.melt2$group,labels= c("E. huxleyi", "coccolith"))

resize.win (18,16)
ggplot(allsteps_comb.melt2 %>% filter (!(group=="Li" & parameter=="infections")), aes(x = "", y = value, fill=piefill)) +
  geom_bar(width = 1, stat = "identity", color = "black") +   coord_polar("y", start = 0)+
  theme_void() +  
  scale_fill_manual(values=c("#FFFFFF", "#fc8d62", "#FFFFFF" , "#66c2a5", "#FFFFFF", "#8da0cb")) +
  theme (legend.position = "none", legend.text = element_text(size = 15), 
         strip.text = element_text(size=15), legend.title=element_blank()) + facet_grid(condition+turb~parameter+group2) + 
  geom_text(aes(y = perc, color=variable, label=sprintf("%0.0f%%", round(value, digits = 1))), size=5, 
            position = position_stack(vjust = 0.5)) +
  scale_color_manual(values=c("black", "#FFFFFF")) 

## ------------------------------------------------------------------------
##all hosts plot
##summaries of all host entities

allhost <- calmstormy %>% filter (condition=="shelf/slope") %>% filter (group=="Cc") %>% 
  select(c(group, beta_all, watcon, virus, propvir, ads, inf))
allhost <- allhost[rep(seq_len(nrow(allhost)), 21), ]
allhost$hostnum <- rep(c (1 %o% 10^(seq(0, 5, 0.25))), 1, each=6)

allhost <- allhost %>%
  mutate (virnum =  hostnum*7)

#calculate propEhV
allhost$propEhV <- allhost$virnum* allhost$propvir

#calculate encounters fast slow
allhost$encounters <- allhost$beta_all*allhost$propEhV 

#calculate total adsorption by virus props
allhost$adstot <- allhost$encounters*allhost$ads

#calculate total successful infections
allhost$sucinf <- allhost$encounters*allhost$ads*allhost$inf

#get infection vs host
allhost_inf <- reshape(allhost %>% select (group, virus, hostnum, watcon, sucinf, virnum), idvar=c("group", "virus", "hostnum", "virnum"), timevar="watcon", direction="wide")
allhost_inf$parameter= "infections"

##combine high and low
allhost_high <- allhost_inf %>% filter (virus=="high") 
allhost_low <- allhost_inf %>% filter (virus=="low") 
allhost_comb <- allhost_high %>% select (c(group, virus, hostnum, virnum))
allhost_comb$calm <- allhost_high$sucinf.calm + allhost_low$sucinf.calm
allhost_comb$moderate <- allhost_high$sucinf.moderate + allhost_low$sucinf.moderate
allhost_comb$stormy <- allhost_high$sucinf.stormy + allhost_low$sucinf.stormy

#melt data
allhost_comb.melt <- reshape2::melt(allhost_comb %>% select (c(group, hostnum, virnum, calm, moderate, stormy)), 
                                    id.vars=c("group", "hostnum", "virnum"))

resize.win(5,5)
ggplot(allhost_comb.melt, aes (x=log10(hostnum), y=log10(value*100), color=variable)) + 
  geom_line (size=2) + theme_Publication2() +  scale_color_manual (values=c('#2b57a7', '#ffc4b4', '#b11346')) +
  labs (x=expression (log[10]~"host concentration"~mL^-1), y= expression ("% population infected "~d^-1)) + 
  theme (legend.title = element_blank(), legend.key.width = unit (1, "cm"), legend.position = "bottom",
         legend.direction = "horizontal", 
         panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#D3D3D3")) +   
  geom_hline(yintercept = log10(10), linetype="dashed") +
  scale_y_continuous(breaks = c(2, 1, 0, -1, -2, -3, -4),label = c(100, 10, 1, 0.1, 0.01, 0.001, 0.0001)) 

allhost_comb.melt$percentage <- allhost_comb.melt$value*100

## ------------------------------------------------------------------------
##contribution of betas

betas_contrib <- reshape2::melt(calmstormy %>% select (group, watcon, beta_BM, beta_DS, beta_turb), id.vars=c("group", "watcon"))
betas_contrib$betasf <- factor (betas_contrib$variable,labels= c("Brownian motion", "Sinking", "Turbulence"))

betas_contrib$group2 <- factor (betas_contrib$group,labels= c("E. huxleyi", "coccolith"))

resize.win (7,5)
ggplot(betas_contrib, aes(x = group2, y = value, fill=betasf)) +
  geom_bar(position="fill", stat="identity") + 
  facet_grid (~watcon) + theme_Publication2() + 
  labs (y=expression("contribution to total "~beta)) +
  theme (axis.title.x = element_blank(), legend.title = element_blank(), legend.key.width = unit (1, "cm")) +
  scale_fill_manual (values=c('#615989', '#dfdfc1', '#c96700')) 

## ------------------------------------------------------------------------
##adsorption plot
library(readr)
ads_plot <- read_csv("E:/Users/karengb/Postdoc-R/CSV Files/ads_may2021.csv")

ads_plot$paper <- factor (ads_plot$paper,labels= c("Johns et. al", "This study"))

resize.win(5,5)
ggplot (ads_plot, aes(x=paper, y=log10(adscoef), color=paper)) + geom_boxplot(lwd=0.75) + theme_Publication2() + 
  theme (axis.title.x = element_blank(), legend.position = "none") + labs (y= expression(log[10]~K[d]~("mL"~"d"^-1))) +
  scale_color_manual (values=c("#1b9e77", "#7570b3")) 


library (plotrix)
cols <- c("adscoef")
ads_sum <- ads_plot %>% group_by(paper) %>% 
  summarise_at(.vars = cols,
               funs(mean, sd))

