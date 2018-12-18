library(readr)
adsorption <- read_csv("Postdoc-R/CSV Files/summary adsorption.csv")

library(tidyr)
adsorption<- separate(adsorption, reservoir, into = paste("group", 1:2, sep = ""))
adsorption[is.na(adsorption)] <- c("lith")

cols <- c("virus", "group1", "group2", "type")
adsorption[cols] <- lapply(adsorption[cols], factor)
adsorption$reservoir <- as.factor(paste(adsorption$group1, adsorption$group2, sep='-'))
adsorption$group <- adsorption$group2
adsorption$group2 <- NULL

allbetas.ads <- merge(allbetas.melt,adsorption, by="group")
allbetas.ads.lith <- merge (adsorption, lith, by="group")

allbetas.ads$finalE <- allbetas.ads$E_V*allbetas.ads$Cd
allbetas.ads.lith$finalE <- allbetas.ads.lith$E_V*allbetas.ads.lith$Cd

allbetas.ads.sum <- summarySE(allbetas.ads, measurevar = "finalE", 
                              groupvars = c("group2", "group", "type", "disrate", "betakernel"))

allbetas.ads.lith.sum <- summarySE(allbetas.ads.lith, measurevar = "finalE", 
                              groupvars = c("group2", "group", "type", "disrate", "betakernel"))
                              
library(ggplot2)
ggplot(data=allbetas.ads.sum %>% filter(betakernel %in% c("beta_all")), 
       aes(x=disrate,y = finalE, color=group2)) + 
  geom_line(size=1, position=position_jitter(w=0.02, h=0))+
  geom_line(data = allbetas.ads.lith.sum, aes(y= finalE, color=group2)) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=4),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=7),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks()+
  theme_Publication() +
  theme(legend.title = element_blank(), legend.key.width=unit(1,"cm"))+
  labs(y = expression("viral encounters " ~day^-1~cell^-1), x = expression("dissipation rate "~(m^2~s^-3))) +
  theme(legend.title = element_blank()) + facet_grid(~type)


