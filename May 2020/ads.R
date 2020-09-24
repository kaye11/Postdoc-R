library(readxl)
ads <- read_excel("Postdoc-R/CSV Files/ads.xlsx")

source("inspack.R")

ads$group <- reorder.factor (ads$group, new.order = c("Nc", "Cc", "Li"))

resize.win(6,6)
ggplot (ads, aes(x=group, y=log10(adscoef), color=group)) + geom_boxplot() + theme_Publication2() + 
  theme (axis.title.x = element_blank(), legend.position = "none") + labs (y= expression("log10"~K[d]~("mL"~"day"^-1))) +
  scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) 

ggplot (ads, aes(x=group, y=adsef*100, color=group)) + geom_boxplot() + theme_Publication2() +  
  theme (axis.title.x = element_blank(), legend.position = "none") + labs(y = bquote(delta ~ "(%)")) +
  scale_color_manual (values=c("#e41a1c", "#377eb8", "#4daf4a")) 

