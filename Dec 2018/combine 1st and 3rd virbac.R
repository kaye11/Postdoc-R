
library(readxl)
third <- read_excel("Postdoc-R/Exported Tables/ThirdExp_virbac.xlsx")
first <- read_excel("Postdoc-R/Exported Tables/FirstExp_virbac.xlsx")

require(ggplot2)
require(Rmisc)
require (plotly)
source("theme_Publication.R")
require(reshape2)
source("resizewin.R")
require(dplyr)
resize.win (12,9)
require (tidyr)

first$countpermldiv <- first$count/10^7

first$exp <- "1st"
third$exp <- "3rd"
a <- first [c(5:13)]
b <- third [c(6:14)]


first.third <- rbind (first [c(5:13)], third [c(6, 8:15)])

first.third.dropvp <- first.third[! first.third$group2=="viralparticles",  ]

ggplotly(ggplot(data=first.third.dropvp, aes(x=time, y=countpermldiv, colour=group2)) +geom_boxplot() + 
           facet_grid(cell~group1, scales="free")+ geom_point()+ theme_bw())

sum.all <- summarySE(first.third.dropvp, measurevar = "countpermldiv", 
                     groupvars = c("maingroup", "group1", "group2", "time", "cell"))


sum.all2 <- summarySE(first.third.dropvp, measurevar = "countpermldiv", 
                     groupvars = c("maingroup", "group1", "group2", "exp", "time", "cell"))

sum.all$time <- as.numeric(sum.all$time)

ggplot(data=sum.all, aes(x=time, y=countpermldiv, colour=group2)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=countpermldiv-se, ymax=countpermldiv+se, width=0.5)) + 
  facet_grid(cell~group1, scales="free")+ theme_bw()

first.third.dropvp$time <- as.numeric (first.third.dropvp$time)

bacteria <- ggplot(data=first.third.dropvp %>% 
                     filter(cell %in% c("Bacteria")), aes(x=time, y=countpermldiv)) +
  geom_point(size=5, aes(colour=group2, shape=exp)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2), alpha=0.2) + 
  labs(y= expression("bacteria per mL"~ scriptstyle(x)~"10"^~7), x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  theme_Publication() + 
  facet_grid(~group1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none")

virus <-ggplot(data=first.third.dropvp %>% 
                 filter(cell %in% c("EhV")), aes(x=time, y=countpermldiv)) +
  geom_point(size=5, aes(colour=group2, shape=exp)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2), alpha=0.2) + 
  labs(y= expression("EhV per mL"~ scriptstyle(x)~"10"^~7), x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  theme_Publication() + 
  facet_grid(~group1) +
  theme(strip.text = element_blank(), legend.title=element_blank())

resize.win(12,16)
grid.newpage()
grid.draw(rbind(ggplotGrob(bacteria), ggplotGrob(virus), size = "last"))

##both conditions in one graph
bacteria.both <-ggplot(data=sum.all %>% 
                          filter(cell %in% c("Bacteria")), 
                       aes(x=time, y=countpermldiv, colour=maingroup, shape=maingroup, linetype=maingroup)) +
  geom_point(size=5) + 
  geom_errorbar(aes(ymin=countpermldiv-se, ymax=countpermldiv+se, width=5)) + 
  geom_smooth(method="loess") + 
  labs(y= expression("bacteria per mL"~ scriptstyle(x)~"10"^~7), x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_color_manual(values = rep(c("#e41a1c", "#e41a1c", "#377eb8", "#377eb8"), times = 2)) +
  scale_linetype_manual(values = rep(c("solid", "longdash"), times = 4)) + 
  scale_shape_manual(values = rep(16:17, 2)) + 
  theme_Publication() + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none")

virus.both  <- ggplot(data=sum.all %>% 
                        filter(cell %in% c("EhV")), 
                      aes(x=time, y=countpermldiv, colour=maingroup, shape=maingroup, linetype=maingroup)) +
  geom_point(size=5) + 
  geom_errorbar(aes(ymin=countpermldiv-se, ymax=countpermldiv+se, width=5)) + 
  geom_smooth(method="loess") + 
  labs(y= expression("EhV per mL"~ scriptstyle(x)~"10"^~7), x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_color_manual(values = rep(c("#e41a1c", "#e41a1c", "#377eb8", "#377eb8"), times = 2)) +
  scale_linetype_manual(values = rep(c("solid", "longdash"), times = 4)) + 
  scale_shape_manual(values = rep(16:17, 2)) + 
  theme_Publication() + 
  theme(legend.key.width=unit(3,"line"), legend.title = element_blank())


grid.draw(rbind(ggplotGrob(bacteria.both), ggplotGrob(virus.both), size = "last"))


