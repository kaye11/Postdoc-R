
library(readxl)
third <- read_excel("Postdoc-R/Exported Tables/ThirdExp_sytox.xlsx")
first <- read_excel("Postdoc-R/Exported Tables/FirstExp_sytox.xlsx")

require(ggplot2)
require(Rmisc)
require (plotly)
source("theme_Publication.R")
require(reshape2)
source("resizewin.R")
require(dplyr)
resize.win (12,9)
require (tidyr)

#extract cell and viral count
first.filt<- first %>% filter(stain %in% c("countperml", "sytox"))
first.filt$exp <- "1st"
third$exp <- "3rd"

first.third <- rbind(first.filt, third)
first.third.dropvp <- first.third[! first.third$group2=="viralparticles",  ]

ggplotly(ggplot(data=first.third.dropvp, aes(x=time, y=value, colour=treatment, shape=exp)) +geom_boxplot() + 
           facet_grid(stain~treatment, scales="free")+ geom_point()+ theme_bw())

sum.all.group <- summarySE(first.third.dropvp, measurevar = "value", 
                           groupvars = c("maingroup", "group1", "group2", "time", "stain"))

sum.all.group2 <- summarySE(first.third.dropvp, measurevar = "value", 
                           groupvars = c("maingroup", "group1", "group2", "time", "stain", "exp"))

cellcount <- ggplot(data=first.third.dropvp %>% 
                      filter(stain %in% c("countpermldiv")), aes(x=time, y=value)) +
  geom_point(size=5, aes(colour=group2, shape=exp)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2), alpha=0.2) + 
  labs (y= expression("cells per mL"~ scriptstyle(x)~"10"^~6)) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  theme_Publication() + 
  facet_grid(~group1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none")

sytox <- ggplot(data=first.third.dropvp %>% 
                  filter(stain %in% c("sytox")), aes(x=time, y=value)) +
  geom_point(size=5, aes(colour=group2, shape=exp)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2), alpha=0.2) + 
  labs(y="% sytox stained") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  theme_Publication() + 
  facet_grid(~group1) +
  theme(strip.text = element_blank(), legend.title=element_blank())

resize.win(12,16)
grid.newpage()
grid.draw(rbind(ggplotGrob(cellcount), ggplotGrob(sytox), size = "last"))

##both conditions in one graph

cellcount.both <-ggplot(data=sum.all.group %>% 
                          filter(stain %in% c("countpermldiv")), 
                        aes(x=time, y=value, colour=maingroup, shape=maingroup, linetype=maingroup)) +
  geom_point(size=5) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se, width=5)) + 
  geom_smooth(method="loess") + 
  labs (y= expression("cells per mL"~ scriptstyle(x)~"10"^~6)) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_color_manual(values = rep(c("#e41a1c", "#e41a1c", "#377eb8", "#377eb8", "#4daf4a", "#4daf4a"), times = 2)) +
  scale_linetype_manual(values = rep(c("solid", "longdash"), times = 6)) + 
  scale_shape_manual(values = rep(16:17, 3)) + 
  theme_Publication() + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none")

sytox.both  <- ggplot(data=sum.all.group %>% 
                        filter(stain %in% c("sytox")), 
                      aes(x=time, y=value, colour=maingroup, shape=maingroup, linetype=maingroup)) +
  geom_point(size=5) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se, width=5)) + 
  geom_smooth(method="loess") + 
  labs(y="% sytox stained", x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_color_manual(values = rep(c("#e41a1c", "#e41a1c", "#377eb8", "#377eb8", "#4daf4a", "#4daf4a"), times = 2)) +
  scale_linetype_manual(values = rep(c("solid", "longdash"), times = 6)) + 
  scale_shape_manual(values = rep(16:17, 3)) + 
  theme_Publication() +
  theme(legend.key.width=unit(3,"line"), legend.title = element_blank())


grid.draw(rbind(ggplotGrob(cellcount.both), ggplotGrob(sytox.both), size = "last"))

