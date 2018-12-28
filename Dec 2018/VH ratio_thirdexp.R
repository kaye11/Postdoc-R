
library(readxl)
host <- read_excel("Postdoc-R/Exported Tables/ThirdExp_sytox.xlsx")
virbac <- read_excel("Postdoc-R/Exported Tables/ThirdExp_virbac.xlsx")

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
cell <- host %>% filter(stain %in% c("countperml"))
ehv <- virbac %>% filter(cell %in% c("EhV"))

cell <- cell%>% arrange (factor(maingroup, c("still-control", "still-infected", 
                                             "turbulent-control", "turbulent-infected", 
                                             "still-viral particles", "turbulent-viral particles")))

ehv <- ehv%>% arrange (factor(maingroup, c("still-control", "still-infected", 
                                           "turbulent-control", "turbulent-infected", 
                                           "still-viral particles", "turbulent-viral particles")))


cell.ehv <- cbind(cell [c(5:6)], ehv [c(6, 8:13)])
cell.ehv$VH <- cell.ehv$count/cell.ehv$value
cell.ehv$VHdiv <- cell.ehv$VH/10^3
cell.ehv.dropvp <- cell.ehv[! cell.ehv$group2=="viralparticles",  ]

ggplotly(ggplot(data=cell.ehv.dropvp, aes(x=time, y=VHdiv, colour=group2)) +geom_boxplot() + 
           facet_grid(~group1, scales="free")+ geom_point()+ theme_bw())

sum.all <- summarySE(cell.ehv.dropvp, measurevar = "VHdiv", 
                     groupvars = c("maingroup", "group1", "group2", "time"))

#combined
resize.win (9, 9)
ggplot(data=sum.all, aes(x=time, y=VHdiv, colour=maingroup, shape=maingroup, linetype=maingroup)) +
  geom_point(size=5) + 
  geom_errorbar(aes(ymin=VHdiv-se, ymax=VHdiv+se, width=5)) + 
  geom_smooth(method="loess") + 
  labs(y= expression("EhV:Ehux"~ scriptstyle(x)~"10"^~3), x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_color_manual(values = rep(c("#e41a1c", "#e41a1c", "#377eb8", "#377eb8"), times = 2)) +
  scale_linetype_manual(values = rep(c("solid", "longdash"), times = 4)) + 
  scale_shape_manual(values = rep(16:17, 2)) + 
  theme_Publication() +
  theme(legend.key.width=unit(3,"line"), legend.title = element_blank())

#boxplots: time should be a factor, geom_smooth: time should be numeric
cell.ehv.dropvp$timef <- as.factor(cell.ehv.dropvp$time)

ggplot(data=cell.ehv.dropvp, aes(x=timef, y=VHdiv, colour=group2)) +
  geom_boxplot() + 
  labs(y= expression("EhV:Ehux"~ scriptstyle(x)~"10"^~3), x= "hours post-infection") +
  scale_x_discrete(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_color_manual(values = c("#e41a1c", "#377eb8")) +
  facet_grid(~group1, scales="free") + geom_point() +
  theme_Publication() +
  theme(legend.title = element_blank())

#notcombined

ggplot(data=sum.all, aes(x=time, y=VHdiv, colour=group2)) +
  geom_point(size=5) + 
  geom_errorbar(aes(ymin=VHdiv-se, ymax=VHdiv+se, width=5)) + 
  geom_smooth(method="loess") + 
  labs(y= expression("EhV:Ehux"~ scriptstyle(x)~"10"^~3), x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_color_manual (values = c(control="lightcoral", viralparticles="seagreen3", infected="steelblue2")) +
  theme_Publication() + 
  facet_grid(~group1)+ theme(legend.title=element_blank())


