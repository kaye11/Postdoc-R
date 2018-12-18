
library(readxl)
first <- read_excel("D:/Postdoc/Experiments/181022 Infection The First/summary.xlsx")

require(ggplot2)
require(Rmisc)

first$treatment <- as.factor (first$treatment)
first$timef <- as.factor(first$time)
first$sytox <- first$sytox*100


require (plotly)
source("theme_Publication.R")
require(reshape2)
source("resizewin.R")
require(dplyr)
resize.win (12,9)

#divide cellcount measurements by 10^6
first$countpermldiv <- first$countperml/10^6
first.long <- melt (data=first, id.vars=c("treatment", "time", "timef", "rep"), variable.name="stain")

first.long$group <- case_when(
  first.long$treatment =="sc"  ~ "still control",
  first.long$treatment =="si" ~ "still infected",
  first.long$treatment =="svp" ~ "still viralparticles",
  first.long$treatment =="tc"  ~ "turbulent control",
  first.long$treatment =="ti" ~ "turbulent infected",
  first.long$treatment =="tvp" ~ "turbulent viralparticles",
  TRUE ~ as.character(first.long$treatment)
)

require(tidyr)

first.long<- separate(first.long, group, into = paste("group", 1:2, sep = ""))
first.long$maingroup <- as.factor(paste(first.long$group1, first.long$group2, sep="-" ))

first.long$group1 <- as.factor(first.long$group1)
first.long$group2 <- as.factor(first.long$group2)

#drop levels
first.long.dropvp <- first.long[! first.long$group2=="viralparticles",  ]

sum.all <- summarySE(first.long.dropvp, measurevar = "value", groupvars = c("treatment", "timef", "stain"))

scientific_10 <- scientific_10 <- function(x) {
  ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scales::scientific_format()(x)))))
}

ggplotly(ggplot(data=first.long.dropvp, aes(x=time, y=value, colour=treatment)) +geom_boxplot() + 
           facet_grid(stain~treatment, scales="free")+ geom_point()+ theme_bw())

ggplot(data=sum.all, aes(x=timef, y=value, colour=treatment)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se)) + facet_grid(stain~treatment, scales="free")+ theme_bw()+
  scale_y_continuous(label=scientific_10)

sum.all.group <- summarySE(first.long.dropvp, measurevar = "value", 
                           groupvars = c("maingroup", "group1", "group2", "stain", "time"))

#ALWAYS REMEMBER FOR GEOM_SMOOTH TO HAVE X VALUE AS NUMERIC!
allplots <- ggplot(data=sum.all.group, aes(x=time, y=value, colour=group2)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se, width=2)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2)) + 
  scale_x_continuous(breaks=c(0, 24, 48, 72)) +
  facet_grid(stain~group1, scales="free")+ 
  scale_y_continuous(label=scientific_10)+ theme_Publication()

#it is not possible to make different y axis labels for facet_grid, so just make multiple graphs then arrange with grob

sum.all.group$stain2 <- factor(sum.all.group$stain, levels=c("countperml", "sytox", "NO", "ROSfl1"), 
                              labels =c ("Cell count", "Sytox", "NO", "ROS"))

cellcount <- ggplot(data=sum.all.group %>% 
                      filter(stain %in% c("countperml")), aes(x=time, y=value, colour=group2)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se, width=2)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2), alpha=0.2) + 
  labs(y="cells per ml") +
  scale_x_continuous(breaks=c(0, 24, 48, 72)) +
  scale_y_continuous(label=scientific_10) +
  theme_Publication() + 
  facet_grid(~group1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none")

sytox <- ggplot(data=sum.all.group %>% 
                 filter(stain %in% c("sytox")), aes(x=time, y=value, colour=group2)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se, width=2)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2), alpha=0.2) + 
  labs(y="% sytox stained") +
  scale_x_continuous(breaks=c(0, 24, 48, 72)) +
  theme_Publication() + 
  facet_grid(~group1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none", 
        strip.text = element_blank())

NO <- ggplot(data=sum.all.group %>% 
               filter(stain %in% c("NO")), aes(x=time, y=value, colour=group2)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se, width=2)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2), alpha=0.2) + 
  labs(y="RFU per cell (NO)") +
  scale_x_continuous(breaks=c(0, 24, 48, 72)) +
  scale_y_continuous(label=scientific_10) +
  theme_Publication() + 
  facet_grid(~group1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none", 
        strip.text = element_blank())

ROS <- ggplot(data=sum.all.group %>% 
               filter(stain %in% c("ROSfl1")), aes(x=time, y=value, colour=group2)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se, width=2)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2), alpha=0.2) + 
  labs(y="RFU per cell (ROS)") +
  scale_x_continuous(breaks=c(0, 24, 48, 72)) +
  scale_y_continuous(label=scientific_10) +
  theme_Publication() + 
  facet_grid(~group1) +
  theme(legend.title=element_blank(), strip.text = element_blank())

resize.win(12,16)
grid.newpage()
grid.draw(rbind(ggplotGrob(cellcount), ggplotGrob(sytox), ggplotGrob(NO), ggplotGrob(ROS),  size = "last"))

grid.draw(rbind(ggplotGrob(cellcount), ggplotGrob(sytox), size= "last"))

##both conditions in one graph, sytox and countperml div only

cellcount.both <-ggplot(data=sum.all.group %>% 
                          filter(stain %in% c("countpermldiv")), 
                        aes(x=time, y=value, colour=maingroup, shape=maingroup, linetype=maingroup)) +
  geom_point(size=5) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se, width=5)) + 
  geom_smooth(method="loess") + 
  labs (y= expression("cells per mL"~ scriptstyle(x)~"10"^~6)) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_color_manual(values = rep(c("#e41a1c", "#e41a1c", "#377eb8", "#377eb8"), times = 2)) +
  scale_linetype_manual(values = rep(c("solid", "longdash"), times = 4)) + 
  scale_shape_manual(values = rep(16:17, 2)) + 
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
  scale_color_manual(values = rep(c("#e41a1c", "#e41a1c", "#377eb8", "#377eb8"), times = 2)) +
  scale_linetype_manual(values = rep(c("solid", "longdash"), times = 4)) + 
  scale_shape_manual(values = rep(16:17, 2)) + 
  theme_Publication() +
  theme(legend.key.width=unit(3,"line"), legend.title = element_blank())


grid.draw(rbind(ggplotGrob(cellcount.both), ggplotGrob(sytox.both), size = "last"))



#export data
setwd("D:/R program")
require(openxlsx)
write.xlsx(sum.all.group, file = "Postdoc-R/Exported Tables/FirstExp_summary_sytox.xlsx")
write.xlsx(first.long, file = "Postdoc-R/Exported Tables/FirstExp_sytox.xlsx")




