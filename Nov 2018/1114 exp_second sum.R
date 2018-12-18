
library(readxl)
second <- read_excel("D:/Postdoc/Experiments/181114 Infection Second/summary.xlsx")

require(ggplot2)
require(Rmisc)

second$treatment <- as.factor (second$treatment)
second$timef <- as.factor(second$time)
second$sytox <- second$sytox*100

require (plotly)
source("theme_Publication.R")
require(reshape2)
source("resizewin.R")
require(dplyr)
resize.win (12,9)

#divide cellcount measurements by 10^6
second$countpermldiv <- second$countperml/10^6

second.long <- melt (data=second, id.vars=c("treatment", "time", "timef", "rep"), variable.name="stain")

second.long$group <- case_when(
  second.long$treatment =="sc"  ~ "still control",
  second.long$treatment =="si" ~ "still infected",
  second.long$treatment =="svp" ~ "still viralparticles",
  second.long$treatment =="tc"  ~ "turbulent control",
  second.long$treatment =="ti" ~ "turbulent infected",
  second.long$treatment =="tvp" ~ "turbulent viralparticles",
  TRUE ~ as.character(second.long$treatment)
)


require(tidyr)

second.long<- separate(second.long, group, into = paste("group", 1:2, sep = ""))
second.long$maingroup <- as.factor(paste(second.long$group1, second.long$group2, sep="-" ))

second.long$maingroup <- factor(second.long$maingroup,
                                  levels = c("still-control", "still-infected", 
                                             "turbulent-control", "turbulent-infected", 
                                             "still-viralparticles", "turbulent-viralparticles"),
                                  labels = c("still-control", "still-infected", 
                                             "turbulent-control", "turbulent-infected", 
                                             "still-viral particles", "turbulent-viral particles"))

second.long$group1 <- as.factor(second.long$group1)
second.long$group2 <- as.factor(second.long$group2)

#drop levels
#second.long.dropvp <- second.long[! second.long$group2=="viralparticles",  ]

#use all data
sum.all <- summarySE(second.long, measurevar = "value", groupvars = c("treatment", "timef", "time", "stain"))

scientific_10 <- scientific_10 <- function(x) {
  ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scales::scientific_format()(x)))))
}

ggplotly(ggplot(data=second.long, aes(x=time, y=value, colour=treatment)) +geom_boxplot() + 
           facet_grid(stain~treatment, scales="free")+ geom_point()+ theme_bw())

ggplot(data=sum.all, aes(x=time, y=value, colour=treatment)) +
  geom_point(size=5, position = position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se), position = position_dodge(width=0.5)) + theme_bw()+
  geom_smooth(method="loess") + facet_grid(stain~treatment, scales="free")

sum.all.group <- summarySE(second.long, measurevar = "value", 
                           groupvars = c("maingroup", "group1", "group2", "time", "stain"))

#ALWAYS REMEMBER FOR GEOM_SMOOTH TO HAVE X sytox AS NUMERIC!
allplots <- ggplot(data=sum.all.group, aes(x=time, y=value, colour=group2)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se, width=2)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2)) + 
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  facet_grid(stain~group1, scales="free")+ 
  theme_Publication()

allplots

#it is not possible to make different y axis labels for facet_grid, so just make multiple graphs then arrange with grob

sum.all.group$stain2 <- factor(sum.all.group$stain, levels=c("countperml", "sytox"), 
                              labels =c ("Cell count", "%Sytox Stained"))

cellcount <- ggplot(data=sum.all.group %>% 
                      filter(stain %in% c("countpermldiv")), aes(x=time, y=value, colour=group2)) +
  geom_point(size=5) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se, width=5)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2), alpha=0.2) + 
  labs (y= expression("cells per mL"~ scriptstyle(x)~"10"^~6)) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  theme_Publication() + 
  facet_grid(~group1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none")

sytox <- ggplot(data=sum.all.group %>% 
                 filter(stain %in% c("sytox")), aes(x=time, y=value, colour=group2)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se, width=5)) + 
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

#export data
setwd("D:/R program")
require(openxlsx)
write.xlsx(sum.all.group, file = "Postdoc-R/Exported Tables/SecondExp_summary_sytox.xlsx")
write.xlsx(second.long, file = "Postdoc-R/Exported Tables/SecondExp_sytox.xlsx")
