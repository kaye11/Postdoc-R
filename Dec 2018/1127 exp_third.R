
library(readxl)
third<- read_excel("D:/Postdoc/Experiments/181127 Infection Third/summary.xlsx")

require(ggplot2)
require(Rmisc)

third$treatment <- as.factor (third$treatment)
third$timef <- as.factor(third$time)
third$sytox <- third$sytox*100

require (plotly)
source("theme_Publication.R")
require(reshape2)
source("resizewin.R")
require(dplyr)
resize.win (12,9)

#divide cellcount measurements by 10^6
third$countpermldiv <- third$countperml/10^6

third.long <- melt (data=third, id.vars=c("treatment", "time", "timef", "rep"), variable.name="stain")

third.long$group <- case_when(
  third.long$treatment =="sc"  ~ "still control",
  third.long$treatment =="si" ~ "still infected",
  third.long$treatment =="svp" ~ "still viralparticles",
  third.long$treatment =="tc"  ~ "turbulent control",
  third.long$treatment =="ti" ~ "turbulent infected",
  third.long$treatment =="tvp" ~ "turbulent viralparticles",
  TRUE ~ as.character(third.long$treatment)
)


require(tidyr)

third.long<- separate(third.long, group, into = paste("group", 1:2, sep = ""))
third.long$maingroup <- as.factor(paste(third.long$group1, third.long$group2, sep="-" ))

third.long$maingroup <- factor(third.long$maingroup,
                               levels = c("still-control", "still-infected", 
                                          "turbulent-control", "turbulent-infected", 
                                          "still-viralparticles", "turbulent-viralparticles"),
                               labels = c("still-control", "still-infected", 
                                          "turbulent-control", "turbulent-infected", 
                                          "still-viral particles", "turbulent-viralparticles"))

third.long$group1 <- as.factor(third.long$group1)
third.long$group2 <- as.factor(third.long$group2)

#drop levels
third.long.dropvp <- third.long[! third.long$group2=="viralparticles",  ]

#use all data
sum.all <- summarySE(third.long, measurevar = "value", groupvars = c("treatment", "timef", "time", "stain"))

scientific_10 <- scientific_10 <- function(x) {
  ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scales::scientific_format()(x)))))
}

ggplotly(ggplot(data=third.long, aes(x=time, y=value, colour=treatment)) +geom_boxplot() + 
           facet_grid(stain~treatment, scales="free")+ geom_point()+ theme_bw())

ggplot(data=sum.all, aes(x=time, y=value, colour=treatment)) +
  geom_point(size=5, position = position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se), position = position_dodge(width=0.5)) + theme_bw()+
  geom_smooth(method="loess") + facet_grid(stain~treatment, scales="free")

sum.all.group <- summarySE(third.long, measurevar = "value", 
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
write.xlsx(sum.all.group, file = "Postdoc-R/Exported Tables/ThirdExp_summary_sytox.xlsx")
write.xlsx(third.long, file = "Postdoc-R/Exported Tables/ThirdExp_sytox.xlsx")
