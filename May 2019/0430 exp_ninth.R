
library(readxl)
ninth <- read_excel("D:/Postdoc/Experiments/190430 Infection Ninth/summary.xlsx")
View(ninth)

require(ggplot2)
require(Rmisc)

ninth$treatment <- as.factor (ninth$treatment)
ninth$timef <- as.factor(ninth$time)
ninth$sytox <- ninth$sytox*100

require (plotly)
source("theme_Publication.R")
require(reshape2)
source("resizewin.R")
require(dplyr)
resize.win (12,9)

#divide cellcount measurements by 10^6
ninth$countpermldiv <- ninth$countperml/10^6

ninth.long <- melt (data=ninth, id.vars=c("treatment", "time", "timef", "rep"), variable.name="parameter")

ninth.long$group <- case_when(
  ninth.long$treatment =="sc"  ~ "still control",
  ninth.long$treatment =="si" ~ "still infected",
  ninth.long$treatment =="tc"  ~ "turbulent control",
  ninth.long$treatment =="ti" ~ "turbulent infected",
  TRUE ~ as.character(ninth.long$treatment)
)


require(tidyr)

ninth.long<- separate(ninth.long, group, into = paste("group", 1:2, sep = ""))
ninth.long$maingroup <- as.factor(paste(ninth.long$group1, ninth.long$group2, sep="-" ))

ninth.long$maingroup <- factor(ninth.long$maingroup,
                                 levels = c("still-control", "still-infected", 
                                            "turbulent-control", "turbulent-infected"),
                                 labels = c("still-control", "still-infected", 
                                            "turbulent-control", "turbulent-infected"))

ninth.long$group1 <- as.factor(ninth.long$group1)
ninth.long$group2 <- as.factor(ninth.long$group2)

#use all data
sum.all <- summarySE(ninth.long, measurevar = "value", groupvars = c("treatment", "timef", "time", "parameter"),
                     na.rm=TRUE)

scientific_10 <- scientific_10 <- function(x) {
  ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scales::scientific_format()(x)))))
}

ggplotly(ggplot(data=ninth.long, aes(x=time, y=value, colour=treatment)) +geom_boxplot() + 
           facet_grid(parameter~treatment, scales="free")+ geom_point()+ theme_bw())

ggplot(data=sum.all, aes(x=time, y=value, colour=treatment)) +
  geom_point(size=5, position = position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se), position = position_dodge(width=0.5)) + theme_bw()+
  geom_smooth(method="loess") + facet_grid(parameter~treatment, scales="free")

sum.all.group <- summarySE(ninth.long, measurevar = "value", 
                           groupvars = c("maingroup", "group1", "group2", "time", "parameter"), na.rm = TRUE)

source("theme_Publication.R")
#ALWAYS REMEMBER FOR GEOM_SMOOTH TO HAVE X sytox AS NUMERIC!
allplots <- ggplot(data=sum.all.group, aes(x=time, y=value, colour=group2)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se, width=2)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2)) + 
  scale_x_continuous(breaks=c(0, 24, 48, 72)) +
  facet_grid(parameter~group1, scales="free")+ 
  theme_Publication()

allplots

#this arrangement you used for the poster (i.e. combine.all script from Dec 2018)
ggplot(data=ninth.long %>% 
         filter(parameter %in% c("countpermldiv", "sytox", "fv/fm")) , 
       aes(x=time, y=value, colour=maingroup, shape=maingroup, linetype=maingroup)) +
  geom_point(size=5) + 
  geom_smooth(method = 'loess', aes(colour=maingroup, fill=maingroup)) + 
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  facet_wrap(.~parameter, scales="free")+ 
  theme_Publication()

#it is not possible to make different y axis labels for facet_grid, so just make multiple graphs then arrange with grob

sum.all.group$parameter2 <- factor(sum.all.group$parameter, levels=c("countperml", "sytox"), 
                                   labels =c ("Cell count", "%Sytox parametered"))

cellcount <- ggplot(data=ninth.long %>% 
                      filter(parameter %in% c("countpermldiv")), aes(x=time, y=value, linetype=maingroup)) +
  geom_point(size=7, aes(colour=maingroup, shape=maingroup)) + 
  geom_smooth(method = 'loess', aes(colour=maingroup, fill=maingroup), alpha=0.2, size=1.5) + 
  labs (y= expression("E.huxleyi"~ "mL"^~-1~ scriptstyle(x)~"10"^~6)) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +
  #facet_grid(~group1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none")

fvfm <- ggplot(data=ninth.long %>% 
                 filter(parameter %in% c("fv/fm")), aes(x=time, y=value, linetype=maingroup)) +
  geom_point(size=7, aes(colour=maingroup, shape=maingroup)) + 
  geom_smooth(method = 'loess', aes(colour=maingroup, fill=maingroup), alpha=0.2, size=1.5) + 
  labs (y= expression("fv/fm")) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +
  #facet_grid(~group1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none")

sytox <- ggplot(data=ninth.long %>% 
                  filter(parameter %in% c("sytox")), aes(x=time, y=value, linetype=maingroup)) +
  geom_point(size=7, aes(colour=maingroup, shape=maingroup)) + 
  geom_smooth(method = 'loess', aes(colour=maingroup, fill=maingroup), alpha=0.2, size=1) + 
  labs(y="% sytox stained", x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +
  #facet_grid(~group1) +
  theme(strip.text = element_blank(), legend.title=element_blank())

resize.win(16,25)
grid.newpage()
grid.draw(rbind(ggplotGrob(cellcount), ggplotGrob(fvfm), ggplotGrob(sytox), size = "last"))





##both conditions in one graph

cellcount.both <-ggplot(data=sum.all.group %>% 
                          filter(parameter %in% c("countpermldiv")), 
                        aes(x=time, y=value, colour=maingroup, shape=maingroup, linetype=maingroup)) +
  geom_point(size=5) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se, width=5)) + 
  geom_smooth(method="loess") + 
  labs (y= expression("cells per mL"~ scriptstyle(x)~"10"^~6)) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96)) +
  scale_color_manual(values = rep(c("#e41a1c", "#e41a1c", "#377eb8", "#377eb8", "#4daf4a", "#4daf4a"), times = 2)) +
  scale_linetype_manual(values = rep(c("solid", "longdash"), times = 6)) + 
  scale_shape_manual(values = rep(16:17, 3)) + 
  theme_Publication() + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none")

sytox.both  <- ggplot(data=sum.all.group %>% 
                        filter(parameter %in% c("sytox")), 
                      aes(x=time, y=value, colour=maingroup, shape=maingroup, linetype=maingroup)) +
  geom_point(size=5) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se, width=5)) + 
  geom_smooth(method="loess") + 
  labs(y="% sytox parametered", x= "hours post-infection") +
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
write.xlsx(sum.all.group, file = "Postdoc-R/Exported Tables/ninthExp_summary_sytox.xlsx")
write.xlsx(ninth.long, file = "Postdoc-R/Exported Tables/ninthExp_sytox.xlsx")
