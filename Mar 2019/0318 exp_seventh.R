
library(readxl)
seventh <- read_excel("D:/Postdoc/Experiments/190318 Infection Seventh/summary.xlsx")
View(seventh)

require(ggplot2)
require(Rmisc)
require(data.table)

seventh$treatment <- as.factor (seventh$treatment)
seventh$timef <- as.factor(seventh$time)
seventh$dead <- seventh$countperml*seventh$sytox
seventh$sytox <- seventh$sytox*100

require (plotly)
source("theme_Publication.R")
require(reshape2)
source("resizewin.R")
require(dplyr)
resize.win (12,9)

#divide cellcount measurements by 10^6
seventh$countpermldiv <- seventh$countperml/10^6

#divide dead cells by 10^3
seventh$deadpermldiv <- seventh$dead/10^3

seventh.long <- melt (data=seventh, id.vars=c("treatment", "time", "timef", "rep"), variable.name="parameter")

seventh.long$group <- case_when(
  seventh.long$treatment =="sc"  ~ "still control",
  seventh.long$treatment =="si" ~ "still infected",
  seventh.long$treatment =="tc"  ~ "turbulent control",
  seventh.long$treatment =="ti" ~ "turbulent infected",
  TRUE ~ as.character(seventh.long$treatment)
)


require(tidyr)

seventh.long<- separate(seventh.long, group, into = paste("group", 1:2, sep = ""))
seventh.long$maingroup <- as.factor(paste(seventh.long$group1, seventh.long$group2, sep="-" ))

seventh.long$maingroup <- factor(seventh.long$maingroup,
                               levels = c("still-control", "still-infected", 
                                          "turbulent-control", "turbulent-infected"),
                               labels = c("still-control", "still-infected", 
                                          "turbulent-control", "turbulent-infected"))

seventh.long$group1 <- as.factor(seventh.long$group1)
seventh.long$group2 <- as.factor(seventh.long$group2)
seventh.long$value=as.numeric(seventh.long$value)

#use all data
sum.all <- summarySE(seventh.long, measurevar = "value", groupvars = c("treatment", "timef", "time", "parameter"),
                     na.rm=TRUE)

scientific_10 <- scientific_10 <- function(x) {
  ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scales::scientific_format()(x)))))
}

ggplotly(ggplot(data=seventh.long, aes(x=time, y=value, colour=treatment)) +geom_boxplot() + 
           facet_grid(parameter~treatment, scales="free")+ geom_point()+ theme_bw())

ggplot(data=sum.all, aes(x=time, y=value, colour=treatment)) +
  geom_point(size=5, position = position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se), position = position_dodge(width=0.5)) + theme_bw()+
  geom_smooth(method="loess") + facet_grid(parameter~treatment, scales="free")

sum.all.group <- summarySE(seventh.long, measurevar = "value", 
                           groupvars = c("maingroup", "group1", "group2", "time", "parameter"), na.rm = TRUE)

#ALWAYS REMEMBER FOR GEOM_SMOOTH TO HAVE X sytox AS NUMERIC!
allplots <- ggplot(data=sum.all.group, aes(x=time, y=value, colour=group2)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se, width=2)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2)) + 
  scale_x_continuous(breaks=c(0, 24, 48, 72)) +
  facet_grid(parameter~group1, scales="free")+ 
  theme_Publication()

allplots

#this arrangement you used for the poster (i.e. combine.all script from Dec 2018)
ggplot(data=seventh.long %>% 
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
                                   labels =c ("Cell count", "%Sytox stained"))

cellcount <- ggplot(data=seventh.long %>% 
                      filter(parameter %in% c("countpermldiv")), aes(x=time, y=value, linetype=maingroup)) +
  geom_point(size=7, aes(colour=maingroup, shape=maingroup)) + 
  geom_smooth(method = 'loess', aes(colour=maingroup, fill=maingroup), alpha=0.2, size=1.5) + 
  labs (y= expression("E.huxleyi "~ "mL"^~-1~ scriptstyle(x)~"10"^~6)) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +
  #facet_grid(~group1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none",
        axis.title.y = element_text(vjust=5), plot.margin= margin(5,2.5,8,10))

fvfm <- ggplot(data=seventh.long %>% 
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
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none",
        axis.title.y = element_text(vjust=5), plot.margin= margin(-2,2.5,8,10))

sytox <- ggplot(data=seventh.long %>% 
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
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none", 
        plot.margin= margin(-2,2.5,8,10))

dead <- ggplot(data=seventh.long %>% 
                 filter(parameter %in% c("deadpermldiv")), aes(x=time, y=value, linetype=maingroup)) +
  geom_point(size=7, aes(colour=maingroup, shape=maingroup)) + 
  geom_smooth(method = 'loess', aes(colour=maingroup, fill=maingroup), alpha=0.2, size=1) + 
  labs (y= expression("dead cells"~ "mL"^~-1~ scriptstyle(x)~"10"^~3), x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +
  #facet_grid(~group1) +
  theme(strip.text = element_blank(), legend.title=element_blank(), plot.margin= margin(-2,2.5,0,10)) +
  guides(linetype=guide_legend(ncol=2), colour=guide_legend(ncol=2), shape=guide_legend(ncol=2), 
         fill=guide_legend(ncol=2))

resize.win(18,30)
grid.newpage()
grid.draw(rbind(ggplotGrob(cellcount), ggplotGrob(fvfm), ggplotGrob(sytox), ggplotGrob(dead), size = "last"))


#export data
setwd("D:/R program")
require(openxlsx)
write.xlsx(sum.all.group, file = "Postdoc-R/Exported Tables/seventhExp_summary_sytox.xlsx")
write.xlsx(seventh.long, file = "Postdoc-R/Exported Tables/seventhExp_sytox.xlsx")

#standardization
sytoxtab <- subset(seventh.long, seventh.long$parameter=="sytox")

sytoxtab$sytoxS=NA
k=split(sytoxtab, sytoxtab$maingroup)
sytoxtabstd <- lapply(k, function (x) scale(x[,c("value")], center=T, scale=T))
sytoxtab$sytoxS=unsplit(sytoxtabstd, sytoxtab$maingroup)

#baselining to 0 at time point 0
NT<-data.table(sytoxtab, key=c("rep"))

t1=NT[,list(treatment=treatment, time=time, timef=timef, sytox=value, group1=group1, group2=group2, 
            maingroup=maingroup, sytoxS=sytoxS, sytoxBase=(sytoxS-sytoxS[1])), by=c("rep")]

sytoxnew <- t1 #DATA IS NOW CALLED COUNTBASE

sytoxnew.long <- melt (data=sytoxnew, id.vars=c("treatment", "time", "timef", "rep", "group1", "group2", "maingroup"), variable.name="parameter")

seventh.long.withsytoxbase <- rbind (seventh.long, (sytoxnew.long %>% filter (parameter=="sytoxBase")))

ggplot(data=sytoxnew, aes(x=time, y=sytoxBase, linetype=maingroup)) +
  geom_point(size=7, aes(colour=maingroup, shape=maingroup)) + 
  geom_smooth(method = 'loess', aes(colour=maingroup, fill=maingroup), alpha=0.2, size=1) + 
  labs(y="standard sytox", x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) + 
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +
  #facet_grid(~group1) +
  theme(strip.text = element_blank(), legend.title=element_blank())

write.xlsx(seventh.long.withsytoxbase, file = "Postdoc-R/Exported Tables/seventhExp_sytox_standard.xlsx")
