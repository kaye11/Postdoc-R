library(readxl)
twe <- read_excel("D:/Postdoc/Experiments/190910 EhV203 Twelfth/190910 EhV203 Twelfth summary.xlsx")
View(twe)

require(ggplot2)
require(Rmisc)
require(data.table)

twe$treatment <- as.factor (twe$treatment)
twe$dayf <- as.factor(twe$day)
twe$dead <- twe$countperml*twe$sytox
twe$sytox <- twe$sytox*100


require (plotly)
source("theme_Publication.R")
require(reshape2)
source("resizewin.R")
require(dplyr)
resize.win (12,9)

#divide ctwelcount measurements by 10^5
twe$countpermldiv <- twe$countperml/10^5

#divide dead ctwels by 10^3
twe$deadpermldiv <- twe$dead/10^3

twe.long <- melt (data=twe, id.vars=c("treatment", "day", "dayf", "rep"), variable.name="parameter")

twe.long$group <- case_when(
  twe.long$treatment =="sc"  ~ "still control",
  twe.long$treatment =="si" ~ "still infected",
  twe.long$treatment =="tc"  ~ "turbulent control",
  twe.long$treatment =="ti" ~ "turbulent infected",
  TRUE ~ as.character(twe.long$treatment)
)

require(tidyr)

twe.long<- separate(twe.long, group, into = paste("group", 1:2, sep = ""))
twe.long$maingroup <- as.factor(paste(twe.long$group1, twe.long$group2, sep="-" ))

twe.long$maingroup <- factor(twe.long$maingroup,
                               levels = c("still-control", "still-infected", 
                                          "turbulent-control", "turbulent-infected"),
                               labels = c("still-control", "still-infected", 
                                          "turbulent-control", "turbulent-infected"))

twe.long$group1 <- as.factor(twe.long$group1)
twe.long$group2 <- as.factor(twe.long$group2)

#change rpms
twe.long$rpm <- "600"
twe.long[twe.long$rep %in% c('tc1', 'tc2', 'ti1', 'ti2'), ]$rpm <- "350"
twe.long[twe.long$group1 %in% c('still'), ]$rpm <- "0"

twe.long$supergroup <- as.factor (paste(twe.long$maingroup, twe.long$rpm, sep="-"))
twe.long$value <- as.numeric (twe.long$value)

#use all data
sum.all <- summarySE(twe.long, measurevar = "value", 
                     groupvars = c("treatment", "dayf", "day", "parameter", "rpm", "supergroup"),
                     na.rm=TRUE)

scientific_10 <- scientific_10 <- function(x) {
  ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scales::scientific_format()(x)))))
}

ggplotly(ggplot(data=twe.long %>% filter (parameter %in% c("countpermldiv", "sytox", "deadpermldiv")), 
                aes(x=day, y=value, colour=supergroup)) +geom_boxplot() + 
           facet_grid(parameter~supergroup, scales="free")+ geom_point()+ theme_bw())

ggplotly(ggplot(data=twe.long %>% filter (parameter %in% c("countpermldiv", "sytox")) %>%
                  filter (group2 %in% c("infected")), 
                aes(x=day, y=value, colour=supergroup)) +geom_boxplot() + 
           facet_grid(parameter~supergroup, scales="free")+ geom_point()+ theme_bw())

ggplot(data=sum.all %>% filter (parameter %in% c("countpermldiv", "sytox")) %>% 
         filter (treatment %in% c("si", "ti")), aes(x=day, y=value, colour=supergroup)) +
  geom_point(size=5, position = position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se), position = position_dodge(width=0.5)) + theme_bw()+
  geom_smooth(method="loess") + facet_grid(parameter~., scales="free")

sum.all.group <- summarySE(twe.long, measurevar = "value", 
                           groupvars = c("supergroup", "maingroup", "group1", "group2", 
                                         "day", "rpm", "parameter"), na.rm = TRUE)

cbPalette <- c("#0072B2", "#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7", "#F0E442")

resize.win (12,9)

twe.long$supergroup <- factor(twe.long$supergroup, levels = c("still-control-0",  "still-infected-0","turbulent-control-350" , "turbulent-infected-350", "turbulent-control-600", "turbulent-infected-600"))

resize.win (6,8)

#cellcount
cellcount <- ggplot(data=twe.long %>% 
         filter(parameter %in% c("countperml")), aes(x=day, y=log10(value+1), linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("log10 E.huxleyi"), x="days post-infection") +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +   theme(legend.position ="none")
#+ guides(linetype=guide_legend(ncol=3), colour=guide_legend(ncol=3), shape=guide_legend(ncol=3), fill=guide_legend(ncol=3)) + theme (legend.position = "top", strip.text = tweement_blank(), legend.title=tweement_blank(), plot.margin= margin(-2,2.5,0,10)) 
  

#cellcount infected
cellcountinf <- ggplot(data=twe.long %>% 
         filter(parameter=="countperml" &  group2=="infected"), 
         aes(x=day, y=log10(value+1), linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("log10 E.huxleyi"), x="days post-infection") +
  scale_shape_manual (values= c(15, 16, 17)) +
  scale_color_manual(values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_fill_manual (values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_linetype_manual(values = c("longdash", "longdash", "longdash")) +
  theme_Publication() +
  theme(legend.position ="none")

#sytox
sytox <- ggplot(data=twe.long %>% 
         filter(parameter %in% c("sytox")), aes(x=day, y=value, linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("% sytox stained"), x="days post-infection") +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +
  theme(legend.position ="none")

#sytox infected
sytoxinf <- ggplot(data=twe.long %>% 
         filter(parameter=="sytox" &  group2=="infected"), aes(x=day, y=value, linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("% sytox stained"), x="days post-infection") +
  scale_shape_manual (values= c(15, 16, 17)) +
  scale_color_manual(values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_fill_manual (values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_linetype_manual(values = c("longdash", "longdash", "longdash")) +
  theme_Publication() +
  theme(legend.position ="none") 

#fv/fm
fvfm <- ggplot(data=twe.long %>% 
         filter(parameter %in% c("fv/fm")), aes(x=day, y=value, linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("fv/fm"), x="days post-infection") +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +
  theme(legend.position ="none") 

#fv/fm infected
fvfminf <- ggplot(data=twe.long %>% 
         filter(parameter=="fv/fm" &  group2=="infected") %>%
         filter (day <=3), 
       aes(x=day, y=value, linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("fv/fm"), x="days post-infection") +
  scale_shape_manual (values= c(15, 16, 17)) +
  scale_color_manual(values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_fill_manual (values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_linetype_manual(values = c("longdash", "longdash", "longdash")) +
  theme_Publication() +
  theme(legend.position ="none") 
#dead
deadcount <- ggplot(data=twe.long %>% 
                      filter(parameter %in% c("dead")), aes(x=day, y=log10(value+1), linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("log10 dead E.huxleyi"), x="days post-infection") +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +
  theme(legend.position ="none")

#dead infected
deadcountinf <- ggplot(data=twe.long %>% 
                         filter(parameter=="dead" &  group2=="infected"), 
                       aes(x=day, y=log10(value+1), linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("log10 dead E.huxleyi" ~ " mL"^~-1), x="days post-infection") +
  scale_shape_manual (values= c(15, 16, 17)) +
  scale_color_manual(values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_fill_manual (values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_linetype_manual(values = c("longdash", "longdash", "longdash")) +
  theme_Publication() +
  theme(legend.position ="none")

resize.win(18,30)
grid.newpage()
grid.draw(rbind(ggplotGrob(cellcount), ggplotGrob(cellcountinf), ggplotGrob(sytoxinf), ggplotGrob(fvfminf), size = "first"))
require(gridExtra)
grid.arrange(cellcount, cellcountinf, sytoxinf, fvfminf, nrow = 1)

#export data
setwd("D:/R program")
require(openxlsx)
write.xlsx(twe.long, file = "Postdoc-R/Exported Tables/0910 exp 12th.xlsx")
