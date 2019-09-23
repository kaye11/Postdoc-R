library(readxl)
el <- read_excel("D:/Postdoc/Experiments/190820 EhV203 Eleventh/190820 EhV203 Eleventh summary.xlsx")
View(el)

require(ggplot2)
require(Rmisc)
require(data.table)

el$treatment <- as.factor (el$treatment)
el$dayf <- as.factor(el$day)
el$dead <- el$countperml*el$sytox
el$sytox <- el$sytox*100

require (plotly)
source("theme_Publication.R")
require(reshape2)
source("resizewin.R")
require(dplyr)
resize.win (12,9)

#divide cellcount measurements by 10^5
el$countpermldiv <- el$countperml/10^5

#divide dead cells by 10^3
el$deadpermldiv <- el$dead/10^3

el.long <- melt (data=el, id.vars=c("treatment", "day", "dayf", "rep"), variable.name="parameter")

el.long$group <- case_when(
  el.long$treatment =="sc"  ~ "still control",
  el.long$treatment =="si" ~ "still infected",
  el.long$treatment =="tc"  ~ "turbulent control",
  el.long$treatment =="ti" ~ "turbulent infected",
  TRUE ~ as.character(el.long$treatment)
)

require(tidyr)

el.long<- separate(el.long, group, into = paste("group", 1:2, sep = ""))
el.long$maingroup <- as.factor(paste(el.long$group1, el.long$group2, sep="-" ))

el.long$maingroup <- factor(el.long$maingroup,
                               levels = c("still-control", "still-infected", 
                                          "turbulent-control", "turbulent-infected"),
                               labels = c("still-control", "still-infected", 
                                          "turbulent-control", "turbulent-infected"))

el.long$group1 <- as.factor(el.long$group1)
el.long$group2 <- as.factor(el.long$group2)

#change rpms
el.long$rpm <- "600"
el.long[el.long$rep %in% c('tc1', 'tc2', 'ti1', 'ti2'), ]$rpm <- "350"
el.long[el.long$group1 %in% c('still'), ]$rpm <- "0"

el.long$supergroup <- as.factor (paste(el.long$maingroup, el.long$rpm, sep="-"))
el.long$value <- as.numeric (el.long$value)

#use all data
sum.all <- summarySE(el.long, measurevar = "value", 
                     groupvars = c("treatment", "dayf", "day", "parameter", "rpm", "supergroup"),
                     na.rm=TRUE)

##figure out why your summary SE is producing NAs

scientific_10 <- scientific_10 <- function(x) {
  ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scales::scientific_format()(x)))))
}

ggplotly(ggplot(data=el.long %>% filter (parameter %in% c("countpermldiv", "sytox", "deadpermldiv")), 
                aes(x=day, y=value, colour=supergroup)) +geom_boxplot() + 
           facet_grid(parameter~supergroup, scales="free")+ geom_point()+ theme_bw())

ggplotly(ggplot(data=el.long %>% filter (parameter %in% c("countperml", "sytox")) %>%
                  filter (group2 %in% c("infected")), 
                aes(x=day, y=value, colour=supergroup)) +geom_boxplot() + 
           facet_grid(parameter~supergroup, scales="free")+ geom_point()+ theme_bw())

ggplot(data=sum.all %>% filter (parameter %in% c("countpermldiv", "sytox")) %>% 
         filter (treatment %in% c("si", "ti")), aes(x=day, y=value, colour=supergroup)) +
  geom_point(size=5, position = position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se), position = position_dodge(width=0.5)) + theme_bw()+
  geom_smooth(method="loess") + facet_grid(parameter~., scales="free")

sum.all.group <- summarySE(el.long, measurevar = "value", 
                           groupvars = c("supergroup", "maingroup", "group1", "group2", 
                                         "day", "rpm", "parameter"), na.rm = TRUE)

cbPalette <- c("#0072B2", "#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7", "#F0E442")

resize.win (12,9)

el.long$supergroup <- factor(el.long$supergroup, levels = c("still-control-0",  "still-infected-0","turbulent-control-350" , "turbulent-infected-350", "turbulent-control-600", "turbulent-infected-600"))

resize.win (6,8)

#cellcount
cellcount <- ggplot(data=el.long %>% 
         filter(parameter %in% c("countperml")), aes(x=day, y=log10(value+1), linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("log10 E.huxleyi"), x="days post-infection") +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +   theme(legend.position ="none")
#+ guides(linetype=guide_legend(ncol=3), colour=guide_legend(ncol=3), shape=guide_legend(ncol=3), fill=guide_legend(ncol=3)) + theme (legend.position = "top", strip.text = element_blank(), legend.title=element_blank(), plot.margin= margin(-2,2.5,0,10)) 
  

#cellcount infected
cellcountinf <- ggplot(data=el.long %>% 
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
sytox <- ggplot(data=el.long %>% 
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
sytoxinf <- ggplot(data=el.long %>% 
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
fvfm <- ggplot(data=el.long %>% 
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
fvfminf <- ggplot(data=el.long %>% 
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
deadcount <- ggplot(data=el.long %>% 
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
deadcountinf <- ggplot(data=el.long %>% 
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
write.xlsx(el.long, file = "Postdoc-R/Exported Tables/0820 exp 11th.xlsx")
