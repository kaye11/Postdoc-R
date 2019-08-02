
library(readxl)
setwd("D:/R program")
seventh <- read_excel("D:/R program/Postdoc-R/Exported Tables/seventhExp_sytox_standard.xlsx")
eight <- read_excel("D:/R program/Postdoc-R/Exported Tables/eightExp_sytox_standard.xlsx")
ninth <- read_excel("D:/R program/Postdoc-R/Exported Tables/ninthExp_sytox_standard.xlsx")
tenth <- read_excel("D:/R program/Postdoc-R/Exported Tables/tenthExp_sytox_standard.xlsx")

require(ggplot2)
require(Rmisc)
require (plotly)
source("theme_Publication.R")
require(reshape2)
source("resizewin.R")
require(dplyr)
resize.win (12,9)
require (tidyr)
require(grid)
require (gridExtra)

#extract cell and viral count
seventh$exp <- "7th"
eight$exp <- "8th"
ninth$exp <- "9th"
tenth$exp <- "10th"

seventh$rpm <- "350"
eight$rpm <- "600"
ninth$rpm <- "600"
tenth$rpm <- "350"

bind_data <- rbind(seventh, eight, ninth, tenth)

still <- bind_data %>% filter (group1=="still") %>% mutate (rpm="0") 
turb <- bind_data %>% filter (group1=="turbulent")

all <- rbind(still, turb)

all$supergroup <- as.factor (paste(all$supergroup2, all$rpm, sep="-"))


sum.all.group <- summarySE(all, measurevar = "value", 
                           groupvars = c("supergroup", "supergroup2", "group1", "group2", 
                                         "time", "parameter", "rpm"))

sum.all.group2 <- summarySE(all, measurevar = "value", 
                            groupvars = c("supergroup2", "group1", "group2", "time", "parameter", "exp"))

all$supergroup <- factor(all$supergroup, levels = c("still-control-0",  "still-infected-0","turbulent-control-350" , "turbulent-infected-350", "turbulent-control-600", "turbulent-infected-600"))

all$supergroup2 <- factor(all$supergroup, levels = c("still-control-0",  "still-infected-0","turbulent-control-350" , "turbulent-infected-350", "turbulent-control-600", "turbulent-infected-600"), labels= c("still-control",  "still-infected","mid-control" , "mid-infected", "stormy-control", "stormy-infected"))

all$exp <- factor(all$exp, levels = c("7th", "8th", "9th","10th" ))

##what I wanted
ggplot(data=all %>% 
         filter(parameter %in% c("sytox")), aes(x=time, y=value, linetype=supergroup2)) +
  geom_point(size=7, aes(colour=supergroup2, shape=supergroup2)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup2, fill=supergroup2), alpha=0.2, size=1.5) + 
  labs (y= expression("E. huxleyi "~ "mL"^~-1~ scriptstyle(x)~"10"^~6)) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) + 
  theme(legend.key.width=unit(3,"line"), legend.position = "bottom") + theme_Publication()

all.drop120 <- all[! all$time=="120",  ]

resize.win(15,12)
ggplot(data=all.drop120 %>% 
         filter(parameter %in% c("countpermldiv", "sytox", "deadpermldiv", "fv/fm")), aes(x=timef, y=value)) +
  geom_boxplot (aes(colour=supergroup2)) +
  geom_point(size=2.5, aes(colour=supergroup2)) +
  theme_Publication() + facet_grid (parameter~supergroup2, scales="free") + labs (x= "hrs post-infection") 


cellcount <- ggplot(data=all.drop120 %>% 
                      filter(parameter %in% c("countpermldiv")), aes(x=time, y=value, linetype=supergroup2)) +
  geom_point(size=7, aes(colour=supergroup2, shape=supergroup2)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup2, fill=supergroup2), alpha=0.2, size=1.5) + 
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

fvfm <- ggplot(data=all.drop120 %>% 
                 filter(parameter %in% c("fv/fm")), aes(x=time, y=value, linetype=supergroup2)) +
  geom_point(size=7, aes(colour=supergroup2, shape=supergroup2)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup2, fill=supergroup2), alpha=0.2, size=1.5) + 
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

sytox <- ggplot(data=all.drop120 %>% 
                  filter(parameter %in% c("sytox")), aes(x=time, y=value, linetype=supergroup2)) +
  geom_point(size=7, aes(colour=supergroup2, shape=supergroup2)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup2, fill=supergroup2), alpha=0.2, size=1) + 
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

dead <- ggplot(data=all.drop120 %>% 
                 filter(parameter %in% c("deadpermldiv")), aes(x=time, y=value, linetype=supergroup2)) +
  geom_point(size=7, aes(colour=supergroup2, shape=supergroup2)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup2, fill=supergroup2), alpha=0.2, size=1) + 
  labs (y= expression("dead cells"~ "mL"^~-1~ scriptstyle(x)~"10"^~3), x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +
  #facet_grid(~group1) +
  theme(strip.text = element_blank(), legend.title=element_blank(), plot.margin= margin(-2,2.5,0,10)) +
  guides(linetype=guide_legend(ncol=3), colour=guide_legend(ncol=3), shape=guide_legend(ncol=3), 
         fill=guide_legend(ncol=3))

resize.win(18,30)
grid.newpage()
grid.draw(rbind(ggplotGrob(cellcount), ggplotGrob(fvfm), ggplotGrob(sytox), ggplotGrob(dead), size = "last"))

ggplot(data=all.drop120 %>% 
         filter(parameter=="countperml" & supergroup2=="still-infected"), aes(x=time, y=value, linetype=exp)) +
  geom_point(size=7, aes(colour=exp, shape=exp)) + 
  geom_smooth(method = 'loess', aes(colour=exp, fill=exp), alpha=0.2, size=1.5)

#cellcount
resize.win(6,9)
ggplot(data=all.drop120 %>% 
         filter(parameter %in% c("countpermldiv")), aes(x=time, y=value, linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("E.huxleyi"~ " mL"^~-1~ scriptstyle(x)~"10"^~6), x="hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +
  #facet_grid(~group1) +
  theme(legend.position ="none")

#cellcount infected
ggplot(data=all.drop120 %>% 
         filter(parameter=="countpermldiv" &  group2=="infected"), aes(x=time, y=value, linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("E.huxleyi"~ " mL"^~-1~ scriptstyle(x)~"10"^~6), x="hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(15, 16, 17)) +
  scale_color_manual(values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_fill_manual (values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_linetype_manual(values = c("longdash", "longdash", "longdash")) +
  theme_Publication2() +
  #facet_grid(~supergroup1) +
  theme(legend.position ="none")

deadtab <- all.drop120 %>% filter(parameter=="dead")  %>% mutate (deadln = log10 (value))

ggplot(data=deadtab %>%
         filter (rpm=="600" & group2=="infected" ), 
       aes(x=time, y=deadln, linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=exp)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("log dead cells"), x="hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  theme_Publication() +
  #facet_grid(~supergroup1) +
  theme(legend.title=element_blank()) 

#sytox
ggplot(data=all.drop120 %>% 
         filter(parameter %in% c("sytox")), aes(x=time, y=value, linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs(y="% dead cells", x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +
  #facet_grid(~group1) +
  theme(legend.position ="none")

#sytox infected
ggplot(data=all.drop120 %>% 
         filter(parameter=="sytox" & group2=="infected"), aes(x=time, y=value, linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs(y="% dead cells", x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(15, 16, 17)) +
  scale_color_manual(values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_fill_manual (values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_linetype_manual(values = c("longdash", "longdash", "longdash")) +
  theme_Publication() +
  #facet_grid(~supergroup1) +
  theme(legend.position ="none")



#fv/fm
ggplot(data=all.drop120 %>% 
         filter(parameter %in% c("fv/fm")), aes(x=time, y=value, linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs(y="fv/fm", x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +
  #facet_grid(~group1) +
  theme(legend.position ="none")

#fv/fm infected
ggplot(data=all.drop120 %>% 
         filter(parameter=="fv/fm" & group2=="infected"), aes(x=time, y=value, linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs(y="fv/fm", x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(15, 16, 17)) +
  scale_color_manual(values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_fill_manual (values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_linetype_manual(values = c("longdash", "longdash", "longdash")) +
  theme_Publication() +
  #facet_grid(~supergroup1) +
  theme(legend.position ="none")

