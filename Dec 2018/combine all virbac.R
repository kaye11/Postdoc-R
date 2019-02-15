
library(readxl)
third <- read_excel("Postdoc-R/Exported Tables/ThirdExp_virbac_plus rerun complete.xlsx")
second <- read_excel("Postdoc-R/Exported Tables/SecondExp_virbac.xlsx")
first <- read_excel("Postdoc-R/Exported Tables/FirstExp_virbac.xlsx")

require(ggplot2)
require(Rmisc)
require (plotly)
source("theme_Publication.R")
require(reshape2)
source("resizewin.R")
require(dplyr)
resize.win (12,9)
require (tidyr)

first$countpermldiv <- first$count/10^7

first$exp <- "1st"
third$exp <- "3rd"
second$exp <- "2nd"
second$Depth <- NULL
a <- first [c(5:13)]
c <- third [c(5, 7:14)]
b <-  second [c(5, 7:14)]

all <- rbind (a, b, c)

all.dropvp<- all[! all$group2=="viralparticles",  ]


ggplotly(ggplot(data=all.dropvp, aes(x=as.numeric(time), y=countpermldiv, colour=group2)) +geom_boxplot() + 
           facet_grid(cell~group1, scales="free")+ geom_point()+ theme_bw())

sum.all <- summarySE(all.dropvp, measurevar = "countpermldiv", 
                     groupvars = c("maingroup", "group1", "group2", "time", "cell"))


sum.all2 <- summarySE(all.dropvp, measurevar = "countpermldiv", 
                      groupvars = c("maingroup", "group1", "group2", "exp", "time", "cell"))

sum.all$time <- as.numeric(sum.all$time)

ggplot(data=sum.all, aes(x=time, y=countpermldiv, colour=group2)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=countpermldiv-se, ymax=countpermldiv+se, width=0.5)) + 
  facet_grid(cell~group1, scales="free")+ theme_bw()

all.dropvp$time <- as.numeric (all.dropvp$time)

bacteria <- ggplot(data=all.dropvp%>% 
                     filter(cell %in% c("Bacteria")), aes(x=time, y=countpermldiv)) +
  geom_point(size=5, aes(colour=group2, shape=exp)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2), alpha=0.2) + 
  labs(y= expression("bacteria per mL"~ scriptstyle(x)~"10"^~7), x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  theme_Publication() + 
  facet_grid(~group1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none")

virus <-ggplot(data=all.dropvp%>% 
                 filter(cell %in% c("EhV")), aes(x=time, y=countpermldiv)) +
  geom_point(size=5, aes(colour=group2, shape=exp)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2), alpha=0.2) + 
  labs(y= expression("EhV per mL"~ scriptstyle(x)~"10"^~7), x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  theme_Publication() + 
  facet_grid(~group1) +
  theme(strip.text = element_blank(), legend.title=element_blank())

resize.win(12,16)
grid.newpage()
grid.draw(rbind(ggplotGrob(bacteria), ggplotGrob(virus), size = "last"))

##what I wanted
#make new factor
all.dropvp$supergroup <- as.factor(paste(all.dropvp$maingroup, all.dropvp$exp, sep="-"))

all.dropvp$rpm <- case_when(
  all.dropvp$supergroup == "still-infected-1st"  ~ "0 rpm",
  all.dropvp$supergroup == "still-infected-2nd"  ~ "0 rpm",
  all.dropvp$supergroup == "still-infected-3rd"  ~ "0 rpm",
  all.dropvp$supergroup == "turbulent-infected-1st"  ~ "5 rpm",
  all.dropvp$supergroup == "turbulent-infected-2nd"  ~ "15 rpm",
  all.dropvp$supergroup == "turbulent-infected-3rd"  ~ "5 rpm", 
  all.dropvp$supergroup == "still-control-1st"  ~ "0 rpm",
  all.dropvp$supergroup == "still-control-2nd"  ~ "0 rpm",
  all.dropvp$supergroup == "still-control-3rd"  ~ "0 rpm",
  all.dropvp$supergroup == "turbulent-control-1st"  ~ "5 rpm",
  all.dropvp$supergroup == "turbulent-control-2nd"  ~ "15 rpm",
  all.dropvp$supergroup == "turbulent-control-3rd"  ~ "5 rpm",
  TRUE ~ as.character(all.dropvp$maingroup)
)

all.dropvp$supergroup2 <- as.factor(paste(all.dropvp$maingroup, all.dropvp$rpm, sep="-"))

all.dropvp$countpermldiv2 <- all.dropvp$count/10^8

sum.all.group <- summarySE(all.dropvp, measurevar = "countpermldiv", 
                           groupvars = c("supergroup2", "maingroup", "group1", "group2", 
                                         "time", "cell", "rpm"))

sum.all.group2 <- summarySE(all.dropvp, measurevar = "countpermldiv", 
                            groupvars = c("maingroup", "group1", "group2", "time", "cell", "exp"))

ggplotly(ggplot(data=all.dropvp, aes(x=time, y=countpermldiv, colour=supergroup2, fill=supergroup2)) +
           geom_boxplot() + 
           facet_grid(cell~., scales="free")+ geom_point()+ theme_bw())

all.dropvp$supergroup2 <- factor(all.dropvp$supergroup2, levels = c("still-control-0 rpm",  "still-infected-0 rpm","turbulent-control-5 rpm" , "turbulent-infected-5 rpm", "turbulent-control-15 rpm", "turbulent-infected-15 rpm"))

ggplot(data=all.dropvp %>% 
         filter(cell %in% c("EhV")), aes(x=time, y=countpermldiv2, linetype=supergroup2)) +
  geom_point(size=7, aes(colour=supergroup2, shape=supergroup2)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup2, fill=supergroup2), alpha=0.2, size=1.5) + 
  labs (y= expression("cells per mL"~ scriptstyle(x)~"10"^~7)) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) + 
  theme(legend.key.width=unit(3,"line"))

EhV<- ggplot(data=all.dropvp %>% 
                      filter(cell %in% c("EhV")), aes(x=time, y=countpermldiv2, linetype=supergroup2)) +
  geom_point(size=7, aes(colour=supergroup2, shape=supergroup2)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup2, fill=supergroup2), alpha=0.2, size=1.5) + 
  labs (y= expression("EhV"~ "mL"^~-1~ scriptstyle(x)~"10"^~8)) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication2() +
  #facet_grid(~group1) +
  theme(legend.title =element_blank())

resize.win(9, 5.8)
EhV

bacteria <- ggplot(data=all.dropvp %>% 
                  filter(cell %in% c("Bacteria")), aes(x=time, y=countpermldiv2, linetype=supergroup2)) +
  geom_point(size=7, aes(colour=supergroup2, shape=supergroup2)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup2, fill=supergroup2), alpha=0.2, size=1.5) + 
  labs (y= expression("bacteria"~ "mL"^~-1~ scriptstyle(x)~"10"^~8), x="hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication2() +
  #facet_grid(~group1) +
  theme(strip.text = element_blank(), legend.title=element_blank())

resize.win(14,16)
grid.newpage()
grid.draw(rbind(ggplotGrob(EhV), ggplotGrob(bacteria), size = "last"))

##all

all$countpermldiv2 <- all$count/10^8

##what I wanted
#make new factor
all$supergroup <- as.factor(paste(all$maingroup, all$exp, sep="-"))


all$rpm <- case_when(
  all$supergroup == "still-infected-1st"  ~ "0 rpm",
  all$supergroup == "still-infected-2nd"  ~ "0 rpm",
  all$supergroup == "still-infected-3rd"  ~ "0 rpm",
  all$supergroup == "turbulent-infected-1st"  ~ "5 rpm",
  all$supergroup == "turbulent-infected-2nd"  ~ "15 rpm",
  all$supergroup == "turbulent-infected-3rd"  ~ "5 rpm", 
  all$supergroup == "still-control-1st"  ~ "0 rpm",
  all$supergroup == "still-control-2nd"  ~ "0 rpm",
  all$supergroup == "still-control-3rd"  ~ "0 rpm",
  all$supergroup == "turbulent-control-1st"  ~ "5 rpm",
  all$supergroup == "turbulent-control-2nd"  ~ "15 rpm",
  all$supergroup == "turbulent-control-3rd"  ~ "5 rpm",
  all$supergroup == "still-viralparticles-1st"  ~ "0 rpm",
  all$supergroup == "still-viralparticles-2nd"  ~ "0 rpm",
  all$supergroup == "still-viralparticles-3rd"  ~ "0 rpm",
  all$supergroup == "turbulent-viralparticles-1st"  ~ "5 rpm",
  all$supergroup == "turbulent-viralparticles-2nd"  ~ "15 rpm",
  all$supergroup == "turbulent-viralparticles-3rd"  ~ "5 rpm",
  
  TRUE ~ as.character(all$maingroup)
)

all$supergroup2 <- as.factor(paste(all$maingroup, all$rpm, sep="-"))

ggplotly(ggplot(data=all, aes(x=time, y=countpermldiv, colour=supergroup2, fill=supergroup2)) +
           geom_boxplot() + 
           facet_grid(cell~., scales="free")+ geom_point()+ theme_bw())

all$supergroup2 <- factor(all$supergroup2, 
                          levels = c("still-control-0 rpm",  "still-infected-0 rpm", "still-viralparticles-0 rpm", 
                                     "turbulent-control-5 rpm" , "turbulent-infected-5 rpm", 
                                     "turbulent-viralparticles-5 rpm",
                                     "turbulent-control-15 rpm", "turbulent-infected-15 rpm",
                                     "turbulent-viralparticles-15 rpm"))

resize.win(14,15)

EhV.all <- ggplot(data=all %>% 
                    filter(cell %in% c("EhV")), aes(x=time, y=countpermldiv2, linetype=supergroup2)) +
  geom_point(size=7, aes(colour=supergroup2, shape=supergroup2)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup2, fill=supergroup2), alpha=0.2, size=1.5) + 
  labs (y= expression("EhV"~ "mL"^~-1~ scriptstyle(x)~"10"^~8)) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 7, 1, 16, 13, 2, 17, 11)) +
  scale_color_manual(values = c("#999999", "#999999", "#999999", "#E69F00", "#E69F00", "#E69F00",
                                "#56B4E9", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#999999", "#E69F00", "#E69F00", "#E69F00",
                                "#56B4E9", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("longdash", "solid", "dotted", "longdash", "solid", "dotted", 
                                   "longdash", "solid", "dotted")) + 
  theme_Publication2() + 
  theme(legend.key.width=unit(3,"line"), legend.title = element_blank()) +  
  guides(linetype=guide_legend(ncol=3), colour=guide_legend(ncol=3), shape=guide_legend(ncol=3), 
         fill=guide_legend(ncol=3) ) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none")

bacteria.all <- ggplot(data=all %>% 
                         filter(cell %in% c("Bacteria")), aes(x=time, y=countpermldiv2, linetype=supergroup2)) +
  geom_point(size=7, aes(colour=supergroup2, shape=supergroup2)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup2, fill=supergroup2), alpha=0.2, size=1.5) + 
  labs (y= expression("bacteria"~ "mL"^~-1~ scriptstyle(x)~"10"^~8), x="hrs post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 7, 1, 16, 13, 2, 17, 11)) +
  scale_color_manual(values = c("#999999", "#999999", "#999999", "#E69F00", "#E69F00", "#E69F00",
                                "#56B4E9", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#999999", "#E69F00", "#E69F00", "#E69F00",
                                "#56B4E9", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("longdash", "solid", "dotted", "longdash", "solid", "dotted", 
                                   "longdash", "solid", "dotted")) + 
  theme_Publication2() + 
  theme(legend.key.width=unit(3,"line"), legend.title = element_blank()) +  
  guides(linetype=guide_legend(ncol=3), colour=guide_legend(ncol=3), shape=guide_legend(ncol=3), 
         fill=guide_legend(ncol=3) ) +
  theme(strip.text = element_blank(), legend.title=element_blank())

resize.win(14,16)
grid.newpage()
grid.draw(rbind(ggplotGrob(EhV.all), ggplotGrob(bacteria.all), size = "last"))


#export data
setwd("D:/R program")
require(openxlsx)
write.xlsx(all, file = "Postdoc-R/Exported Tables/allexp_virbac.xlsx")
write.xlsx(all.dropvp, file = "Postdoc-R/Exported Tables/allexp_virbac minus vp.xlsx")

