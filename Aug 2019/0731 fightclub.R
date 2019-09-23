library(readxl)
fightclub <- read_excel("D:/Postdoc/Experiments/190731 Fight Club/summary.xlsx")
View(fightclub)

require(ggplot2)
require(Rmisc)
require(data.table)
require (plotly)
source("theme_Publication.R")
require(reshape2)
source("resizewin.R")
require(dplyr)
resize.win (12,9)
require(viridis)

fightclub$strain <- as.factor (fightclub$strain)
fightclub$day <- fightclub$time/24
fightclub$dead <- fightclub$countperml*fightclub$sytox
fightclub$sytox <- fightclub$sytox*100

#divide cellcount measurements by 10^6
fightclub$countpermldiv <- fightclub$countperml/10^6

#divide dead cells by 10^4
fightclub$deadpermldiv <- fightclub$dead/10^4

fightclub.long <- melt (data=fightclub, id.vars=c("strain", "time", "day", "rep"), variable.name="parameter")

sum.all <- summarySE(fightclub.long, measurevar = "value", groupvars = c("strain", "time", "day", "parameter"),
                     na.rm=TRUE)

scientific_10 <- scientific_10 <- function(x) {
  ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scales::scientific_format()(x)))))
}

ggplotly(ggplot(data=fightclub.long, aes(x=time, y=value, colour=strain)) +geom_boxplot() + 
           facet_grid(parameter~strain, scales="free")+ geom_point()+ theme_bw())

ggplotly(ggplot(data=fightclub.long %>% filter (parameter %in% c("countpermldiv", "sytox", "deadpermldiv")) %>% filter (!(strain %in% c("control"))), aes(x=day, y=value, colour=strain)) +geom_boxplot() + 
           facet_grid(parameter~., scales="free")+ geom_point()+ theme_bw())

sum.all.group <- summarySE(fightclub.long, measurevar = "value", 
                           groupvars = c("strain", "time", "day", "parameter"), na.rm = TRUE)

#combine all parameters

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7", "#0072B2",  "#F0E442")

ggplot(data=fightclub.long %>% 
         filter(parameter %in% c("countpermldiv", "sytox", "deadpermldiv")) %>% 
         filter (!(strain %in% c("control"))), 
       aes(x=time, y=value, colour=strain, shape=strain)) +
  geom_point(size=5) + 
  geom_smooth(method = 'loess', aes(colour=strain, fill=strain)) + 
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120, 144, 168, 192 )) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual (values=cbPalette)  +
  facet_wrap(.~parameter, scales="free")+ 
  theme_Publication()

#plot 1 by 1
sum.all.group$parameter2 <- factor(sum.all.group$parameter, levels=c("countperml", "sytox", "deadpermldiv"), 
                                   labels =c ("Cell count", "%Sytox stained", "Dead cell count"))

#cellcount
cellcount <- ggplot(data=fightclub.long %>% 
         filter(parameter %in% c("countpermldiv")) %>% 
         filter (!(strain %in% c("control"))), 
       aes(x=time/24, y=value, colour=strain, shape=strain)) +
  geom_point(size=5) + 
  geom_smooth(method = 'loess', aes(colour=strain, fill=strain)) + 
  #scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120, 144, 168, 192 )) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual (values=cbPalette)  +
  theme_Publication()+
  labs (y= expression("E.huxleyi "~ "mL"^~-1~ scriptstyle(x)~"10"^~5)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none",
        axis.title.y = element_text(vjust=5), plot.margin= margin(5,2.5,8,10))
  
sytox <- ggplot(data=fightclub.long %>% 
                      filter(parameter %in% c("sytox")) %>% 
                      filter (!(strain %in% c("control"))), 
                    aes(x=time/24, y=value, colour=strain, shape=strain)) +
  geom_point(size=5) + 
  geom_smooth(method = 'loess', aes(colour=strain, fill=strain)) + 
  #scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120, 144, 168, 192 )) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual (values=cbPalette)  +
  theme_Publication()+
  labs(y="% sytox stained", x= "days post-infection") +
  theme(legend.title=element_blank(), plot.margin= margin(-2,2.5,0,10)) 

deadcount <- ggplot(data=fightclub.long %>% 
                      filter(parameter %in% c("deadpermldiv")) %>% 
                      filter (!(strain %in% c("control"))), 
                    aes(x=time, y=value, colour=strain, shape=strain)) +
  geom_point(size=5) + 
  geom_smooth(method = 'loess', aes(colour=strain, fill=strain)) + 
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120, 144, 168, 192 )) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual (values=cbPalette)  +
  theme_Publication()+
  labs (y= expression("dead cells"~ "mL"^~-1~ scriptstyle(x)~"10"^~3), x= "hours post-infection") +  
  theme(legend.title=element_blank(), plot.margin= margin(-2,2.5,0,10)) 

resize.win(20,30)
grid.newpage()
grid.draw(rbind(ggplotGrob(cellcount),ggplotGrob(sytox), size = "last"))

ggplot(data=fightclub.long %>% 
         filter(parameter %in% c("sytox")),
       aes(x=day, y=value, colour=strain)) +
  geom_point(size=3) + 
  geom_smooth(method = 'loess', aes(colour=strain, fill=strain)) + 
  scale_x_continuous(breaks=c(0, 2, 4, 6, 8, 10, 12 )) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual (values=cbPalette)  +
  theme_Publication() + facet_grid (~strain) +
  labs(y="% sytox stained", x= "days post-infection") 

ggplot(data=fightclub.long %>% 
         filter(parameter %in% c("countperml")),
       aes(x=day, y=log10(value+1), colour=strain)) +
  geom_point(size=3) + 
  geom_smooth(method = 'loess', aes(colour=strain, fill=strain)) + 
  scale_x_continuous(breaks=c(0, 2, 4, 6, 8, 10, 12 )) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual (values=cbPalette)  +
  theme_Publication() + facet_grid (~strain) +
  labs(y= "log10 E.huxleyi" , x= "days post-infection") +
  theme_Publication()

#export data
setwd("D:/R program")
require(openxlsx)
write.xlsx(fightclub.long, file = "Postdoc-R/Exported Tables/0731 fightclub.xlsx")


#203 vs 207, 86 vs 203
resize.win(6,4)
ggplot(data=fightclub.long %>% 
         filter(parameter %in% c("sytox")) %>% 
         filter (!(strain %in% c("control"))) %>% filter (strain %in% c("203")), 
       aes(x=time/24, y=value, colour=strain, shape=strain)) +
  geom_point(size=5) + 
  geom_smooth(method = 'loess', aes(colour=strain, fill=strain)) + 
  #scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120, 144, 168, 192 )) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual (values=cbPalette)  +
  theme_Publication() + 
  labs(y= "%Sytox stained", x= "days post-infection") 

ggplot(data=fightclub.long %>% 
         filter(parameter %in% c("sytox")) %>% 
         filter (!(strain %in% c("control"))) %>% filter (strain %in% c("203", "86")), 
       aes(x=time/24, y=value, colour=strain, shape=strain)) +
  geom_point(size=5) + 
  geom_smooth(method = 'loess', aes(colour=strain, fill=strain)) + 
  #scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120, 144, 168, 192 )) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual (values=cbPalette)  +
  theme_Publication() + 
  labs(y= expression("E.huxleyi "~ "mL"^~-1~ scriptstyle(x)~"10"^~5), x= "hours post-infection") 
