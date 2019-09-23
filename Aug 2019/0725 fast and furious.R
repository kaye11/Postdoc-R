library(readxl)
fastfur <- read_excel("D:/Postdoc/Experiments/190725 Fast and Furious/190725 summary.xlsx")
View(fastfur)

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

fastfur$strain <- as.factor (fastfur$strain)
fastfur$time <- fastfur$day*24
fastfur$dead <- fastfur$countperml*fastfur$sytox
fastfur$sytox <- fastfur$sytox*100

#divide cellcount measurements by 10^6
fastfur$countpermldiv <- fastfur$countperml/10^6

#divide dead cells by 10^4
fastfur$deadpermldiv <- fastfur$dead/10^4

fastfur.long <- melt (data=fastfur, id.vars=c("strain", "day", "time", "rep"), variable.name="parameter")

sum.all <- summarySE(fastfur.long, measurevar = "value", groupvars = c("strain", "day", "time", "parameter"),
                     na.rm=TRUE)

scientific_10 <- scientific_10 <- function(x) {
  ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scales::scientific_format()(x)))))
}

ggplotly(ggplot(data=fastfur.long %>% filter (parameter %in% c("countpermldiv", "sytox", "deadpermldiv")) %>% filter (!(strain %in% c("control"))), aes(x=day, y=value, colour=strain)) +geom_boxplot() + 
           facet_grid(parameter~., scales="free")+ geom_point()+ theme_bw())

sum.all.group <- summarySE(fastfur.long, measurevar = "value", 
                           groupvars = c("strain", "day", "parameter"), na.rm = TRUE)

#combine all parameters

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7", "#0072B2",  "#F0E442")

ggplot(data=fastfur.long %>% 
         filter(parameter %in% c("countpermldiv", "sytox", "deadpermldiv")) %>% 
         filter (!(strain %in% c("control"))), 
       aes(x=day, y=value, colour=strain, shape=strain)) +
  geom_point(size=5) + 
  geom_smooth(method = 'loess', aes(colour=strain, fill=strain)) + 
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120, 144, 168, 192 )) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual (values=cbPalette)  +
  facet_wrap(.~parameter, scales="free")+ 
  theme_Publication()

#plotting
resize.win(6,6)

ggplot(data=fastfur.long %>% 
         filter(parameter %in% c("sytox")),
       aes(x=day, y=value, colour=strain, shape=strain)) +
  geom_point(size=5) + 
  geom_smooth(method = 'loess', aes(colour=strain, fill=strain)) + 
  scale_x_continuous(breaks=c(0, 2, 4, 6, 8, 10, 12 )) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual (values=cbPalette)  +
  theme_Publication() + 
  labs(y="% sytox stained", x= "days post-infection") 

ggplot(data=fastfur.long %>% 
         filter(parameter %in% c("countperml")),
       aes(x=day, y=log10(value), colour=strain, shape=strain)) +
  geom_point(size=5) + 
  geom_smooth(method = 'loess', aes(colour=strain, fill=strain)) + 
  scale_x_continuous(breaks=c(0, 2, 4, 6, 8, 10, 12 )) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual (values=cbPalette)  +
  theme_Publication() + 
  labs(y= "log10 E.huxleyi" , x= "days post-infection") +
  theme_Publication()

#export data
setwd("D:/R program")
require(openxlsx)
write.xlsx(fastfur.long, file = "Postdoc-R/Exported Tables/0725 fastfur.xlsx")
