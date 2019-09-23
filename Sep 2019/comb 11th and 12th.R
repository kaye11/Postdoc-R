#read data
library(readxl)
el <- read_excel("Postdoc-R/Exported Tables/0820 exp 11th.xlsx")
twe <- read_excel("Postdoc-R/Exported Tables/0910 exp 12th.xlsx")
fightclub <- read_excel("Postdoc-R/Exported Tables/0731 fightclub.xlsx")

#combine dataset (el and twe)
el$exp <- "11th"
twe$exp <- "12th"
comb <- rbind(el, twe)

require(ggplot2)
require(grid)
require(gridExtra)
require(dplyr)
source("theme_Publication.R")
source("resizewin.R")

comb$supergroup <- factor(comb$supergroup, levels = c("still-control-0",  "still-infected-0","turbulent-control-350" , "turbulent-infected-350", "turbulent-control-600", "turbulent-infected-600"))

comb$supergroup2 <- factor(comb$supergroup, levels = c("still-control-0",  "still-infected-0","turbulent-control-350" , "turbulent-infected-350", "turbulent-control-600", "turbulent-infected-600"), labels= c("still-control",  "still-infected","mid-control" , "mid-infected", "stormy-control", "stormy-infected"))

comb$virus <- "EhV 203"

require(openxlsx)
write.xlsx(comb, file = "Postdoc-R/Exported Tables/EhV203_11to12.xlsx")

#cellcount
cellcount <- ggplot(data=comb %>% 
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
cellcountinf <- ggplot(data=comb %>% 
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
sytox <- ggplot(data=comb %>% 
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
sytoxinf <- ggplot(data=comb %>% 
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
fvfm <- ggplot(data=comb %>% 
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
fvfminf <- ggplot(data=comb %>% 
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


resize.win(18,30)
grid.newpage()
grid.draw(rbind(ggplotGrob(cellcount), ggplotGrob(cellcountinf), ggplotGrob(sytoxinf), ggplotGrob(fvfminf), size = "first"))
require(gridExtra)
grid.arrange(cellcount, cellcountinf, sytoxinf, fvfminf, nrow = 1)




###-------------------STOP HERE------------------------###

#shift 11th data to plus 1
elshift <- el
elshift$day <- elshift$day + 1

combshift <- rbind(elshift, twe)
#remove 8th day
combshift <- combshift%>% 
  filter(!(day %in% c("8")))

#graph shifted data
#cellcount
cellcountshift <- ggplot(data=combshift %>% 
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
cellcountinfshift <- ggplot(data=combshift %>% 
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
sytoxshift <- ggplot(data=combshift %>% 
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
sytoxinfshift <- ggplot(data=combshift %>% 
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
fvfmshift <- ggplot(data=combshift %>% 
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
fvfminfshift <- ggplot(data=combshift %>% 
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


resize.win(18,30)
grid.newpage()
require(gridExtra)
grid.arrange(cellcountshift, cellcountinfshift, sytoxinfshift, fvfminfshift, nrow = 1)


