load("D:/R program/Postdoc-R/Workspace/Dec 2018/combine all.RData")


require(ggplot2)
require(Rmisc)
require (plotly)
source("theme_Publication2.R")
require(reshape2)
source("resizewin.R")
require(dplyr)
resize.win (12,9)
require (tidyr)

all.dropvp$countpermldiv <- all.dropvp$value/10^6


cellcount <- ggplot(data=all.dropvp %>% 
                  filter(stain %in% c("countpermldiv")), aes(x=time, y=value, linetype=supergroup2)) +
  geom_point(size=10, aes(colour=supergroup2, shape=supergroup2)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup2, fill=supergroup2), alpha=0.2, size=1.5) + 
  labs (y= expression("E.huxleyi "~ "mL"^~-1~ scriptstyle(x)~"10"^~6)) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication2() +
  #facet_grid(~group1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none")

sytox <- ggplot(data=all.dropvp %>% 
                      filter(stain %in% c("sytox")), aes(x=time, y=value, linetype=supergroup2)) +
  geom_point(size=10, aes(colour=supergroup2, shape=supergroup2)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup2, fill=supergroup2), alpha=0.2, size=1.5) + 
  labs(y="% sytox stained", x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication2() +
  #facet_grid(~group1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none")

resize.win(14,16)
grid.newpage()
grid.draw(rbind(ggplotGrob(cellcount), ggplotGrob(sytox), size = "last"))

resize.win(8.3, 6.8)

ehv<- ggplot(data=all.dropvp %>% 
               filter(cell %in% c("EhV")), aes(x=time, y=countpermldiv2, linetype=supergroup2)) +
  geom_point(size=10, aes(colour=supergroup2, shape=supergroup2)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup2, fill=supergroup2), alpha=0.2, size=1.5) + 
  labs (y= expression("EhV"~ "mL"^~-1~ scriptstyle(x)~"10"^~8),  x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication2() +
  #facet_grid(~group1) +
  theme(legend.title =element_blank())


