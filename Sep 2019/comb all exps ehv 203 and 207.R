library(readxl)
ehv207 <- read_excel("Postdoc-R/Exported Tables/EhV207_7to10.xlsx")
ehv203 <- read_excel("Postdoc-R/Exported Tables/EhV203_11to12.xlsx")
ehv207vir <- read_excel("Postdoc-R/Exported Tables/comb_7thto9th.xlsx")
ehv203vir <- read_excel("Postdoc-R/Exported Tables/EleventhExp_virbac.xlsx")

require(ggplot2)
require(grid)
require(gridExtra)
require(dplyr)
source("theme_Publication.R")
source("resizewin.R")

comb <- rbind (ehv203, ehv207)
comb.drop5 <- comb[! comb$day>5,  ]
resize.win (4,8)
ehv203vir <- select (ehv203vir, -c (flowrate))
ehv207vir$day <- ehv207vir$time/24
ehv207vir$maingroup <- ehv207vir$supergroup
ehv207vir <- select (ehv207vir,-c(time, supergroup))
ehv207vir$virus <- "EhV 207"
ehv203vir$virus <- "Ehv 203"
ehv203vir$exp <- "11th"
combvir <- rbind (ehv203vir, ehv207vir)
combvir.drop5 <- combvir[! combvir$day>5,  ]

#export data
setwd("D:/R program")
require(openxlsx)
write.xlsx(comb, file = "Postdoc-R/Exported Tables/Ehux7thto12th.xlsx")
write.xlsx(combvir, file = "Postdoc-R/Exported Tables/EhV7thto12th.xlsx")

ggplot(data=comb.drop5 %>% 
         filter(parameter %in% c("countperml")), aes(x=day, y=log10(value+1), linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("log10 E.huxleyi"), x="days post-infection") +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +   theme(legend.position ="none") + facet_grid(virus~.)

#cellcount
cellcount <- ggplot(data=comb.drop5 %>% 
                      filter(parameter %in% c("countperml")), aes(x=day, y=log10(value+1), linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("log10+1 E.huxleyi"), x="days post-infection") +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9"))+
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +   theme(legend.position ="none", strip.text.y = element_blank()) + facet_grid (virus~.)
#+ guides(linetype=guide_legend(ncol=3), colour=guide_legend(ncol=3), shape=guide_legend(ncol=3), fill=guide_legend(ncol=3)) + theme (legend.position = "top", strip.text = element_blank(), legend.title=element_blank(), plot.margin= margin(-2,2.5,0,10)) 


#cellcount infected
cellcountinf <- ggplot(data=comb.drop5 %>% 
                         filter(parameter=="countperml" &  group2=="infected"), 
                       aes(x=day, y=log10(value+1), linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("log10+1 E.huxleyi"), x="days post-infection") +
  scale_shape_manual (values= c(15, 16, 17)) +
  scale_color_manual(values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_fill_manual (values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_linetype_manual(values = c("longdash", "longdash", "longdash")) +
  theme_Publication() +
  theme(legend.position ="none") + theme(legend.position ="none", strip.text.y = element_blank()) + 
  facet_grid (virus~.)

#sytox infected
sytoxinf <- ggplot(data=comb.drop5 %>% 
                     filter(parameter=="sytox" &  group2=="infected"), aes(x=day, y=value, linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("% sytox stained"), x="days post-infection") +
  scale_shape_manual (values= c(15, 16, 17)) +
  scale_color_manual(values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_fill_manual (values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_linetype_manual(values = c("longdash", "longdash", "longdash")) +
  theme_Publication() +
  theme(legend.position ="none") + theme(legend.position ="none", strip.text.y = element_blank()) + 
  facet_grid (virus~.)


#fv/fm infected
fvfminf <- ggplot(data=comb.drop5 %>% 
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
  theme(legend.position ="none", strip.text.y = element_blank()) + 
  facet_grid (virus~.)

resize.win(18,9)
grid.newpage()
require(gridExtra)
grid.arrange(cellcount, cellcountinf, sytoxinf, fvfminf, nrow = 1)

##viruses
ggplot(data=combvir.drop5 %>% 
         filter(group2=="infected"), 
       aes(x=day, y=log10(count), linetype=maingroup)) +
  geom_point(size=7, aes(colour=maingroup, shape=maingroup)) + 
  geom_smooth(method = 'loess', aes(colour=maingroup, fill=maingroup), alpha=0.2, size=1.5) + 
  labs (y= expression("log10 EhVs"), x="days post-infection") +
  scale_shape_manual (values= c(15, 16, 17)) +
  scale_color_manual(values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_fill_manual (values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_linetype_manual(values = c("longdash", "longdash", "longdash")) +
  theme_Publication() +
  theme(legend.position ="none") + 
  facet_grid (virus~.)

