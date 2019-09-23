library(readxl)
ehv207 <- read_excel("Postdoc-R/Exported Tables/EhV207_7to10.xlsx")
ehv203 <- read_excel("Postdoc-R/Exported Tables/EhV203_11to12.xlsx")

require(ggplot2)
require(grid)
require(gridExtra)
require(dplyr)
source("theme_Publication.R")
source("resizewin.R")

comb <- rbind (ehv203, ehv207)

comb.drop5 <- comb %>% filter (! day < 5)

influxdata2 %>% filter (!(cell=="EhV" & maingrouptime %in% c("still-control-48", "still-control-72",
                                                             "turbulent-control-48", "turbulent-control-72",
                                                             "still-control-120", "turbulent-control-120")))


ggplot(data=comb %>% 
         filter(parameter %in% c("countperml")), aes(x=day, y=log10(value+1), linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("log10 E.huxleyi"), x="days post-infection") +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +   theme(legend.position ="none") + facet_grid(~virus)
