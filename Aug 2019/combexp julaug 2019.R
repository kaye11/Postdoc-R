library(readxl)
fastfur <- read_excel("Postdoc-R/Exported Tables/0725 fastfur.xlsx")
fightclub <- read_excel("Postdoc-R/Exported Tables/0731 fightclub.xlsx")
ehv201 <- read_excel("Postdoc-R/Exported Tables/0724 ehv201.xlsx")
ehv203 <- read_excel("Postdoc-R/Exported Tables/0809 ehv203.xlsx")

fastfur$exp <- "fastfur"
fightclub$exp <- "fightclub"
ehv201$exp <- "ehv201"
ehv203$exp <- "ehv203"

#combine all
allexp <- rbind(fastfur, fightclub, ehv201, ehv203)

require(ggplot2)
require(Rmisc)
require(data.table)
require (plotly)
source("theme_Publication.R")
require(reshape2)
source("resizewin.R")
require(dplyr)
require(viridis)

cbPalette <- c("#0072B2", "#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7", "#F0E442")

resize.win (12,9)
ggplot(data=allexp %>% 
         filter(parameter %in% c("sytox")) %>%
         filter (day <=16),
       aes(x=day, y=value, colour=strain)) +
  geom_point(size=3) + 
  geom_smooth(method = 'loess', aes(colour=strain, fill=strain)) + 
  scale_x_continuous(breaks=c(seq(0,22,4))) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual (values=cbPalette)  +
  theme_Publication() + facet_grid (~strain) +
  labs(y="% sytox stained", x= "days post-infection") 

ggplot(data=allexp %>% 
         filter(parameter %in% c("countperml")) %>%
         filter (day <=16),
       aes(x=day, y=log10(value+1), colour=strain)) +
  geom_point(size=3) + 
  geom_smooth(method = 'loess', aes(colour=strain, fill=strain)) + 
  scale_x_continuous(breaks=c(seq(0,22,4))) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual (values=cbPalette)  +
  theme_Publication() + facet_grid (~strain) +
  labs(y= "log10 E.huxleyi" , x= "days post-infection") +
  theme_Publication()
  