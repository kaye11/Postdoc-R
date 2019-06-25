
library(readxl)
seventh <- read_excel("Postdoc-R/Exported Tables/SeventhExp_virbac.xlsx")
eight <- read_excel("Postdoc-R/Exported Tables/eightExp_virbac.xlsx")
ninth <- read_excel("Postdoc-R/Exported Tables/ninthExp_virbac.xlsx")
ninth_drop <- read_excel("Postdoc-R/Exported Tables/ninthExp_virbac_drop.xlsx")

require(ggplot2)
require(Rmisc)
require (plotly)
source("theme_Publication2.R")
require(reshape2)
source("resizewin.R")
require(dplyr)
resize.win (12,9)
require (tidyr)

#extract cell and viral count
seventh$exp <- "7th"
eight$exp <- "8th"
ninth$exp <- "9th"
ninth_drop$exp <- "9th"

seventh$rpm <- "350"
eight$rpm <- "600"
ninth$rpm <- "600"
ninth_drop$rpm <- "600"

bind_data <- rbind(seventh, eight, ninth)

still <- bind_data %>% filter (group1=="still") %>% mutate (rpm="0") 
turb <- bind_data %>% filter (group1=="turbulent")

all <- rbind(still, turb)

all$supergroup <- as.factor (paste(all$maingroup, all$rpm, sep="-"))


sum.all.group <- summarySE(all, measurevar = "countpermldiv", 
                           groupvars = c("supergroup", "maingroup", "group1", "group2", 
                                         "time", "cell", "rpm"))

sum.all.group2 <- summarySE(all, measurevar = "countpermldiv", 
                            groupvars = c("maingroup", "group1", "group2", "time", "cell", "exp"))

all$supergroup <- factor(all$supergroup, levels = c("still-control-0",  "still-infected-0","turbulent-control-350" , "turbulent-infected-350", "turbulent-control-600", "turbulent-infected-600"))


all.drop120 <- all[! all$time=="120",  ]
all.drop120.78 <- all[! all$exp=="7th" & all$exp=="8th",  ]
all.drop120.79 <- all[! all$exp=="7th" & all$exp=="9th",  ]

##what I wanted
resize.win(8,8)
ggplot(data=all.drop120 %>% 
         filter(cell=="EhV" & group2=="infected"), aes(x=time, y=log10(count), linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("log10 (EhV"~ "mL"^~-1~")"), x="hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(15, 16, 17)) +
  scale_color_manual(values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_fill_manual (values = c("#999999",  "#E69F00", "#56B4E9")) +
  scale_linetype_manual(values = c("longdash", "longdash", "longdash")) +
  theme_Publication2() +   theme(legend.position ="none")




EhV <- ggplot(data=all.drop120 %>% 
                       filter(cell %in% c("EhV")), aes(x=time, y=countpermldiv, linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("viral particles per mL"~ scriptstyle(x)~"10"^~8)) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication2() +
  #facet_grid(~group1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none")

bacteria <- ggplot(data=all.drop120 %>% 
                  filter(cell %in% c("Bacteria")), aes(x=time, y=countpermldiv, linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1) + 
  labs (y= expression("bacterial cells per mL"~ scriptstyle(x)~"10"^~8)) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication2() +
  #facet_grid(~group1) +
  theme(strip.text = element_blank(), legend.title=element_blank())

resize.win(30,35)
grid.newpage()
grid.draw(rbind(ggplotGrob(EhV), ggplotGrob(bacteria), size = "last"))

ggplot(data=all.drop120 %>% 
         filter(exp==c("8th", "9th") & maingroup=="turbulent-control"), 
       aes(x=time, y=countpermldiv, linetype=exp)) +
  geom_point(size=7, aes(colour=exp, shape=exp)) + 
  geom_smooth(method = 'loess', aes(colour=exp, fill=exp), alpha=0.2, size=1.5) + facet_wrap(~cell, scales="free")

#export data
setwd("D:/R program")
require(openxlsx)
write.xlsx(all.dropvp, file = "Postdoc-R/Exported Tables/allexp_virbac_minusvp.xlsx")



