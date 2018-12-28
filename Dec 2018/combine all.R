
library(readxl)
third <- read_excel("Postdoc-R/Exported Tables/ThirdExp_sytox.xlsx")
second <- read_excel("Postdoc-R/Exported Tables/SecondExp_sytox.xlsx")
first <- read_excel("Postdoc-R/Exported Tables/FirstExp_sytox.xlsx")

require(ggplot2)
require(Rmisc)
require (plotly)
source("theme_Publication.R")
require(reshape2)
source("resizewin.R")
require(dplyr)
resize.win (12,9)
require (tidyr)

#extract cell and viral count
first.filt<- first %>% filter(stain %in% c("countperml", "sytox"))
first.filt$exp <- "1st"
third$exp <- "3rd"
second$exp <- "2nd"

all <- rbind(first.filt, second, third)
all.dropvp <- all[! all$group2=="viralparticles",  ]

sum.all.group <- summarySE(all.dropvp, measurevar = "value", 
                           groupvars = c("supergroup2", "maingroup", "group1", "group2", 
                                         "time", "stain", "rpm"))

sum.all.group2 <- summarySE(alldropvp, measurevar = "value", 
                            groupvars = c("maingroup", "group1", "group2", "time", "stain", "exp"))

all.dropvp$supergroup2 <- factor(all.dropvp$supergroup2, levels = c("still-control-0 rpm",  "still-infected-0 rpm","turbulent-control-5 rpm" , "turbulent-infected-5 rpm", "turbulent-control-15 rpm", "turbulent-infected-15 rpm"))



##what I wanted
ggplot(data=all.dropvp %>% 
         filter(stain %in% c("sytox")), aes(x=time, y=value, linetype=supergroup2)) +
  geom_point(size=5, aes(colour=supergroup2, shape=supergroup2)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup2, fill=supergroup2), alpha=0.2) + 
  labs (y= expression("E. huxleyi"~ "mL"^~-1~ scriptstyle(x)~"10"^~6)) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) + 
  theme(legend.key.width=unit(3,"line"))

cellcount <- ggplot(data=all.dropvp %>% 
                       filter(stain %in% c("countpermldiv")), aes(x=time, y=value, linetype=supergroup2)) +
  geom_point(size=5, aes(colour=supergroup2, shape=supergroup2)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup2, fill=supergroup2), alpha=0.2) + 
  labs (y= expression("E.huxleyi"~ "mL"^~-1~ scriptstyle(x)~"10"^~6)) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +
  #facet_grid(~group1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none")

sytox <- ggplot(data=all.dropvp %>% 
                  filter(stain %in% c("sytox")), aes(x=time, y=value, linetype=supergroup2)) +
  geom_point(size=5, aes(colour=supergroup2, shape=supergroup2)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup2, fill=supergroup2), alpha=0.2) + 
  labs(y="% sytox stained", x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication() +
  #facet_grid(~group1) +
  theme(strip.text = element_blank(), legend.title=element_blank())

resize.win(12,16)
grid.newpage()
grid.draw(rbind(ggplotGrob(cellcount), ggplotGrob(sytox), size = "last"))


#export data
setwd("D:/R program")
require(openxlsx)
write.xlsx(all.dropvp, file = "Postdoc-R/Exported Tables/allexp_sytox_minusvp.xlsx")



#calculate alpha

#1. calculate growth rate
#compute some parameters
#Two points, N1 and N2,  at the extremes of this linear phase (see fig below) are taken and substituted into the equation
# Growth rate ;  K' = Ln (N2 / N1) / (t2 - t1)
# Divisions per day ; Div.day-1 = K' / Ln2
# Generation time ; Gen' t  = 1 / Div.day-1

countperml.data <- all.dropvp %>% 
  filter(stain %in% c("countperml"))

gcdata <- countperml.data %>% 
  filter(group2 %in% c("control"))

gcdata$lncellsR <- log(countperml.data$value)

NT = data.table (countperml.data,  key=c("supergroup" ))


GC.comp=NT [,list(K= (lncellsR[match("day7", day)]/lncellsR [match ("day0", day)])/7),   #divide according to number of days
            by=c("medtreatreplet")]

GC.comp [,div.day:=K/log(2)] #this is always 2
GC.comp [, genT:= 1/div.day]

GC.comp <- na.omit(GC.comp)




#rough stats

all.dropvp$ID <- as.factor(paste(all.dropvp$supergroup, all.dropvp$rep, sep="-"))

Form <- formula (value~ supergroup2)
sytox.gls<- gls(Form, data=all.dropvp %>% 
                filter(stain %in% c("sytox")))

sytoxdata <- all.dropvp %>% 
  filter(stain %in% c("sytox"))
                      
plot(residuals(sytox.gls))

sytox.lme1 <- lme (Form, random= ~1|ID, method="REML", sytoxdata)

sytox.gamm0 <- gamm (value~s(time, by=supergroup2, bs="fs"), method="REML", sytoxdata) 
