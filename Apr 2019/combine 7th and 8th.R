
library(readxl)
seventh <- read_excel("Postdoc-R/Exported Tables/SeventhExp_sytox.xlsx")
eight <- read_excel("Postdoc-R/Exported Tables/eightExp_sytox.xlsx")

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

seventh$rpm <- "350"
eight$rpm <- "600"

bind_data <- rbind(seventh, eight)

still <- bind_data %>% filter (group1=="still") %>% mutate (rpm="0") 
turb <- bind_data %>% filter (group1=="turbulent")

all <- rbind(still, turb)

all$supergroup <- as.factor (paste(all$maingroup, all$rpm, sep="-"))


sum.all.group <- summarySE(all, measurevar = "value", 
                           groupvars = c("supergroup", "maingroup", "group1", "group2", 
                                         "time", "parameter", "rpm"))

sum.all.group2 <- summarySE(all, measurevar = "value", 
                            groupvars = c("maingroup", "group1", "group2", "time", "parameter", "exp"))

all$supergroup <- factor(all$supergroup, levels = c("still-control-0",  "still-infected-0","turbulent-control-350" , "turbulent-infected-350", "turbulent-control-600", "turbulent-infected-600"))

##what I wanted
ggplot(data=all %>% 
         filter(parameter %in% c("sytox")), aes(x=time, y=value, linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("E. huxleyi"~ "mL"^~-1~ scriptstyle(x)~"10"^~6)) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) + 
  theme(legend.key.width=unit(3,"line"))

cellcount <- ggplot(data=all %>% 
                       filter(parameter %in% c("countpermldiv")), aes(x=time, y=value, linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("E.huxleyi"~ "mL"^~-1~ scriptstyle(x)~"10"^~6)) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication2() +
  #facet_grid(~group1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none")

fvfm <- ggplot(data=all %>% 
                      filter(parameter %in% c("fv/fm")), aes(x=time, y=value, linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("fv/fm")) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication2() +
  #facet_grid(~group1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position ="none")

sytox <- ggplot(data=all %>% 
                  filter(parameter %in% c("sytox")), aes(x=time, y=value, linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1) + 
  labs(y="% sytox stained", x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication2() +
  #facet_grid(~group1) +
  theme(strip.text = element_blank(), legend.title=element_blank())

resize.win(20,25)
grid.newpage()
grid.draw(rbind(ggplotGrob(cellcount), ggplotGrob (fvfm), ggplotGrob(sytox), size = "last"))


##cellcount

resize.win(6.6)
ggplot(data=all %>% 
                      filter(parameter %in% c("countpermldiv")), aes(x=time, y=value, linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("E.huxleyi"~   "mL"^~-1~ scriptstyle(x)~"10"^~6)) +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication2() +
  #facet_grid(~group1) +
  theme( legend.position ="none")

ggplot(data=all %>% filter (parameter == "countpermldiv" & group2 =="infected"), 
       aes(x=time, y=value, linetype=supergroup)) +
  geom_point(size=7, aes(colour=supergroup, shape=supergroup)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup, fill=supergroup), alpha=0.2, size=1.5) + 
  labs (y= expression("E.huxleyi" ~   "mL"^~-1~ scriptstyle(x)~"10"^~6), 
        x="hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(15, 16, 17)) +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#E69F00", "#56B4E9")) +
  scale_linetype_manual(values = c("longdash", "longdash",  "longdash")) +
  theme_Publication2() +
  #facet_grid(~group1) +
  theme( legend.position ="none")


all$supergroup2 <- factor(all$supergroup,
                    levels = c("still-control-0", "still-infected-0", 
                               "turbulent-control-350", "turbulent-infected-350",
                               "turbulent-control-600", "turbulent-infected-600"),
                    labels = c("still-control", "still-infected", 
                               "mid-control", "mid-infected",
                               "stormy-control", "stormy-infected"))

ggplot(data=all %>% 
         filter(parameter %in% c("sytox")), aes(x=time, y=value, linetype=supergroup2)) +
  geom_point(size=7, aes(colour=supergroup2, shape=supergroup2)) + 
  geom_smooth(method = 'loess', aes(colour=supergroup2, fill=supergroup2), alpha=0.2, size=1) + 
  labs(y="% sytox stained", x= "hours post-infection") +
  scale_x_continuous(breaks=c(0, 24, 48, 72, 96, 120)) +
  scale_shape_manual (values= c(0, 15, 1, 16, 2, 17)) +
  scale_color_manual(values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_fill_manual (values = c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash", "solid", "longdash")) +
  theme_Publication2() +
  #facet_grid(~group1) +
  theme(strip.text = element_blank(), legend.title=element_blank())




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
  filter(parameter %in% c("countperml"))

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

Form <- formula (value~ supergroup)
sytox.gls<- gls(Form, data=all.dropvp %>% 
                filter(parameter %in% c("sytox")))

sytoxdata <- all.dropvp %>% 
  filter(parameter %in% c("sytox"))
                      
plot(residuals(sytox.gls))

sytox.lme1 <- lme (Form, random= ~1|ID, method="REML", sytoxdata)

sytox.gamm0 <- gamm (value~s(time, by=supergroup, bs="fs"), method="REML", sytoxdata) 
