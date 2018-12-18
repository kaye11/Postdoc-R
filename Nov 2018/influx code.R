
#readin data
library(readxl)
influxdata <- read_excel("D:/Postdoc/Experiments/181022 Infection The First/20181101 1st exp virus bacteria counts.xls")
View(influxdata)

#for laptop use
#influxdata <- read.csv("D:/Karen's/Postdoc/Experiments/181022 Infection The First/20181101 1st exp virus bacteria counts.csv", sep=";")

#remove NA counts
influxdata <- dplyr::filter(influxdata,  !is.na(Statistic))

#Name example below
#C:\Users\ADMIN\Desktop\Karen\20181101\sample_001.fcs/Bacteria

#to remove dir is the code below
influxdata$Name <- gsub("^.*\\\\","\\",influxdata$Name)
#This regular expression matches the beginning of the string (^), any character (.) repeated zero or more times (*), and backslash (one backslash = 4 backslashes on gsub/regex as it is used as an escape character) 

#library(tidyverse)

influxdata$Name <- gsub(".temp","", influxdata$Name) #remove .temp

#regex patterns from new names
pattern <- "(\\w+)(_)(\\d+)(.)(\\w+)[/](\\w+)"

influxdata$samplenumber <- as.numeric(gsub(pattern, '\\3', influxdata$Name))
influxdata$cell <- gsub(pattern, '\\6', influxdata$Name)

library(dplyr)
influxdata$time <- case_when(
  influxdata$samplenumber <17  ~ "0",
  influxdata$samplenumber >16 & influxdata$samplenumber <33 ~ "24",
  influxdata$samplenumber >32 & influxdata$samplenumber <49 ~ "48",
  influxdata$samplenumber >48  ~ "72", 
  TRUE ~ as.character(influxdata$samplenumber)
)

treatments <- c ("still control 1", "still control 2", "still control 3", 
                 "turbulent control 1", "turbulent control 2", "turbulent control 3", 
                 "still viralparticles 1", "still viralparticles 2",
                 "still infected 1", "still infected 2", "still infected 3", 
                 "turbulent infected 1", "turbulent infected 2", "turbulent infected 3",
                 "turbulent viralparticles 1", "turbulent viralparticles 2")

#reorder variables
influxdata <- influxdata%>%
  arrange(factor(cell, c("EhV", "Bacteria")))

influxdata$treatment <- rep_len (treatments, length.out = 62)

library(tidyr)
influxdata <- influxdata %>% separate(treatment, c("group1", "group2", "rep"))
influxdata$maingroup <- as.factor(paste(influxdata$group1, influxdata$group2, sep="-" ))

influxdata$count <- (influxdata$X.Cells/(9.45/2))*50*1000

##code for plots next
library(Rmisc)
sum.all <- summarySE(influxdata, measurevar = "count", groupvars = c("maingroup", "group1", "group2", "time", "cell"))

scientific_10 <- scientific_10 <- function(x) {
  ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scales::scientific_format()(x)))))
}

library(plotly)

ggplotly(ggplot(data=influxdata, aes(x=time, y=count, colour=group2)) +geom_boxplot() + 
           facet_grid(cell~group1, scales="free")+ geom_point()+ theme_bw())

ggplot(data=sum.all, aes(x=time, y=count, colour=group2)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=count-se, ymax=count+se, width=0.5)) + facet_grid(cell~group1, scales="free")+ theme_bw()+
  scale_y_continuous(label=scientific_10)

source("resizewin.R")
source("theme_Publication.R")
resize.win(12,9)

sum.all$time <- as.numeric(sum.all$time)

bacteria <- ggplot(data=sum.all %>% 
         filter(cell %in% c("Bacteria")), aes(x=time, y=count, colour=group2)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=count-se, ymax=count+se, width=2)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2), alpha=0.2) + 
  labs(y="bacteria per ml") +
  scale_x_continuous(breaks=c(0, 24, 48, 72)) +
  scale_y_continuous(label=scientific_10) +
  scale_color_manual (values = c(control="lightcoral", viralparticles="seagreen3", infected="steelblue2")) +
  theme_Publication() + 
  facet_grid(~group1) +
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), legend.position="none", 
        axis.title.y = element_text(angle=90,vjust = 5))

virus <- ggplot(data=sum.all %>% 
                     filter(cell %in% c("EhV")), aes(x=time, y=count, colour=group2)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=count-se, ymax=count+se, width=2)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2), alpha=0.2) + 
  labs(y="viral particles per ml") +
  scale_x_continuous(breaks=c(0, 24, 48, 72)) +
  scale_y_continuous(label=scientific_10) +
  scale_color_manual (values = c(control="lightcoral", viralparticles="seagreen3", infected="steelblue2"))+
  theme_Publication() + 
  facet_grid(~group1) +
  theme(legend.title=element_blank(), strip.text=element_blank())

resize.win(12,15)
grid.newpage()
grid.draw(rbind(ggplotGrob(bacteria), ggplotGrob(virus),  size = "last"))

#export data
setwd("D:/R program")
require(openxlsx)
write.xlsx(sum.all, file = "Postdoc-R/Exported Tables/FirstExp_summary_virbac.xlsx")
write.xlsx(influxdata, file = "Postdoc-R/Exported Tables/FirstExp_virbac.xlsx")
