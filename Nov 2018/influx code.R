
#readin data
library(readxl)
influxdata <- read_excel("D:/Postdoc/Experiments/181022 Infection The First/20181101 1st exp virus bacteria counts.xls")
View(influxdata)

#remove NA values
influxdata <- dplyr::filter(influxdata,  !is.na(Statistic))

#Name example below
#C:\Users\ADMIN\Desktop\Karen\20181101\sample_001.fcs/Bacteria

#to remove dir is the code below
influxdata$Name <- gsub("^.*\\\\","\\",influxdata$Name)
#This regular expression matches the beginning of the string (^), any character (.) repeated zero or more times (*), and backslash (one backslash = 4 backslashes on gsub/regex as it is used as an escape character) 

library(tidyverse)

influxdata$Name <- gsub(".temp","", influxdata$Name) #remove .temp

#regex patterns from new names
pattern <- "(\\w+)(_)(\\d+)(.)(\\w+)[/](\\w+)"

influxdata$samplenumber <- as.numeric(gsub(pattern, '\\3', influxdata$Name))
influxdata$cell <- gsub(pattern, '\\6', influxdata$Name)

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

first.long<- separate(first.long, group, into = paste("group", 1:2, sep = ""))

