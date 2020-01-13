library(readxl)
cj_masterdata <- read_excel("D:/Postdoc/theoretical/CJ data/Naked_Calcifed_infection_dynamics_master.xlsx")
View(cj_masterdata)

source("inspack.R")
cj_masterdata$ID <- as.factor(paste(cj_masterdata$calcification, cj_masterdata$media, cj_masterdata$treatment, cj_masterdata$host_strain, sep="-"))

cj_masterdata0 <- as.data.table(cj_masterdata [! cj_masterdata$time=="0",  ])

ggplot(data=cj_masterdata, aes(x=time, y=log10(cell_concentration), color=treatment, shape=treatment)) + geom_point(size=4) + geom_smooth() + facet_grid(~calcification)
#max host density: naked(48h), calc (96h)

cj_masterdata0 [, burstsize_naked:= (shift(virus_concentration)-(virus_concentration[match("48", time)]))/
                 ((cell_concentration[match("48", time)])-shift(cell_concentration)), by= c("ID")]
cj_masterdata0 [, burstsize_calcified:= (shift(virus_concentration)-(virus_concentration[match("96", time)]))/
                  ((cell_concentration[match("96", time)])-shift(cell_concentration)), by= c("ID")]

cj_masterdata0<- data.frame(cj_masterdata0, burstsize = ifelse(cj_masterdata0$calcification=="naked", cj_masterdata0$burstsize_naked, ifelse (cj_masterdata0$calcification=="calcified",cj_masterdata0$burstsize_calcified, "NA")))

cj_masterdata0$burstsize <- as.numeric(as.character(cj_masterdata0$burstsize))
sumburst <- summarySE(cj_masterdata0, measurevar="burstsize", 
                      groupvars=c("calcification", "treatment", "time"), na.rm=TRUE)

ggplot(data=sumburst, aes(x=time, y=burstsize, colour=treatment, shape=treatment)) +geom_point(size=4) + 
  geom_errorbar(aes(ymin=burstsize-se, ymax=burstsize+se, width=0.2)) + 
  geom_smooth(method = 'loess', alpha=0.2) +
  theme_Publication2() + facet_grid(~calcification)

ggplot(data=cj_masterdata0, aes(x=as.factor(time), y=burstsize, colour=treatment, shape=treatment)) +geom_point(size=4) + 
  geom_boxplot() +
  theme_Publication2() + facet_grid(~calcification)

write.table(cj_masterdata0, "Postdoc-R/Exported Tables/cj_masterdata_calcburst.csv", sep=";", col.names=T, row.names=F)
write.table(sumburst, "Postdoc-R/Exported Tables/cj_masterdata_calcburst_sum.csv", sep=";", col.names=T, row.names=F)
