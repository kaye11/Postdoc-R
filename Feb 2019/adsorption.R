
#note: I renamed your columns, I also renamed your replicates. I did not use 1-9. I used 1-3. 

#import data set
library(readxl)
adsdata <- read_excel("D:/Postdoc/theoretical/CJ data/20190102 Adsorption Assay.xlsx")
View(adsdata)

#calc virus conc
adsdata$virusc <- ((adsdata$events/adsdata$runtime)/(adsdata$flowrate*2))*50*1000
#make new IDs
adsdata$ID <- as.factor(paste(adsdata$treatment, adsdata$rep, sep="-"))
adsdata$reptime <- as.factor(paste(adsdata$rep, adsdata$time, sep="-"))

#calc ratio loss and transforming to lognormal
library(data.table)
adsdata<- data.table (adsdata, key= c("ID") )
adsdata [, ratiolossln:= log((virusc/(virusc[match("0", time)]))), by= c("ID")]

#plotting
library(ggplot2)
library(plotly)
source ("resizewin.R")
resize.win (9, 6)
ggplot(data = adsdata, aes(x=as.factor(time), y=ratiolossln, color=treatment)) + geom_boxplot()
ggplot(data = adsdata, aes(x=time, y=ratiolossln, color=treatment, fill=treatment)) + geom_point(size=5) + 
  geom_smooth(method="lm", alpha=0.3)

ggplot(data = adsdata, aes(x=time, y=virusc, color=treatment, fill=treatment)) + geom_point(size=5) + 
  geom_smooth(method="lm", alpha=0.3)

library (Rmisc)
sum.ratioloss <- summarySE (adsdata, measurevar = "ratiolossln", groupvars = c("treatment", "time"))

#what if blanks are subtracted already
adsdata [, blanksubs:= (ratiolossln-(ratiolossln[match ("blanks", treatment)])), by=c("reptime")]
adsdata [, blanksubsvirusc:= (virusc-(virusc[match ("blanks", treatment)])), by=c("reptime")]

ggplot(data = adsdata, aes(x=as.factor(time), y=blanksubs, color=treatment)) + geom_boxplot ()
ggplot(data = adsdata, aes(x=time, y=blanksubs, color=treatment, fill=treatment)) + geom_point(size=5) + 
  geom_smooth (method="lm", alpha=0.3)

ggplot(data = adsdata, aes(x=time, y=blanksubsvirusc, color=treatment, fill=treatment)) + geom_point(size=5) + 
  geom_smooth(method="lm", alpha=0.3)

data.sum <- summarySE (adsdata, measurevar = "ratiolossln", groupvars = c("treatment"))

#getting slopes
slopes <- adsdata [,list(intercept=coef(lm(ratiolossln~time))[1], coef=coef(lm(ratiolossln~time))[2]),by=c("ID")]
slopes$treatment <- rep(c("blanks", "bleached", "untreated"), each=3)
aveslope <-aggregate(coef~treatment,data=subset(slopes,treatment=="blanks"),mean)
slopes$lith <- rep(c(0, 7.51e6, 7.49e6), each=3)
slopes$adscoef <- abs((slopes$coef - aveslope [1,2])/ slopes$lith)
slopes

library(Rmisc)
slopes.sum <- summarySE (slopes, measurevar = "adscoef", groupvars = "treatment")

#slopes with blanks subtracted already from rawdata.
slopesblanksubs <- adsdata [,list(intercept=coef(lm(blanksubs~time))[1], coef=coef(lm(blanksubs~time))[2]),by=c("ID")]
slopesblanksubs$lith <- rep(c(0, 7.51e6, 7.49e6), each=3)
slopesblanksubs$treatment <- rep(c("blanks", "bleached", "untreated"), each=3)
slopesblanksubs [, adscoef:= abs(coef/lith)]
slopesblanksubs

slopesblank.sum <- summarySE (slopesblanksubs, measurevar = "adscoef", groupvars = "treatment")

#if you want to save data
library(openxlsx)
write.xlsx(slopes, file = "destination folder.xlsx")
