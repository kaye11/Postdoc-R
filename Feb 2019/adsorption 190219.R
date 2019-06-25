
#note: I renamed your columns, I also renamed your replicates. I did not use 1-9. I used 1-3. 

#import data set
library(readxl)
adsdata <- read_excel("D:/Postdoc/theoretical/CJ data/Adsorption Assay 20191902_R.xlsx")
View(adsdata)

#calc virus conc
adsdata$virusc <- ((adsdata$events/adsdata$runtime)/(adsdata$flowrate*2))*50*1000
adsdata$treatment <- factor (adsdata$treatment, levels = c("blank", "liths and virus"),
                             labels = c ("blank", "liths"))
#make new IDs
adsdata$ID <- as.factor(paste(adsdata$treatment, adsdata$rep, adsdata$flask,  sep="-"))
adsdata$reptime <- as.factor(paste(adsdata$rep, adsdata$time, sep="-"))
adsdata$reptimeflask <- as.factor(paste(adsdata$reptime, adsdata$flask, sep="-"))


#calc ratio loss and transforming to lognormal
library(data.table)
adsdata<- data.table (adsdata, key= c("ID") )
adsdata [, ratiolossln:= log((virusc/(virusc[match("0", time)]))), by= c("ID")]

#plotting
library(ggplot2)
library(plotly)
source("resizewin.R")
resize.win (12,9)
ggplot(data = adsdata, aes(x=as.factor(time), y=ratiolossln, color=treatment)) + geom_boxplot() + facet_grid(~flask)
ggplot(data = adsdata, aes(x=time, y=ratiolossln, color=treatment, fill=treatment)) + geom_point(size=5) + 
  geom_smooth(method="lm", alpha=0.3) +  facet_grid(~flask)

ggplot(data = adsdata, aes(x=time, y=virusc, color=treatment, fill=treatment)) + geom_point(size=5) + 
  geom_smooth(method="lm", alpha=0.3) + facet_grid(~flask)

#what if blanks are subtracted already
adsdata<- data.table (adsdata, key= c("reptimeflask"))
adsdata [, blanksubs:= (ratiolossln-(ratiolossln[match ("blank", treatment)])), by=c("reptimeflask")]
adsdata [, blanksubsvirusc:= (virusc-(virusc[match ("blank", treatment)])), by=c("reptimeflask")]


ggplot(data = adsdata, aes(x=as.factor(time), y=blanksubs, color=treatment)) + geom_boxplot () +facet_grid(~flask)
ggplot(data = adsdata, aes(x=time, y=blanksubs, color=treatment, fill=treatment)) + geom_point(size=5) + 
  geom_smooth (method="lm", alpha=0.3) + facet_grid(~flask)

ggplot(data = adsdata, aes(x=time, y=blanksubsvirusc, color=treatment, fill=treatment)) + geom_point(size=5) + 
  geom_smooth(method="lm", alpha=0.3) + facet_grid(~flask)

library(Rmisc)
data.sum <- summarySE (adsdata, measurevar = "ratiolossln", groupvars = c("treatment", "flask" ))


#getting slopes
slopes <- adsdata [,list(intercept=coef(lm(ratiolossln~time))[1], coef=coef(lm(ratiolossln~time))[2]),by=c("ID")]
slopes$treatment2 <- rep(c("blank-glass","liths-glass", 
                          "blank-plastic","liths-plastic"), times=3)
slopes$ID2 <- slopes$ID
slopes <- slopes %>% separate(ID, c("treatment", "rep", "flask")) 
aveslope_glass <-aggregate(coef~treatment,data=subset(slopes,treatment2=="blank-glass"),mean)
aveslope_plastic <-aggregate(coef~treatment,data=subset(slopes,treatment2=="blank-plastic"),mean)

slopes$lith <- (values= rep(c(0, 7.45e6), times=3))

slopes_glass <- slopes %>% filter (flask=="glass")
slopes_plastic <- slopes %>% filter (flask=="plastic")
slopes_glass$adscoef <- abs((slopes_glass$coef - aveslope_glass [1,2])/ slopes_glass$lith)
slopes_plastic$adscoef <- abs((slopes_plastic$coef - aveslope_plastic [1,2])/ slopes_plastic$lith)

slopes <- rbind (slopes_glass, slopes_plastic)

library(Rmisc)
slopes.sum <- summarySE (slopes, measurevar = "adscoef", groupvars = c("treatment", "flask"))

#if you want to save data
library(openxlsx)
write.xlsx(slopes, file = "destination folder.xlsx")
