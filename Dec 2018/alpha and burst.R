library(readxl)
all.dropvp <- read_excel("Postdoc-R/Exported Tables/allexp_sytox_minusvp.xlsx")

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

gcdata$lncellsR <- log(gcdata$value)

#plotting
qplot(as.factor(time), lncellsR, data = gcdata,  geom = "boxplot") + facet_grid(exp~., scales="free") 

library(Rmisc)
GC.sum <- summarySE(gcdata, measurevar="lncellsR", groupvars=c("exp", "time", "group1"))
GC.sum2 <- summarySE(gcdata, measurevar="value", groupvars=c("exp", "time", "group1"))


#plot with log normal cells R
ggplot(data=GC.sum, aes(x=time, y=lncellsR)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=lncellsR-se, ymax=lncellsR+se), width=0.5, size=1) + facet_grid(group1~exp)+
  geom_smooth(method="loess", aes(group=1))

ggplot(data=GC.sum2, aes(x=time, y=value)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=0.5, size=1) + facet_grid(group1~exp)+
  geom_smooth(method="loess", aes(group=1))

#make an ID
gcdata$ID <- as.factor(paste(gcdata$supergroup, gcdata$rep, sep="-"))
gcdata$ID2 <- as.factor (paste(gcdata$group1, gcdata$rep, gcdata$exp, gcdata$time, sep="-"))

NT = data.table (gcdata, key=c("ID"))


GC.comp=NT[,list(time=time, day=time/24, rep=rep, group1=group1, group2=group2, maingroup=maingroup, 
                 supergroup=supergroup, supergroup2=supergroup2, exp, exp, rpm=rpm, count=value, 
                 ID2=ID2, lncellsR=lncellsR, 
                 r= (lncellsR/shift(lncellsR))), by=c("ID")]

#GC.comp [,div.day:=r/log(2)] #this is always 2
#GC.comp [, genT:= 1/div.day]

#all.dropvp$ID2 <- as.factor (paste(all.dropvp$group1, all.dropvp$rep, all.dropvp$exp, all.dropvp$time, sep="-"))
countperml.data$ID2 <- as.factor (paste(countperml.data$group1, countperml.data$rep, countperml.data$exp, 
                                       countperml.data$time, sep="-"))

#compdf <-  merge(GC.comp [, 12:14], countperml.data, by = "ID2", all.x = TRUE)

virbac <- read_excel("Postdoc-R/Exported Tables/allexp_virbac minus vp.xlsx")

virbac$ID2 <- as.factor (paste(virbac$group1, virbac$rep2, virbac$exp, virbac$time, sep="-"))
virbac$ID3 <- as.factor (paste(virbac$group1, virbac$exp, virbac$time, sep="-"))

virus <- virbac %>% 
  filter(cell %in% c("EhV"))

GC.comp$ID3 <- as.factor (paste(GC.comp$group1, GC.comp$exp, GC.comp$time, sep="-"))
countperml.data$ID3 <- as.factor (paste(countperml.data$group1, countperml.data$exp, countperml.data$time, sep="-"))

virus$host <- countperml.data$value [match(virus$ID2, countperml.data$ID2)]
virus$r <- GC.comp$r [match(virus$ID3, GC.comp$ID3)]

alldata <- virus [, c (2:7, 9:18)]

alldata$day <- alldata$time/24

alldata$ID <- as.factor(paste(alldata$supergroup, alldata$rep2, sep="-"))

alldata <- data.table (alldata, key=c("ID"))

alldata [, Ct:= ((host-shift(host))/(day-shift(day))), by= c("ID")]

write.xlsx(alldata, file = "Postdoc-R/Exported Tables/alldata for alpha.xlsx")

#betas
betas <- read_excel("Postdoc-R/Exported Tables/allbetas.xlsx")

nakedbetas <- betas %>% 
  filter(group %in% c("naked"))

rpmbetas <- nakedbetas %>% 
  filter(disrate %in% c("3.16227766016838e-07", "1e-05", "0"))

rpmbetas$rpm <- case_when(
  rpmbetas$disrate == 0 ~ "0 rpm",
  rpmbetas$disrate == 3.16227766016838e-07 ~ "5 rpm",
  rpmbetas$disrate == 1e-05 ~ "15 rpm",
  TRUE ~ as.character(rpmbetas$disrate)
)

alldata$cell <- "naked"
rpmbetas$cell <- "naked"
rpmbetas$group2 <- NULL

alldata$beta <- rpmbetas$beta_d [match(alldata$rpm, rpmbetas$rpm)]

alldata2 <- join (alldata, rpmbetas [, c(8, 3:4, 7)])
alldata2 <- data.table (alldata2, key=c("cell", "rpm", "betakernel"))

alldata2 [, alpha:= (Ct-r)*(-1/(beta_d*host*count))]

alldata2.dropNA <- alldata2 %>% drop_na(alpha)
alldata2.dropBM <- alldata2.dropNA  [! alldata2.dropNA$betakernel=="beta_BM",  ]
imptdata <- alldata2.dropBM %>% 
  filter(betakernel %in% c("beta_BM_DS", "beta_all"))

ggplotly(ggplot(data=imptdata, aes(x=time, y=alpha, color=betakernel)) + geom_boxplot()+geom_point(size=2) +
           theme_Publication() + facet_wrap(maingroup~exp))

sumbetas <- summarySE(alldata2.dropNA, measurevar="alpha", 
                      groupvars=c("exp", "time", "maingroup", "group1", "group2", "supergroup2", "betakernel", "rpm"))

ggplot(data=sumbetas %>% 
         filter(betakernel %in% c("beta_BM_DS", "beta_all")), 
       aes(x=time, y=alpha, colour=betakernel)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=alpha-se, ymax=alpha+se, width=2)) + 
  geom_smooth(method = 'loess', aes(colour=betakernel, fill=betakernel), alpha=0.2) + 
  scale_x_continuous(breaks=c(24, 48, 72, 96, 120)) +
  theme_Publication() +
  facet_wrap(maingroup~rpm, scales="free")

alphadatafinal <- sumbetas %>% 
  filter(betakernel %in% c("beta_BM_DS", "beta_all"))

write.xlsx(alphadatafinal, file = "Postdoc-R/Exported Tables/alpha summary.xlsx")


#burst size
a <- alldata2[ , .SD[which.max(count)], by = ID]
b <- alldata2[ , .SD[which.min(time)], by = ID]
c <- alldata2[ , .SD[which.max(host)], by = ID]

burstdata <- rbind (a, b)
burstdata [, burstsize:= ((shift(count)-count)/(host-shift(host))), by= c("ID")]
burstdata.dropNA <- burstdata %>% drop_na(burstsize)

burstsum <- summarySE(data=burstdata.dropNA, measurevar = "burstsize", 
                     groupvars=c("exp", "maingroup", "group1", "group2"))

ggplot(data=burstdata.dropNA %>% filter(exp %in% c("2nd", "3rd")), 
       aes(x=group1, y=burstsize, colour=group2)) +geom_boxplot() + geom_point () + 
  theme_Publication() + facet_wrap(~exp, scales="free")

write.xlsx(burstdata, file = "Postdoc-R/Exported Tables/burstdata.xlsx")

#try kay's suggestion

alldata.drop0 <- alldata2 [! alldata2$time=="0",  ]

alldata.drop0 [, burstsize:= ((shift(count)-count)/(host-shift(host))), by= c("ID")]
alldata.drop0 [, burstsize2:= (shift(count)-(count[match("24", time)]))/
            ((host[match("24", time)])-shift(host)), by= c("ID")]

sumburst <- summarySE(alldata.drop0, measurevar="burstsize", 
                      groupvars=c("exp", "time", "maingroup", "group1", "group2", "supergroup2", "rpm"))


ggplot(data=sumburst %>% filter(exp %in% c("2nd", "3rd")), 
       aes(x=time, y=burstsize, colour=group2)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=burstsize-se, ymax=burstsize+se, width=2)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2), alpha=0.2) + 
  scale_x_continuous(breaks=c(24, 48, 72, 96, 120)) +
  theme_Publication() +
  facet_wrap(group1~exp, scales="free")

sumburst2 <- summarySE(alldata3.drop, measurevar="burstsize2", 
                      groupvars=c("exp", "time", "maingroup", "group1", "group2", "supergroup2", "rpm"))

sumburst2.1 <- sumburst2 %>% filter(time %in% c("72", "96", "120"))


ggplot(data=sumburst2.1 %>% filter(exp %in% c("2nd", "3rd")),
       aes(x=time, y=burstsize2, colour=group2)) +geom_point(size=5) + 
  geom_errorbar(aes(ymin=burstsize2-se, ymax=burstsize2+se, width=2)) + 
  geom_smooth(method = 'loess', aes(colour=group2, fill=group2), alpha=0.2) + 
  scale_x_continuous(breaks=c(24, 48, 72, 96, 120)) +
  theme_Publication() +
  facet_wrap(group1~exp, scales="free")


write.xlsx(alldata.drop0, file = "Postdoc-R/Exported Tables/burstsize final.xlsx")
  