
#load old data, run
hostnum <- (10)^3
virnum <- hostnum*100

BM$E_V <- (BM$beta_d*virnum)/hostnum
BM$E_HV <- BM$beta_d*virnum*hostnum
PIC$E_DS_HV <- (PIC$beta_d*virnum*hostnum)  #E calculated with Virus and Host (10:1 MOI)
PIC$E_DS_V <- (PIC$beta_d*virnum)/hostnum #E calculated with Virus
#use TK, in cm3 s
turb$E_turb_HV <- (turb$beta_d*hostnum*virnum) #E calculated with Virus and Host (10:1 MOI)
turb$E_turb_V <- (turb$beta_d*virnum)/hostnum #E calculated with virus only

#extract mean betas from PIC_newdata
beta_DS <- summarySE (PIC, measurevar = "beta_d", groupvars = c("group", "group2"))

all <- Reduce(function(x,y) merge(x,y,by="group",all=TRUE) ,
              list(BM, beta_DS, turb %>% filter(group %in% c("naked", "calcified"))))

library(data.table)
NT = data.table(all, key="group2")
allbetas = NT[, list(group=group, disrate=disrate, beta_BM=beta_d.x, beta_DS=beta_d, beta_turb = beta_d.y, 
                     beta_BM_DS =beta_d.x + beta_d,
                     beta_DS_turb = beta_d + beta_d.y,
                     beta_BM_turb = beta_d.x + beta_d.y,
                     beta_all = beta_d.x + beta_d.y + beta_d), 
              by=c("group2")]

allbetas.melt <- melt (allbetas, id.vars = c("group2", "group", "disrate"), value.name = "beta_d", 
                       variable.name = "betakernel")

allbetas.melt$E_V <- allbetas.melt$beta_d*virnum
allbetas.melt$E_HV <- allbetas.melt$beta_d*virnum*hostnum

#STOPE HERE
#save as
lowdensity <- allbetas.melt %>% filter(group2 %in% c("naked", "moderately calcified", "heavily calcified"))
lowdensity$density = "10"
highdensity <- allbetas.melt %>% filter(group2 %in% c("naked", "moderately calcified", "heavily calcified"))
highdensity$density = "100"

resize.win (10,8)
ggplot(data=highdensity %>% filter(betakernel %in% c("beta_all")), 
       aes(x=disrate,y = E_V, color=group2)) + 
  geom_line(size=1.5, position=position_jitter(w=0.02, h=0), aes(linetype="1:100"))+
  geom_line(size=1.5, data = lowdensity %>% filter(betakernel %in% c("beta_all")), aes(y= E_V, color=group2, linetype="1:10")) +
  scale_linetype_manual (values=c("solid", "dotted")) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=3),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=7),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks()+
  theme_Publication2() +
  theme(legend.title = element_blank(), legend.key.width=unit(1,"cm"))+
  labs(y = expression("viral encounters " ~day^-1~cell^-1), x = expression("dissipation rate "~(m^2~s^-3))) +
  theme(legend.title = element_blank())+ guides(linetype=guide_legend(nrow  =4), colour=guide_legend(nrow=4,byrow=TRUE)) 

low.sum <- summarySE (data=lowdensity %>% filter(betakernel %in% c("beta_BM", "beta_DS")),
                      measurevar = "E_V", 
                      groupvars=c ("group", "betakernel"), na.rm=TRUE)

high.sum <- summarySE (data=highdensity %>% filter(betakernel %in% c("beta_BM", "beta_DS")),
                      measurevar = "E_V", 
                      groupvars=c ("group", "betakernel"), na.rm=TRUE)
