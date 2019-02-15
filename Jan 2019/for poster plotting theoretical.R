#for poster
resize.win(15,12)

PIC$group2 <- factor (PIC$group2,levels= c("naked_bouyant", "naked/calcified uncertain",
                                           "moderately calcified", "strongly calcified"),
                      labels = c("naked", "naked/calcified uncertain",
                                 "moderately calcified", "strongly calcified"))


ggplot(data=PIC, aes(x=PICpercellpg, y=SinkVel, color=group2)) + 
  geom_point(aes(size=Den_celltotal))+
  theme_Publication() +
  labs(y = expression("sinking velocity"~ ("m"~day^-1)), x = expression("PIC"~cell^-1)) +
  theme(legend.position = "right", legend.title = element_blank()) +
  guides(linetype=guide_legend(ncol=1), colour=guide_legend(ncol=1,byrow=TRUE))

#other format
resize.win(9,6)
ggplot(data=PIC, aes(x=PICpercellpg, y=SinkVel, color=group2)) + 
  geom_point(aes(size=Den_celltotal))+
  theme_Publication2() +
  labs(y = expression("sinking velocity"~ ("m"~day^-1)), x = expression("PIC"~cell^-1)) +
  theme(legend.title = element_blank(), legend.position = "right") +
  guides(linetype=guide_legend(nrow=4), colour=guide_legend(nrow=4,byrow=TRUE))

#for lith
ggplot(data=PIC, aes(x=perlithpg, y=SinkVel_lith, color=group2)) + 
  geom_point(aes(size=Denlith))+
  theme_Publication2() +   scale_y_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4)) +
  labs(y = expression("sinking velocity"~("m"~day^-1)), x = expression("PIC"~lith^-1)) +
  theme(legend.title = element_blank(), legend.position = "right") +
  guides(linetype=guide_legend(nrow=4), colour=guide_legend(nrow=4,byrow=TRUE))

#for predicted (cell and lith, make 2 geom points with diff color, edit in inkscape)
resize.win(6, 6)
ggplot(data=PIC_newdata, aes(x=PICpercellpg)) + geom_point(aes(y=SinkVel.pred), size=4) +
  geom_point(aes(y=SinkVel.pred.lith), size=4, color="blue") + 
  theme_Publication2()+
  labs(y = expression("sinking velocity"~("m"~day^-1)), x = expression("PIC pg")) 

#for poster
resize.win(6,7)
cellplot <- ggplot(data=PIC, aes(x=PICpercellpg, y=E_DS_V)) +geom_point(size=7, aes(color=group2)) +
  theme_Publication2() + geom_smooth (color="black") +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=3),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) + annotation_logticks(sides="l") +
  labs(y = expression("viral encounters " ~ day^-1~lith^-1), x = expression("PIC pg"~cell^-1)) +
  theme(legend.title = element_blank())+
  guides(linetype=guide_legend(ncol=2), colour=guide_legend(ncol=2,byrow=TRUE))


#for liths
lithplot <- ggplot(data=PIC, aes(x=perlithpg, y=Elith_DS_V)) +geom_point(size=7, aes(color=group2)) +
  theme_Publication2() + geom_smooth (color="black") +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=3),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) + annotation_logticks(sides="l") +
  labs(y = expression("viral encounters " ~ day^-1~lith^-1), x = expression("PIC pg"~lith^-1)) +
  theme(legend.title = element_blank())+
  guides(linetype=guide_legend(ncol=2), colour=guide_legend(ncol=2,byrow=TRUE))

gt1 <- ggplotGrob(cellplot)
gt2 <- ggplotGrob(lithplot)

newWidth = unit.pmax(gt1$widths[2:3], gt2$widths[2:3])

gt1$widths[2:3] = as.list(newWidth)
gt2$widths[2:3] = as.list(newWidth)

grid.arrange(gt1, gt2, ncol=2)



ggplot(data=PIC_newdata, aes(x=PICpercellpg, y=E_DS_V.pred)) +geom_point(size=7, aes(color=group2)) +
  theme_Publication2() + geom_smooth (color="black") +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=4),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) + annotation_logticks(sides="l") +
  labs(y = expression("viral encounters " ~ day^-1~cell^-1), x = expression("PIC pg"~cell^-1)) +
  theme(legend.title = element_blank()) +
  guides(linetype=guide_legend(ncol=2), colour=guide_legend(ncol=2,byrow=TRUE))

ggplot(data=PIC_newdata, aes(x=perlithpg.pred, y=E_DS_V.pred.lith)) +geom_point(size=7, aes(color=group2)) +
  theme_Publication2() + geom_smooth (color="black") +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=4),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) + annotation_logticks(sides="l") +
  labs(y = expression("viral encounters " ~ day^-1~lith^-1), x = expression("PIC pg"~lith^-1)) +
  theme(legend.title = element_blank()) +
  guides(linetype=guide_legend(ncol=2), colour=guide_legend(ncol=2,byrow=TRUE))




#for poster
resize.win(8,7)

turb$group <- factor(turb$group, levels= c("naked", "calcified", "lith"))

ggplot(data = turb, aes(x = disrate, y = E_turb_V, color=group)) + geom_point(size =6) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=4),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=8),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks()+ scale_color_manual (values=c("#F8766D", "#C77CFF", "blue")) +
  theme_Publication2()+
  labs(y = expression("viral encounters " ~ day^-1~cell^-1), x = expression("dissipation rate "~(m^2~s^-3))) +
  theme(legend.title = element_blank())


#for poster  
resize.win(10,10)
ggplot(data=allbetas.melt %>% filter(betakernel %in% c("beta_all")), 
       aes(x=disrate,y = E_V, color=group2)) + 
  geom_line(size=2, position=position_jitter(w=0.02, h=0), aes(linetype="cell"))+
  geom_line(size=2, data = lith, aes(y= E_V, color=maingroup, linetype="lith")) +
  scale_linetype_manual (values=c("solid", "dashed")) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=2),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=7),
    labels = scales:: trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks()+
  theme_Publication2() +
  theme(legend.title = element_blank(), legend.key.width=unit(1,"cm"))+
  labs(y = expression("viral encounters " ~day^-1~cell^-1), x = expression("dissipation rate "~(m^2~s^-3))) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.key.width=unit(4,"line"))+
  guides(linetype=guide_legend(ncol=1), colour=guide_legend(ncol=1,byrow=TRUE))


ggplot(data=allbetas.melt %>% filter(betakernel %in% c("beta_all")), 
       aes(x=disrate,y = E_HV, color=group2)) + 
  geom_line(size=2, position=position_jitter(w=0.02, h=0), aes(linetype="cell"))+
  geom_line(size=2, data = lith, aes(y= E_HV, color=maingroup, linetype="lith")) +
  scale_linetype_manual (values=c("solid", "dashed")) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=2),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=7),
    labels = scales:: trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks()+
  theme_Publication2() +
  theme(legend.title = element_blank(), legend.key.width=unit(1,"cm"))+
  labs(y = expression("viral encounters " ~ cm^-3~day^-1),x = expression("dissipation rate "~(m^2~s^-3))) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.key.width=unit(4,"line"))+
  guides(linetype=guide_legend(ncol=1), colour=guide_legend(ncol=1,byrow=TRUE))


####Version 2 poster

#PIC.subset <- PIC[c(17, 8, 15:16, 21, 23:24, 26, 30)]

PIC <- as.data.frame(PIC) ##--> needed to convert back because after dplyr, melt would not work with a tibble
cell <- rep_len(c("cell"), length.out=46)
lith <- rep_len(c("lith"), length.out=46)
type <- c(cell, lith)

PIC.PIC <- PIC [c (17, 8, 23)]
PIC.den <- PIC [c (17, 15, 24)]
PIC.sinkvel <- PIC [c (17, 16, 26)]
PIC.E_V <- PIC [c (17, 21, 30)]

library(reshape2)
detach("package:Rmisc", unload=TRUE)  ##reshape2 will not work with plyr on. plyr is a dependency of Rmisc so just detach the whole thing

PIC.melt <- melt (PIC.PIC, id.vars = c("group2"), value.name = c("PIC"), 
                      variable.name = "type")
den.melt <- melt (PIC.den, id.vars = c("group2"), value.name = "density", 
                      variable.name = "type")
sinkvel.melt <- melt (PIC.sinkvel, id.vars = c("group2"), value.name = "SinkVel", 
                      variable.name = "type")
E_V.melt <- melt (PIC.E_V, id.vars = c("group2"), value.name = "E_V", 
                      variable.name = "type")

df_list <- list(PIC.melt, den.melt, sinkvel.melt, E_V.melt)
PIC.meltall <- do.call(cbind, df_list )
PIC.meltall$type <- type

PIC.meltall2 <- PIC.meltall [c(1,2,3, 6, 9, 12)]

ggplot(data=PIC.meltall2, aes(x=PIC, y=SinkVel, color=group2, shape=type)) + 
  geom_point(aes(size=density))+
  theme_Publication() +
  labs(y = expression("sinking velocity"~ ("m"~day^-1)), x = expression("PIC pg")) +
  theme(legend.position = "right", legend.title = element_blank()) +
  guides(linetype=guide_legend(ncol=1), colour=guide_legend(ncol=1,byrow=TRUE))

resize.win(12,9)
ggplot(data=PIC.meltall2, aes(x=PIC, y=SinkVel, color=group2)) + 
  geom_point(aes(size=density))+
  theme_Publication2() +
  labs(y = expression("sinking velocity"~ ("m"~day^-1)), x = expression("PIC pg"~"cell or lith"^-1)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(linetype=guide_legend(ncol=2), colour=guide_legend(ncol=2,byrow=TRUE), 
         size=guide_legend(ncol=3,byrow=TRUE)) + 
  facet_grid(~type, scales="free_x") +
  scale_size_continuous(range = c(7, 18)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())


ggplot(data=PIC.meltall2, aes(x=PIC, y=E_V)) +geom_point(size=12, aes(color=group2)) +
  theme_Publication2() + geom_smooth (color="black") +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=3),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) + annotation_logticks(sides="l") +
  labs(y = expression("viral encounters " ~ day^-1~"cell or lith"^-1), x = expression("PIC pg"~"cell or lith"^-1)) +
  theme(legend.title = element_blank())+
  guides(linetype=guide_legend(ncol=2), colour=guide_legend(ncol=2,byrow=TRUE), 
         size=guide_legend(ncol=3,byrow=TRUE)) + facet_grid(~type, scales="free_x") +
  theme(strip.text = element_blank(), legend.title=element_blank())

resize.win(18,20)
grid.newpage()
grid.draw(rbind(ggplotGrob(splot), ggplotGrob(vplot), size = "last"))
