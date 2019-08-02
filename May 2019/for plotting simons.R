#a is high density
a <- allbetas.melt.dropnc %>% filter(group2 %in% c("naked", "moderately calcified", "heavily calcified"))

ggplot(data=highdensity %>% filter(betakernel %in% c("beta_all")), 
       aes(x=disrate,y = E_V, color=group2)) + 
  geom_line(size=1, position=position_jitter(w=0.02, h=0))+
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=2),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=7),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks()+
  theme_Publication() +
  theme(legend.title = element_blank(), legend.key.width=unit(1,"cm"))+
  labs(y = expression("viral encounters " ~day^-1~cell^-1), x = expression("dissipation rate "~(m^2~s^-3))) +
  theme(legend.title = element_blank())+ guides(linetype=guide_legend(nrow  =4), colour=guide_legend(nrow=4,byrow=TRUE)) 
