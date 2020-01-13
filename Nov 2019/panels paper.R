##load betamaster4. workspace
source("inspack.R")

#plotting
#1. viral encounters per day
ggplot(data=all, aes(x=disrate,y = E_all_low , color=group, fill=group)) + 
  geom_smooth(size=1, position=position_jitter(w=0.02, h=0), aes(linetype="10^3", fill=group))+
  geom_smooth(data = all, aes(y= E_all_high, color=group,fill=group, linetype="10^5")) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=7),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks()+
  theme_Publication2() +
  theme(legend.title = element_blank(), legend.key.width=unit(1.5,"cm"))+
  labs(y = expression("viral encounters " ~day^-1~cell^-1), x = expression("dissipation rate "~(m^2~s^-3))) +
  theme(legend.title = element_blank())

#2.knowles graph
calm <- all %>% filter (disrate %in% c ("1e-07"))
calm <- calm[rep(seq_len(nrow(calm)), 6), ]
calm$host <- rep_len(c (1 %o% 10^(seq(1, 6, 1))), length.out = 30)

stormy <- all %>% filter (disrate %in% c ("1e-04"))
stormy <- stormy[rep(seq_len(nrow(stormy)), 6), ]
stormy$host <- rep_len(c (1 %o% 10^(seq(1, 6, 1))), length.out = 30)

calmstormy <- rbind (calm, stormy)
calmstormy$E_all_host <- calmstormy$beta_all*(calmstormy$host*10)
calmstormy$days <- 1/(calmstormy$beta_all*calmstormy$host)

#ben knowles graph is hard to understand
ggplot(data=calmstormy, aes(x=log10(host),y = log10(days) , color=group, fill=group, linetype=as.factor(disrate))) + geom_smooth() + facet_grid (~group)

ggplot(data=calmstormy, aes(x=host,y = days , color=group, fill=group)) + geom_smooth() + facet_grid (~disrate) +  scale_y_log10(
  breaks = scales::trans_breaks("log10", function(x) 10^x, n=5),
  labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n=7),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks()+
  theme_Publication2() +
  theme(legend.title = element_blank(), legend.key.width=unit(1.5,"cm"))+
  labs(y = expression("days in the extracellular milieu"), x = expression("E. huxleyi"~mL^-1)) +
  theme(legend.title = element_blank())


