a <- Ehux_30m_sep %>% filter (!(step=="abundance")) %>% group_by (depthf) %>% filter (y>0.02)  %>% 
  mutate (percent=(10^(x)*100))

Ehux_1m_sep %>% filter (step=="infcomb") %>% group_by (depthf) %>% filter (y>0.02) %>%  summarise (freq=sum(y*100))

Ehux_30m_sep %>% filter (step=="enccomb") %>% group_by (depthf) %>% filter (y>0.02) %>%  summarise (freq=sum(y*100))

Ehux_30m_sep %>% filter (step=="enccomb") %>% group_by (step, depthf) %>% 
  mutate (percent=(10^(x)*100)) %>%
  filter (percent>28) %>%
  summarise (freq=sum(y*100))

Ehux_30m_sep %>% filter (step=="infcomb") %>% group_by (depthf) %>% filter (xexp>0.09)  %>% 
  mutate (percent=(10^(x)*100)) %>%
  summarise (freq=sum(y*100))

NAdata.betas_comb_30m %>% group_by (depthf, entity) %>% 
  summarise (mean=mean(absenc, na.rm = TRUE), first=first(absenc), last=last(absenc))

NAdata.betas_comb_30m %>% group_by (depthf, entity) %>% 
  summarise (mean=mean(absads, na.rm = TRUE), first=first(absads), last=last(absads))


##analysis
NAdata.betas_comb_30m$absenc <- NAdata.betas_comb_30m$perencounters*NAdata.betas_comb_30m$abundance
NAdata.betas_comb_30m$absads <- NAdata.betas_comb_30m$peradsorbed*NAdata.betas_comb_30m$abundance
NAdata.betas_comb_1m$absenc <- NAdata.betas_comb_1m$perencounters*NAdata.betas_comb_1m$abundance
NAdata.betas_comb_1m$absads <- NAdata.betas_comb_1m$peradsorbed*NAdata.betas_comb_1m$abundance

NAdata_merge %>% group_by (depthf) %>% 
  summarise (mean=mean(max_wind_speed, na.rm = TRUE), first=first(max_wind_speed), last=last(max_wind_speed))

wind.sum <- setDT(NAdata_merge)[, .N, by=c("max_wind_speed", "depthf")]

ggplot(data=NAdata_merge %>% filter (entity=="calcified"), aes(x = max_wind_speed, color=depthf, fill=depthf)) + 
  geom_histogram(alpha = 0.2, binwidth = 1) + 
  theme_Publication2() + theme (legend.key.width = unit (1, "cm")) + labs (x=expression ("wind speed"~(m~s^-1))) + 
  facet_wrap(~depthf, scales="free")
