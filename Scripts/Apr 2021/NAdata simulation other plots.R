#1m
NA_infection_1m_above10 <-ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+
  geom_point(data= melted_NAdata.betas_comb_1m %>% filter(parameter=="infcomb") %>% filter (group=="Cc") %>% 
               filter (value>0.099),
             aes(x = lon, y = lat, color = log10(value*100)))+
  scale_colour_gradientn(colours = BlRed(100))  + 
  scale_x_continuous(breaks=seq(-70, 10, 20)) +
  scale_y_continuous(breaks=seq(30, 75, 20)) +
  theme_Publication2() +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) + 
  labs (color= expression("log10 percent infected"~d^-1), x="", y="") +
  guides(color = guide_colorbar(title.position = "right")) 

ggsave(filename = "NA_infection_1m_above10.png", width = 7.5, height = 5, dpi = 600, device="png", units="in")

#30m
NA_infection_30m_above10 <-ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+
  geom_point(data= melted_NAdata.betas_comb_30m %>% filter(parameter=="infcomb") %>% filter (group=="Cc") %>% 
               filter (value>0.099),
             aes(x = lon, y = lat, color = log10(value*100)))+
  scale_colour_gradientn(colours = BlRed(100))  + 
  scale_x_continuous(breaks=seq(-70, 10, 20)) +
  scale_y_continuous(breaks=seq(30, 75, 20)) +
  theme_Publication2() +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) + 
  labs (color= expression("log10 percent infected"~d^-1), x="", y="") +
  guides(color = guide_colorbar(title.position = "right")) 

ggsave(filename = "NA_infection_30m_above10.png", width = 7.5, height = 5, dpi = 600, device="png", units="in")


abundance_Ehux.png <- ggplot(data = world)+ geom_sf(color="darkgray", fill="gainsboro") + 
  coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+   
  geom_point(data=NAdata_merge %>% filter (group=="Cc"), aes(x = lon, y = lat, color = log10(abundance)))+ 
  scale_colour_gradientn(colours = BlRed(100)) +  
  scale_x_continuous(breaks=seq(-70, 10, 20)) +
  scale_y_continuous(breaks=seq(30, 75, 20)) +
  theme_Publication2() +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10), strip.text.x = element_text (face="italic")) + 
  labs (color= expression(log[10]~"concentration"~mL^-1), x="", y="") +
  guides(color = guide_colorbar(title.position = "right")) + facet_grid (~entity)

ggsave(filename = "abundance_Ehux.png", width = 8, height = 6, dpi = 600, device="png", units="in")

####################

#1m
NA_infection_1m <-ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+
  geom_point(data= melted_NAdata.betas_comb_1m %>% filter(parameter=="infcomb") %>% filter (group=="Cc"),
             aes(x = lon, y = lat, color = log10(value*100)))+
  scale_colour_gradientn(colours = BlRed(100))  + 
  scale_x_continuous(breaks=seq(-70, 10, 20)) +
  scale_y_continuous(breaks=seq(30, 75, 20)) +
  theme_Publication2() +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) + 
  labs (color= expression("% infected"~d^-1), x="", y="") +
  guides(color = guide_colorbar(title.position = "right")) 

ggsave(filename = "NA_infection_1m.png", width = 7.5, height = 5, dpi = 600, device="png", units="in")

#30m
NA_infection_30m <-ggplot(data = world)+ geom_sf(color="gray", fill="gainsboro") + 
  coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+
  geom_point(data= melted_NAdata.betas_comb_30m %>% filter(parameter=="infcomb") %>% filter (group=="Cc"),
             aes(x = lon, y = lat, color = log10(value*100)))+
  scale_colour_gradientn(colours = BlRed(100))  + 
  scale_x_continuous(breaks=seq(-70, 10, 20)) +
  scale_y_continuous(breaks=seq(30, 75, 20)) +
  theme_Publication2() +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) + 
  labs (color= expression("% infected"~d^-1), x="", y="") +
  guides(color = guide_colorbar(title.position = "right")) 

ggsave(filename = "NA_infection_30m.png", width = 7.5, height = 5, dpi = 600, device="png", units="in")

disrate_1m_scale.png <-  ggplot(data = world)+ geom_sf(color="darkgray", fill="gainsboro") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=NAdata_merge, aes(x = lon, y = lat, color = log10(disrate_1m)))+
  scale_colour_gradientn(colours = BlRed(1000), breaks = c(seq (-7, -3, 1)), limits=c(-7, -3)) + theme_Publication2() + 
  theme_Publication2() + 
  scale_x_continuous(breaks=seq(-70, 10, 20)) +
  scale_y_continuous(breaks=seq(30, 75, 20)) +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10), plot.title = element_blank()) +  
  labs (color= expression(log[10]~epsilon~(m^2~s^-3)), x="", y="") +
  guides(color = guide_colorbar(title.position = "right"))  

ggsave(filename = "disrate_1m_scale.png", width = 8, height = 6, dpi = 600, device="png", units="in")

#disrate_30m
disrate_30m_scale.png <- ggplot(data = world)+ geom_sf(color="darkgray", fill="gainsboro") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=NAdata_merge, aes(x = lon, y = lat, color = log10(disrate_30m)))+
  scale_colour_gradientn(colours = BlRed(1000), breaks = c(seq (-7, -3, 1)), limits=c(-7, -3)) + theme_Publication2() + 
  theme_Publication2() + 
  scale_x_continuous(breaks=seq(-70, 10, 20)) +
  scale_y_continuous(breaks=seq(30, 75, 20)) +
  theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"),
        legend.key.width = unit (1.25, "line"), legend.title = element_text(angle = 270, hjust= 0.5), 
        panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10), plot.title = element_blank()) +  
  labs (color= expression(log[10]~epsilon~(m^2~s^-3)), x="", y="") +
  guides(color = guide_colorbar(title.position = "right"))  

ggsave(filename = "disrate_30m_scale.png", width = 8, height = 6, dpi = 600, device="png", units="in")
