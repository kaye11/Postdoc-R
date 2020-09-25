
resize.win (6,6)
library(rcartocolor)
ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=NAdata_merge, aes(x = lon, y = lat, color = max_wind_speed)) +
  scale_colour_gradientn(colours = BlGr3(100)) +
  #scale_color_carto_c(palette="Earth", direction=-1) +
  #scale_color_carto_c(palette="Safe", direction=-1) +
  #scale_color_distiller(palette="Paired", direction=-1, na.value = "white") +
  #scale_color_viridis_c (na.value = "white") + #expand_limits(colour = c(seq(-7, -3, 1))) +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + labs (title= expression("Wind speed"~m~s^-1))

myPalette <- colorRampPalette(c ('#00429d', '#3d68aa', '#6190b7', '#86b8c4', '#b6ded1', '#c7c98d', '#d49672', '#cc665c', '#b5384a', '#93003a'))

myPalette <- colorRampPalette(rev(c('#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4')))

myPalette <- colorRampPalette((c('#00429d', '#315ea1', '#4b7ba5', '#6697a8', '#88b2a9', '#b8927e', '#c56974', '#bd4663', '#ab254f', '#93003a')))

myPalette <- colorRampPalette(c('#00429d', '#275dae', '#3d78be', '#5394ce', '#6bb1dd', '#9ecae1', '#b2c578', '#7fa45b', '#54823d', '#2c6020', '#004002'))

myPalette <- colorRampPalette((c('#00429d', '#3760ab', '#577fb7', '#779fc0', '#9dbfc6', '#f18a73', '#dd696c', '#c64b5e', '#ad2c4d', '#93003a')))

BlGr3 <- colorRampPalette(rev(c('#ffff62', '#e5e861', '#c9d36a', '#abbe74', '#8ba97d', '#679586', '#36828e', '#29689e', '#284e9c', '#253494')))

###trial
abundance.png <- ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
  geom_point(data=NAdata_merge %>%  filter(entitycode %in% c("Ncells_max", "Nliths_max")), 
             aes(x = lon, y = lat, color = log10(abundance)))+ 
  scale_colour_gradientn(colours = BlGr(100)) +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank(), legend.margin=margin(0,0,0,0),
                               legend.box.margin=margin(10,10,10,10)) + facet_grid(~entity)

ggsave(filename = "abundance.png", width = 10, height = 5, dpi = 600, device="png", units="in")


BlGr <- colorRampPalette(rev(c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58')))

BlGr2 <- colorRampPalette((c('#023858', '#0a4c6e', '#196280', '#2a798c', '#3c918a', '#46ad63', '#71c08d', '#9ed3a8', '#cee5b7', '#fff7bc')))


