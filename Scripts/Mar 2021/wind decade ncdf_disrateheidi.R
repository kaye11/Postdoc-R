##Notes: Maximum,minimun,median and mean dissipation rates and wind calculated from the COPERNICUS-CMEMS CERSAT data. Wind data have a 6 hours resolution and the statistics were calculated from these data. Dissipation rates were calculated using the empirical relation described in MacKenzie and Leggett, 1993.

#library
source("inspack_map.R")
library(ncdf4)
#library(ncdf.tools)
library(PCICt)
library(RColorBrewer)

wind_path <- paste0 ("D:/Users/karengb/NA simulation data/Modelling virus encounters/wind_dissipation_NA_2010_2018.nc")
wind_out <- nc_open(wind_path)
windmax <- c("max_wind_speed") #choose wind max because you would need to recalculate disrate anyway

wind_out #to view

##lon lat
lon <- ncvar_get(wind_out, varid="longitude")
nlon <- dim (lon)
lat <- ncvar_get(wind_out, varid="latitude")
nlat <- dim(lat)
print (c(nlon, nlat))

#get wind array
wind.array <- ncvar_get(wind_out, windmax)
windname <- ncatt_get(wind_out, windmax, "max_wind_speed")
windunit <- ncatt_get(wind_out, windmax, "m/s")
fillvalue <- ncatt_get(wind_out, windmax, "_FillValue")
dim(wind.array)

#image lon lat
image(lon, lat, wind.array, col = rev(brewer.pal(10, "RdBu")))

grid <- expand.grid(lon = lon, lat = lat)
levelplot(wind.array ~ lon * lat, data = grid,pretty=TRUE, col.regions = viridis(256, begin=0, end=1), cuts=25,
          region=TRUE)

#get global attributes
year <- ncatt_get(wind_out, 0, "year")
depth <- ncatt_get(wind_out, 0, "depth")

#make a csv file
lonlat <- expand.grid(lon, lat)
#for wind
wind.vec <- as.vector(wind.array)
length(wind.vec)
wind <- data.frame(cbind(lonlat, wind.vec))
names(wind) <- c("lon", "lat", paste(windmax))
head(na.omit(wind), 5)

#converting disrate, formula from Heidi
#constants
K= 0.4 #von Karman's constant
Cd <- 1.15*(10)^-3 #drag coefficient
Den_air <-  (1.212)*10^-3 #g/m3 at sea level and 18C
Den_CH2O= 1.025 #g/m3 density seawater at 18C #these two will even out so no need to convert
wind$U <- sqrt(Cd*((wind$max_wind_speed)^2)*((Den_air/Den_CH2O)))
wind$disrate_1m <- (wind$U^3)/(K*1) #1 here is depth
wind$disrate_30m <- (wind$U^3)/(K*30) #30 here is depth

#write into table
write.table (wind, "D:/Users/karengb/Exported Tables/winddisrate_mar2021.csv", sep=",", col.names=T, row.names=F)

#mapping
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#wind plot
resize.win(6,6)

#color scheme
BlRed <- colorRampPalette((c('#00429d', '#2b57a7', '#426cb0', '#5681b9', '#6997c2', '#7daeca', '#93c4d2', '#abdad9', '#caefdf', '#ffe2ca', '#ffc4b4', '#ffa59e', '#f98689', '#ed6976', '#dd4c65', '#ca2f55', '#b11346', '#93003a')))

ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-59, 10), ylim = c(30, 70) , expand = FALSE)+   geom_point(data=wind, aes(x = lon, y = lat, color = max_wind_speed))+
  scale_colour_gradientn(colours = BlRed(100)) +  
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank()) + labs (title= "Maximum wind speeds (m/s)")




#disrate_upper1m
ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-59, 10), ylim = c(30, 70) , expand = FALSE)+ 
  geom_point(data=wind, aes(x = lon, y = lat, color = log10(disrate_1m)))+
  scale_color_viridis_c (na.value = "white") + #expand_limits(colour = c(seq(-7, -3, 1))) +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank()) + labs (title= expression("Maximum dissipation rate"~m^2~s^-3~"(1m depth)"))

#disrate_30m
ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-59, 10), ylim = c(30, 70) , expand = FALSE)+ 
  geom_point(data=wind, aes(x = lon, y = lat, color = log10(disrate_30m)))+
  scale_color_viridis_c (na.value = "white") +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank()) + labs (title= expression("Maximum dissipation rate"~m^2~s^-3~"(30m depth)"))

