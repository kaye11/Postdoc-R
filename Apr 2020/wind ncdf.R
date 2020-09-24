##Notes: Maximum,minimun,median and mean dissipation rates and wind calculated from the COPERNICUS-CMEMS CERSAT data. Wind data have a 6 hours resolution and the statistics were calculated from these data. Dissipation rates were calculated using the empirical relation described in MacKenzie and Leggett, 1993.

#library
source("inspack_map.R")
library(ncdf4)
#library(ncdf.tools)
library(PCICt)
library(RColorBrewer)

wind_path <- paste0 ("D:/Postdoc/theoretical/NA simulation data/Modelling virus encounters/wind_dissipation_NA_2010.nc")
wind_out <- nc_open(wind_path)
windmax <- c("max_wind_speed")
disratemax <- c("max_epsilon")

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

#get disrate array
disrate.array <- ncvar_get(wind_out, disratemax)
epname <- ncatt_get(wind_out, disratemax, "max_disrate")
epunits <- ncatt_get(wind_out, disratemax, "W/m3")
fillvalue2 <- ncatt_get(wind_out, disratemax, "_FillValue")
dim(disrate.array)

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
wind_2010 <- data.frame(cbind(lonlat, wind.vec))
names(wind_2010) <- c("lon", "lat", paste(windmax))
head(na.omit(wind_2010), 5)
#for disrate
disrate.vec <- as.vector(disrate.array)
length(disrate.vec)
disrate_2010 <- data.frame(cbind(lonlat, disrate.vec))
names(disrate_2010) <- c("lon", "lat", paste(disratemax))
head(na.omit(disrate_2010), 5)
#converting disrate
den15 <- 1026 #m2/kg
disrate_2010$disrate_1m <-  (10^(2.688*(log10(wind_2010$max_wind_speed)) - 1.322* (log10(1)) -4.812))/den15 #dividing by den15 will convert W/m3 to m2/s3, the other whole equation was from the paper by Mackenzie & Leggett. Change the depth log10(30 or 1)
disrate_2010$disrate_30m <-  (10^(2.688*(log10(wind_2010$max_wind_speed)) - 1.322* (log10(30)) -4.812))/den15 #dividing by den15 will convert W/m3 to m2/s3, the other whole equation was from the paper by Mackenzie & Leggett. Change the depth log10(30 or 1)

#merge
winddis_2010 <- inner_join(wind_2010, disrate_2010) #checked merging via head, tail and df [c(row#n, row#n)]

#write into table
write.table (wind_2010, "Postdoc-R/Exported Tables/wind_2010.csv", sep=",", col.names=T, row.names=F)
write.table (disrate_2010, "Postdoc-R/Exported Tables/disrate_2010.csv", sep=",", col.names=T, row.names=F)
write.table (winddis_2010, "Postdoc-R/Exported Tables/winddisrate_2010.csv", sep=",", col.names=T, row.names=F)

#mapping
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#wind plot
resize.win(6,6)
ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-59, 10), ylim = c(30, 70) , expand = FALSE)+ 
  geom_point(data=wind_2010, aes(x = lon, y = lat, color = max_wind_speed))+
  scale_color_viridis_c (na.value = "white") +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank()) + labs (title= "Maximum wind speeds (m/s) 2010")

#disrate_upper1m
ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-59, 10), ylim = c(30, 70) , expand = FALSE)+ 
  geom_point(data=winddis_2010, aes(x = lon, y = lat, color = log10(disrate_1m)))+
  scale_color_viridis_c (na.value = "white") + #expand_limits(colour = c(seq(-7, -3, 1))) +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank()) + labs (title= expression("Maximum dissipation rate"~m^2~s^-3~"(1m depth)"))

#disrate_30m
ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-59, 10), ylim = c(30, 70) , expand = FALSE)+ 
  geom_point(data=winddis_2010, aes(x = lon, y = lat, color = log10(disrate_30m)))+
  scale_color_viridis_c (na.value = "white") +
  theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (4, "line"), legend.key.width = unit (1.25, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank()) + labs (title= expression("Maximum dissipation rate"~m^2~s^-3~"(30m depth)"))

