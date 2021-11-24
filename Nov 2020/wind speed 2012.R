##Notes: all wind speed for 2012

#library
source("inspack_map.R")
library(ncdf4)
library(ncdf.tools)
library(PCICt)
library(RColorBrewer)

wind_path <- paste0 ("D:/Postdoc/theoretical/NA simulation data/Modelling virus encounters/wind_speed_NA_2012.nc")
wind_out <- nc_open(wind_path)
windspeed <- c("wind_speed")

wind_out #to view

##lon lat
lon <- ncvar_get(wind_out, varid="longitude")
nlon <- dim (lon)
lat <- ncvar_get(wind_out, varid="latitude")
nlat <- dim(lat)
time <- ncvar_get(wind_out, varid="time")
ntime <- dim(time)
tunits <- ncatt_get(wind_out,"time","units")

print (c(nlon, nlat))

##time
#Convert a numeric  MATLAB datenum (days since 0000-1-1 00:00) to seconds in 
#the Unix epoch (seconds since 1970-1-1 00:00). Specify a time zone if the 
#input datenum is anything other than the GMT/UTC time zone. 
matlab2POS = function(x, timez = "UTC") {
  days = x - 719529 	# 719529 = days from 1-1-0000 to 1-1-1970
  secs = days * 86400 # 86400 seconds in a day
  # This next string of functions is a complete disaster, but it works.
  # It tries to outsmart R by converting the secs value to a POSIXct value
  # in the UTC time zone, then converts that to a time/date string that 
  # should lose the time zone, and then it performs a second as.POSIXct()
  # conversion on the time/date string to get a POSIXct value in the user's 
  # specified timezone. Time zones are a goddamned nightmare.
  return(as.POSIXct(strftime(as.POSIXct(secs, origin = '1970-1-1', 
                                        tz = 'UTC'), format = '%Y-%m-%d %H:%M', 
                             tz = 'UTC', usetz = FALSE), tz = timez))}

time.conv <- matlab2POS(time)

#get wind array
wind.array <- ncvar_get(wind_out, windspeed)
windname <- ncatt_get(wind_out, windspeed, "wind_speed")
windunit <- ncatt_get(wind_out, windspeed, "m/s")
fillvalue <- ncatt_get(wind_out, windspeed, "_FillValue")
dim(wind.array)

m <- 1
tmp_slice <- wind.array[,,m]

#make a csv file
lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)
tmp_vec <- as.vector(tmp_slice)
length(tmp_vec)

# create dataframe and add names
tmp_df01 <- data.frame(cbind(lonlat,tmp_slice))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
head(na.omit(tmp_df01), 10)

#for wind
wind.vec <- as.vector(wind.array)
length(wind.vec)
wind <- data.frame(cbind(lonlat, wind.vec))
names(wind) <- c("lon", "lat", paste(windspeed))
head(na.omit(wind), 5)