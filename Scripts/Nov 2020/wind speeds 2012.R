##Notes: all wind speed for 2012

#library
source("inspack_map.R")
library(ncdf4)
#library(ncdf.tools)
library(PCICt)
library(RColorBrewer)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("between", "dplyr")

wind_path <- paste0 ("D:/Users/karengb/NA simulation data/Modelling virus encounters/wind_speed_NA_2012.nc")
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

lonIdx <- which( wind_out$dim$lon$vals > 0 | wind_out$dim$lon$vals < 20)
latIdx <- which( wind_out$dim$lat$vals > 50 | wind_out$dim$lat$vals < 70)

#get wind array
wind.array <- ncvar_get(wind_out, windspeed)
windname <- ncatt_get(wind_out, windspeed, "wind_speed")
windunit <- ncatt_get(wind_out, windspeed, "m/s")
fillvalue <- ncatt_get(wind_out, windspeed, "_FillValue")
dim(wind.array)
#wind.array[wind.array==fillvalue$value] <- NA

indices <- expand.grid(lon, lat, time.conv)

#for wind
wind.vec <- as.vector(wind.array)
length(wind.vec)
wind <- data.frame(cbind(indices, wind.vec))
names(wind) <- c("lon", "lat", "date", paste(windspeed))
head(na.omit(wind), 5)

##calculate disrate
#constants
K= 0.4 #von Karman's constant
Cd <- 1.15*(10)^-3 #drag coefficient
Den_air <-  (1.212)*10^-3 #g/m3 at sea level and 18C
Den_CH2O= 1.025 #g/m3 density seawater at 18C #these two will even out so no need to convert
wind$U <- sqrt(Cd*((wind$wind_speed)^2)*((Den_air/Den_CH2O)))
wind$disrate_1m <- (wind$U^3)/(K*1) #1 here is depth
wind$disrate_30m <- (wind$U^3)/(K*30) #30 here is depth


##all NA
mylog10_trans <- function (base = 10) 
{
  trans <- function(x) log(x + 1, base)
  inv <- function(x) base^x
  trans_new(paste0("log-", format(base)), trans, inv, log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

library(scales)
ggplot(data=wind, aes(x = wind_speed)) + geom_histogram(alpha = 0.5, position = "stack", binwidth = 0.5, color="#525252") + 
  theme_Publication2() + theme (legend.key.width = unit (1, "cm")) + labs (x=expression ("wind speed"~(m~s^-1))) +
  scale_y_continuous(trans = "log10", breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)))


#navice_slice <- wind %>% filter (between (lat, 50, 70)) %>% filter (between (lon, -40, -20))

#get range of coordinates for EI, LI, PI, and CI

EI <- wind %>% filter (between (lat, 61, 62)) %>% filter (between (lon, -35, -33))
LI <- wind %>% filter (between (lat, 63, 64)) %>% filter (between (lon, -33, -32))
PI <- wind %>% filter (between (lat, 53, 54)) %>% filter (between (lon, -31, -30))
CI <- wind %>% filter (between (lat, 65, 65.5)) %>% filter (between (lon, -26.5, -26))

EI$station = "EI"
LI$station = "LI"
PI$station = "PI"
CI$station = "CI"

#rbind all NAVICE stations
navice_slice <- rbind (EI, LI, PI, CI)

##extract months
navice_slice$month <- month(navice_slice$date)

library(zoo)
yq <- as.yearqtr(as.yearmon(navice_slice$date, "%Y-%m/%d") + 1/12)
navice_slice$season <- factor(format(yq, "%q"), levels = 1:4, 
                    labels = c("winter", "spring", "summer", "fall"))

ggplot(data=navice_slice, aes((wind_speed))) + geom_histogram()

resize.win(12,6)
ggplot(data=navice_slice, aes(x = wind_speed, fill = station)) + geom_histogram(alpha = 0.4, position = "stack", binwidth = 1) + 
  theme_Publication2() + theme (legend.key.width = unit (1, "cm")) + labs (x=expression ("wind speed"~(m~s^-1))) #+ facet_grid(~station)

#stations seems like the same

ggplot(data=navice_slice, aes(x = wind_speed, fill = season)) + geom_histogram(alpha = 0.4, position = "stack", binwidth = 1) + 
  theme_Publication2() + theme (legend.key.width = unit (1, "cm")) + labs (x=expression ("wind speed"~(m~s^-1))) + facet_grid(station~season)

#disrate 30m
ggplot(data=navice_slice, aes(x = log10(disrate_30m), fill = season)) + geom_histogram(alpha = 0.4, position = "stack", binwidth = 0.1) + 
  theme_Publication2() + theme (legend.key.width = unit (1, "cm")) + labs (x=expression(log[10]~epsilon~(m^2~s^-3))) + 
  facet_grid(station~season)

#stations are the same, so collate
ggplot(data=navice_slice, aes(x = log10(disrate_30m), fill = season)) + geom_histogram(alpha = 0.4, position = "stack", binwidth = 0.1) + 
  theme_Publication2() + theme (legend.key.width = unit (1, "cm")) + labs (x=expression(log[10]~epsilon~(m^2~s^-3))) + 
  facet_grid(~season)

ggplot(data=navice_slice, aes(x = log10(disrate_1m), fill = season)) + geom_histogram(alpha = 0.4, position = "stack", binwidth = 0.1) + 
  theme_Publication2() + theme (legend.key.width = unit (1, "cm")) + labs (x=expression(log[10]~epsilon~(m^2~s^-3))) + 
  facet_grid(station~season)

ggplot(data=navice_slice, aes(x = log10(disrate_1m), fill = season)) + geom_histogram(alpha = 0.4, position = "stack", binwidth = 0.1) + 
  theme_Publication2() + theme (legend.key.width = unit (1, "cm")) + labs (x=expression(log[10]~epsilon~(m^2~s^-3))) + 
  facet_grid(~season)

ggplot(data=navice_slice, aes(x = log10(disrate_30m), fill = season)) + geom_histogram(alpha = 0.4, position = "stack", binwidth = 0.1) + 
  theme_Publication2() + theme (legend.key.width = unit (1, "cm")) + labs (x=expression(log[10]~epsilon~(m^2~s^-3))) + 
  facet_wrap(~month)

ggplot(data=navice_slice %>% filter (month=="7"), aes(x = date, y=log10(disrate_30m), fill = station)) + 
 geom_line() + 
  theme_Publication2() + theme (legend.key.width = unit (1, "cm")) + labs (x=expression(log[10]~epsilon~(m^2~s^-3))) + 
  facet_wrap(~station)

ggplot(data=navice_slice, aes(x = date, y=wind_speed, color=station)) + 
  geom_line() + 
  theme_Publication2() + theme (legend.key.width = unit (1, "cm"), axis.text.x = element_text(angle=45, hjust=1.2), 
                                axis.title.x = element_blank()) +
  labs (y=expression ("wind speed"~(m~s^-1))) #+   facet_wrap(~station)

LI.lehahn <- LI %>% filter (between (date, as.POSIXct("2012-05-29"), as.POSIXct("2012-07-18")))
CI.slice <- CI %>% filter (between (date, as.POSIXct("2012-05-29"), as.POSIXct("2012-07-18")))
navice.speed <- navice_slice %>% filter (between (date, as.POSIXct("2012-05-29"), as.POSIXct("2012-07-29")))

ggplot(data=navice.speed, aes(x = date, y=wind_speed, color=station)) + geom_line() + #geom_smooth() +
  theme_Publication2() + theme (legend.key.width = unit (1, "cm"), #axis.text.x = element_text(angle=45, hjust=1.2), 
                                axis.title.x = element_blank()) +
  labs (y=expression ("wind speed"~(m~s^-1))) +   facet_wrap(~station)

ggplot(data=navice.speed, aes(x = date, y=log10(disrate_1m), color=station)) + geom_line() + #geom_smooth() +
  theme_Publication2() + theme (legend.key.width = unit (1, "cm"), #axis.text.x = element_text(angle=45, hjust=1.2), 
                                axis.title.x = element_blank()) +
  labs (y=expression(log[10]~epsilon~(m^2~s^-3))) +   facet_wrap(~station)

##final plots
resize.win (6,6)
ggplot(data=navice_slice, aes(x = wind_speed)) + 
  geom_histogram(alpha = 0.5, position = "stack", binwidth = 0.5, color="#525252",  fill="#525252") + 
  theme_Publication2() + theme (legend.key.width = unit (1, "cm")) + labs (x=expression ("wind speed"~(m~s^-1)))

ggplot(data=navice_slice, aes(x = log10(disrate_1m))) + 
  geom_histogram(alpha = 0.5, position = "stack", binwidth = 0.05, color="#ae017e", fill="#ae017e") + 
  theme_Publication2() + theme (legend.key.width = unit (1, "cm")) + labs (x=expression(log[10]~epsilon~(m^2~s^-3))) 

ggplot(data=navice_slice, aes(x = log10(disrate_30m))) + 
  geom_histogram(alpha = 0.5, position = "stack", binwidth = 0.05, color="#f768a1", fill="#f768a1") + 
  theme_Publication2() + theme (legend.key.width = unit (1, "cm")) + labs (x=expression(log[10]~epsilon~(m^2~s^-3))) 
