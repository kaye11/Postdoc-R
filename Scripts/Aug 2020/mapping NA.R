library(marmap)
library(oce)
library(ocedata)
data("coastlineWorldFine")
source("inspack.R")

b = getNOAA.bathy(lon1 = -70, lon2 = 10, lat1 = 30, lat2 = 75, resolution = 1)

# convert bathymetry
bathyLon = as.numeric(rownames(b))
bathyLat = as.numeric(colnames(b))
bathyZ = as.numeric(b)
dim(bathyZ) = dim(b)

## Save it to a function to make it easy to re-run
mp <- function() {
  mapPlot(coastlineWorldFine,  projection="+proj=merc",
          longitudelim = c(-70, 10),
          latitudelim = c(30, 75), col='gainsboro', border="grey")
}

resize.win (6,7)

mp()

mapContour(bathyLon,bathyLat,bathyZ,
           levels = c(-100, -200),
           col = 'black', drawlabels=TRUE )


#add coords for NA-VICE

pts <- read.csv("D:/Users/karengb/CSV Files/coords.txt", sep=";")
pts = pts[-4,]

mapPoints(longitude = pts$lon, latitude = pts$lat, pch = 16, col = 'red', cex=2)

##NAVICE
navice <- function() {
  mapPlot(coastlineWorldFine,  projection="+proj=merc",
          longitudelim = c(-40, -20),
          latitudelim = c(50, 70), col='gainsboro', border="grey")
}

resize.win (4,6)

navice()

mapContour(bathyLon,bathyLat,bathyZ,
           levels = c(-100, -200),
           col = 'black', drawlabels=TRUE )

mapPoints(longitude = pts$lon, latitude = pts$lat, pch = 16, col = '#de2d26', cex=2)
