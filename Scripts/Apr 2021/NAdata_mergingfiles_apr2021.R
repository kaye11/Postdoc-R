##read data
#for annotate
winddisrate <- read.csv("D:/Users/karengb/Exported Tables/winddisrate_mar2021.csv")
NAdata <- read.csv("D:/Users/karengb/Exported Tables/NAdata_entityvirconc_apr2021.csv") 

#libraries
library(geosphere)
library(doParallel)
library(itertools)
library(dplyr)

chunk = 1000
NA_chunks <- split(NAdata, rep(1:ceiling(nrow(NAdata)/1000), each=chunk, length.out=nrow(NAdata)))

ncores <- 40
cl <- makePSOCKcluster (ncores)
doParallel::registerDoParallel(cl)

myfun <- function(x,y) {
  mat = distm(x[,c('lon','lat')], y[,c('lon','lat')], fun=distHaversine)
  x$max_wind_speed=y$max_wind_speed[apply(mat, 1, which.min)]
  x$disrate_1m=y$disrate_1m[apply(mat, 1, which.min)]
  x$disrate_30m=y$disrate_30m[apply(mat, 1, which.min)]
  return(x)
}

Sys.time()
NAdata_merge_raw <- foreach(m=NA_chunks, .combine='rbind',.packages='geosphere') %dopar% {
                myfun(m, winddisrate) 
}
Sys.time()
memory.size()

stopCluster(cl)
stopImplicitCluster()
##15 min

##delete rows that have no match

NAdata_merge <- NAdata_merge_raw %>% filter (!(max_wind_speed=="NA"))

write.table (NAdata_merge, "Exported Tables/NAdata_merge_apr2021.csv", sep=",", col.names=T, row.names=F)

