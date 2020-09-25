##read data
#winddisrate_2010 <- read.csv("D:/R program/Postdoc-R/Exported Tables/winddisrate_2010.csv")
#NAdata <- read.csv("D:/R program/Postdoc-R/Exported Tables/NAdata_mergewithAli.csv")

#for annotate
NAdata_merge <- read.csv("D:/Users/karengb/Exported Tables/NAdata_merge_masked.csv")
atl <- read.csv("D:/Users/karengb/Exported Tables/atl.csv", sep=",")

atl$lat <- atl$y
atl$lon <- atl$x

#libraries
#source("inspack_map.R")
library(geosphere)
library(doParallel)
library(itertools)
library(dplyr)

chunk = 1000
#NAdata_split1 <-  NAdata [1:5000, ]
NA_chunks <- split(NAdata_merge, rep(1:ceiling(nrow(NAdata_merge)/1000), each=chunk, length.out=nrow(NAdata_merge)))

ncores <- 25
cl <- makePSOCKcluster (ncores)
doParallel::registerDoParallel(cl)

myfun <- function(x,y) {
  mat = distm(x[,c('lon','lat')], y[,c('lon','lat')], fun=distHaversine)
  x$z=y$z[apply(mat, 1, which.min)]
  return(x)
}

Sys.time()
NAdata_merge_bath <- foreach(m=NA_chunks, .combine='rbind',.packages='geosphere') %dopar% {
  myfun(m, atl) 
}
Sys.time()
memory.size()

stopCluster(cl)
stopImplicitCluster()
##6 min

#save the data to free memory
write.table (NAdata_merge_bath, "Exported Tables/NAdata_merge_masked_bathy.csv", sep=",", col.names=T, row.names=F)

#note: data is already melted. ready for plotting and probs