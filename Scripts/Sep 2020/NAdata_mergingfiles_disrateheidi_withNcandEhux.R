##read data
#winddisrate_2010 <- read.csv("D:/R program/Postdoc-R/Exported Tables/winddisrate_2010.csv")
#NAdata <- read.csv("D:/R program/Postdoc-R/Exported Tables/NAdata_mergewithAli.csv")

#for annotate
winddisrate <- read.csv("D:/Users/karengb/Exported Tables/winddisrate_heidi.csv")
NAdata <- read.csv("D:/Users/karengb/Exported Tables/NAdata_mergewithAli_withvirnum_raw_withallcells_200916.csv") #masked 100 m removed

#libraries
#source("inspack_map.R")
library(geosphere)
library(doParallel)
library(itertools)
library(dplyr)

chunk = 1000
#NAdata_split1 <-  NAdata [1:5000, ]
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
##13 min

#save the data to free memory
#write.table (NAdata_merge_raw, "Exported Tables/NAdata_merge_raw_disrateheidi.csv", sep=",", col.names=T, row.names=F)

NAdata_merge <- NAdata_merge_raw %>% filter (!(waterdepth_m < 100))  %>%  filter (!(waterdepth_m == "NA"))

write.table (NAdata_merge, "Exported Tables/NAdata_merge_masked_withNcandEhux.csv", sep=",", col.names=T, row.names=F)

#note: data is already melted. ready for plotting and probs