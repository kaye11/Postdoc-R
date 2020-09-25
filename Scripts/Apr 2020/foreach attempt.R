library(doParallel)

all_entities <- unique(melted_NAdata.betas_comb$entity)
ncores <- detectCores() - 1
ind_cluster <- sort(rep_len(1:ncores, length(all_entities)))
entity_cluster <- split(all_entities, ind_cluster)
registerDoParallel(cl <- makeCluster(ncores))

tmp <- tempfile()

files <- foreach(ic = 1:ncores, .packages = c("tidyverse")) %dopar% {
  melted_NAdata.betas_comb %>%
    filter(entity %in% entity_cluster[[ic]]) %>%
    group_by(entity) %>%
    do(
      plot = ggplot(.) +
        geom_sf (data = world, color=NA)+ coord_sf(xlim = c(-70, 10), ylim = c(30, 75) , expand = FALSE)+ 
        geom_point(aes(x = lon, y = lat, color = log10(value)))+
        scale_color_viridis(breaks = c(seq(-6, 10, 2))) + theme_Publication2() + theme(legend.position = "right", legend.direction = "vertical", legend.key.height = unit (8, "line"), legend.key.width = unit (2, "line"),  legend.title=element_blank(), panel.spacing = unit(3, "lines"), axis.title = element_blank()) + facet_grid(watcon~step2) 
      
    ) %>%
    pmap(function(date, plot) {
      print(plot + ggtitle(date))
      NULL
    })
  
  dev.off()
  
  image_write(image_animate(img, fps = 5), paste0(tmp, ic, ".gif"))
}

library(parallel)
library(doParallel)
library(foreach)
library()
cl <- makeCluster(5)
doParallel::registerDoParallel(cl)
entities <- unique(melted_NAdata.betas_comb$entity)

Sys.time()
foreach(i=1:length(entities), .packages = c("tidyverse", "dplyr")) %dopar% {
  ggplot (data=world) + geom_sf(color="white", fill="white") +  coord_sf(xlim = c(-34,-30), ylim = c(53,62) , expand = FALSE) + theme_Publication2() + geom_point(subset(melted_NAdata.betas_comb, melted_NAdata.betas_comb$entity==entities[i]), mapping=aes(x = lon, y = lat, color = log10(value))) + facet_grid(watcon~step2) +
    ggtitle(entities[i])
  ggsave(filename = paste0(i, "_10cores",".png"))
}
Sys.time()

# Real physical cores in my computer
cores <- detectCores(logical = FALSE)
cl <- makeCluster(10)
registerDoParallel(cl, cores=cores)

# clusterSplit are very convience to split data but it takes lots of extra memory
# chunks <- clusterSplit(cl, 1:len)

# split data by ourselves
chunk.size <- entities/cores





entity.graph <- function(df, na.rm = TRUE, ...){
  
  # create list of counties in data to loop over 
  entity_list <- unique(df$entity)
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(entity_list)) { 
    
    # create plot for each county in df 
    plot <- 
      ggplot(data = world)+ geom_sf(color="white", fill="white") + coord_sf(xlim = c(-70, -10), ylim = c(30,75) , expand = FALSE)+  geom_point(subset(df, df$entity==entity_list[i]), mapping=aes(x = lon, y = lat, color = log10(value)))+
      facet_grid (watcon~step2) + theme_Publication2() +
      ggtitle(paste(entity_list[i]))
    ggsave(filename = paste0(i, "forloop", ".png"))
  }
}

Sys.time()
entity.graph (melted_NAdata.betas_comb)
Sys.time()

