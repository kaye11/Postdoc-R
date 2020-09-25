########################################################################
# setup
########################################################################

# creating some sample data for one year
# 4 categories; each category has a specific value per day
set.seed(1)
x <- data.frame(
  rep(as.Date((Sys.Date()-364):Sys.Date(), origin="1970-01-01"),4),
  c(rep("cat01",length.out=365),
    rep("cat02",length.out=365),
    rep("cat03",length.out=365),
    rep("cat04",length.out=365)),
  sample(0:50,365*4, replace=TRUE)
)
colnames(x) <- c("date", "category", "value")

# creating a cumulative measure making the graphs appear "growing"
library(dplyr)
x <- x %>%
  as_tibble() %>%
  arrange(date) %>%
  mutate(date = as.character(date)) %>%
  group_by(category) %>%
  mutate(cumsum = cumsum(value))

y_max <- max(x$cumsum) + 500

library(doParallel)

all_dates <- unique(x$date)
ncores <- detectCores() - 1
ind_cluster <- sort(rep_len(1:ncores, length(all_dates)))
date_cluster <- split(all_dates, ind_cluster)
registerDoParallel(cl <- makeCluster(ncores))

tmp <- tempfile()

files <- foreach(ic = 1:ncores, .packages = c("tidyverse", "magick")) %dopar% {
  
  img <- image_graph(1000, 700, res = 96)
  
  x %>%
    filter(date %in% date_cluster[[ic]]) %>%
    group_by(date) %>%
    do(
      plot = ggplot(.) +
        geom_col(aes(category, cumsum)) +
        scale_y_continuous(expand = c(0, 0), 
                           breaks = seq(0, y_max, 500), 
                           limits = c(0, y_max))
    ) %>%
    pmap(function(date, plot) {
      print(plot + ggtitle(date))
      NULL
    })
  
  dev.off()
  
  image_write(image_animate(img, fps = 5), paste0(tmp, ic, ".gif"))
}
stopCluster(cl)

test <- do.call(c, lapply(files, magick::image_read))
test