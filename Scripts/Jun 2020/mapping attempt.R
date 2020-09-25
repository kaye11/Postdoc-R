source("inspack_map.R")
library(marmap)

atl_raw <- getNOAA.bathy(-70,10,30,75,res=10, keep=TRUE)

library("ggplot2")
# convert bathy object into a data.frame
head(fortify(atl_raw))

# one can now use bathy objects with ggplot directly
ggplot(atl) + geom_contour(aes(x=x, y=y, z=z)) + coord_map() + theme_Publication2()

# which allows complete plot configuration
atl.df <- fortify(atl_raw)
atl.df2 <- filter (atl.df, z>(-201) & z<(-99))
ggplot(atl.df, aes(x=x, y=y)) + 
  #geom_raster(aes(fill=z), data=atl.df[atl.df$z <= 0,]) +
  geom_contour(aes(z=z),colour="black", size=0.1
  ) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))

atl <- fortify(atl.df2)
atl <- as.bathy(atl)
autoplot(atl, coast = "TRUE") + theme_Publication2()

#resize.win(6.6)
last_plot() + theme_Publication2()

#how about just get all the data and use it for mapping (overlay the coloring)
write.table (wind, "D:/Users/karengb/Exported Tables/atlalldata.csv", sep=",", col.names=T, row.names=F)

