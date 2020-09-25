
#calculate virnum here depending on Cc concentration
source("inspack.R")

#toby's data
NA_abundancedata<- read.csv("D:/Users/karengb/NA simulation data/cocco_work4Karen/cocco_work4Karen_withrownames.txt", sep="")

NA_abundancedata <- NA_abundancedata  %>%  filter (!(waterdepth_m == "NA")) %>% filter (!(Ncells_max < 10)) %>%  filter (!(Ncells_max == "NA"))
NA_abundancedata$depthf <- if_else(NA_abundancedata$waterdepth_m <200, "<200 m", ">200 m")

#NA_abundancedata$depthf100 <- if_else(NA_abundancedata$waterdepth_m<100, "<100 m", ">100 m")

##calculate virnum depending on Ncells_max (1:27, host:virus ratio)
NA_abundancedata$Nvirnum_Cc <- NA_abundancedata$Ncells_max*27
NA_abundancedata$Nvirnum_Li <- NA_abundancedata$Nvirnum_Cc

#melt data (wide to long)
NA_abundancedata_entity <- reshape2::melt (NA_abundancedata %>% select (lat, lon, waterdepth_m,depthf, Ncells_max, Nliths_max), id.vars = c("lat", "lon", "waterdepth_m", "depthf"), value.name = "abundance", variable.name = "entity")

NA_abundancedata_virnum <- reshape2::melt (NA_abundancedata %>% select (lat, lon, waterdepth_m,depthf, Nvirnum_Cc, Nvirnum_Li), id.vars = c("lat", "lon", "waterdepth_m", "depthf"), value.name = "virnum", variable.name = "entity")

NA_abundancedata_entity$virnum <- NA_abundancedata_virnum$virnum

NA_abundancedata$ratcoc <- NA_abundancedata$Nliths_max/NA_abundancedata$Ncells_max
NA_abundancedata.ratcoc.mean <- summarise (NA_abundancedata, ratcoc.mean=mean(ratcoc, na.rm=TRUE), 
                                           ratcoc.median=median(ratcoc, na.rm=TRUE))

#melted_NAabundancedata <- NA_abundancedata_entity %>% filter (!(abundance < 10)) %>%  filter (!(abundance == "NA"))

melted_NAabundancedata <- NA_abundancedata_entity

melted_NAabundancedata$entitycode <- melted_NAabundancedata$entity

melted_NAabundancedata$entity <- case_when(
  melted_NAabundancedata$entitycode == "Ncells_max"  ~ "Cc",
  melted_NAabundancedata$entitycode == "Nliths_max"  ~ "Li",
  TRUE ~ as.character(melted_NAabundancedata$entity)
)

write.table (melted_NAabundancedata, "D:/Users/karengb/Exported Tables/NAdata_mergewithAli_withvirnum_alldepths.csv", sep=",", col.names=T, row.names=F)


