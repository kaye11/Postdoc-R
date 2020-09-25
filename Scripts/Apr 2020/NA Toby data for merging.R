#hoping this wouldn't break R attempt lol
source("inspack.R")

#toby's data
NA_abundancedata<- read.csv("D:/Postdoc/theoretical/NA simulation data/cocco_work4Karen/cocco_work4Karen_withrownames.txt", sep="")

NA_abundancedata$depthf <- if_else(NA_abundancedata$waterdepth_m <200, "<200 m", ">200 m")
#NA_abundancedata$depthf100 <- if_else(NA_abundancedata$waterdepth_m<100, "<100 m", ">100 m")

#melt data (wide to long)
melted_NAabundancedata <- reshape2::melt (NA_abundancedata, id.vars = c("lat", "lon", "waterdepth_m", "depthf"), value.name = "abundance", variable.name = "entity")

melted_NAabundancedata <- melted_NAabundancedata %>% filter (!(abundance < 10)) %>%  filter (!(abundance == "NA"))

melted_NAabundancedata$entitycode <- melted_NAabundancedata$entity

melted_NAabundancedata$entity <- case_when(
  melted_NAabundancedata$entitycode == "Ncells_min"  ~ "Cc",
  melted_NAabundancedata$entitycode == "Ncells_max"  ~ "Cc",
  melted_NAabundancedata$entitycode == "Nliths_min"  ~ "Li",
  melted_NAabundancedata$entitycode == "Nliths_max"  ~ "Li",
  TRUE ~ as.character(melted_NAabundancedata$entity)
)

NAdata <- melted_NAabundancedata %>% filter (entitycode %in% c("Ncells_max", "Nliths_max")) 

write.table (NAdata, "Postdoc-R/Exported Tables/NAdata_mergewithAli_rawer.csv", sep=",", col.names=T, row.names=F)
