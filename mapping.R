### Shape file 

###packages
library(sf)
library(ggplot2)
#save path
graph_path <- "E:/STAT685/Graphs/"
#aoi_boundry
aoi_boundary_texas <- st_read(
  "E:/STAT685/Data/shapefile/Current_Districts.shp")

#join on ethnicity data
for (year in c(13,14,15,16,17)){
hisp <- all_years[all_years$YEAR==year,c("DISTRICT","DPETHISP")]
print(head(hisp))
names(hisp)[1] <- "DISTRICT_N"
hisp_shp <- merge(aoi_boundary_texas, hisp, by='DISTRICT_N', all.x=TRUE)
plot<-ggplot(hisp_shp) + 
  geom_sf(size=.5, color = "black", aes(fill = DPETHISP)) + 
  ggtitle(paste0("Percentage Hispanic in 20",year)) + 
  scale_fill_gradient2( low = "red", mid = "white",
                       high = "green", space = "Lab", limits=c(0,100),
                       na.value = "grey50", guide = "colourbar", aesthetics = "fill") +
  coord_sf()
png(paste0(graph_path,"TexasHisp",year,".png"), width=648, height=432)
print(plot)
dev.off()
}

#there may be an increasing trend in the average score over time
#does the test change difficulty or kids get smarter?
#join on test data
for (year in c(12,13,14,15,16,17,18,19)){
  hisp <- all_years[all_years$YEAR==year,c("DISTRICT","a1_all_rs")]
  print(head(hisp))
  names(hisp)[1] <- "DISTRICT_N"
  hisp_shp <- merge(aoi_boundary_texas, hisp, by='DISTRICT_N', all.x=TRUE)
  plot<-ggplot(hisp_shp) + 
    geom_sf(size=.5, color = "black", aes(fill = a1_all_rs)) + 
    ggtitle(paste0("Average Scale Score Algebra I 20",year)) + 
    coord_sf()
  png(paste0(graph_path,"a1_all_rs",year,".png"), width=648, height=432)
  print(plot)
  dev.off()
}