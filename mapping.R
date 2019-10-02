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
for (year in c(12,13,14,15,16,17)){
hisp <- all_years[all_years$YEAR==year
                  ,c("DISTRICT","DPETHISP","DPETBLAP","DPETWHIP","DPETASIP")]
print(head(hisp))
names(hisp)[1] <- "DISTRICT_N"
hisp_shp <- merge(aoi_boundary_texas, hisp, by='DISTRICT_N', all.x=TRUE)
p1<-ggplot(hisp_shp) + 
  geom_sf(size=.1, color = "black", aes(fill = DPETHISP)) + 
  ggtitle(paste0("Percentage Hispanic in 20",year)) + 
  scale_fill_gradient(low="blue", high="red", midpoint=4000) +
  coord_sf()
p2<-ggplot(hisp_shp) + 
  geom_sf(size=.1, color = "black", aes(fill = DPETBLAP)) + 
  ggtitle(paste0("Percentage Black in 20",year)) + 
  scale_fill_gradient(low="blue", high="red") +
  coord_sf()
p3<-ggplot(hisp_shp) + 
  geom_sf(size=.1, color = "black", aes(fill = DPETWHIP)) + 
  ggtitle(paste0("Percentage White in 20",year)) + 
  scale_fill_gradient(low="blue", high="red") +
  coord_sf()
p4<-ggplot(hisp_shp) + 
  geom_sf(size=.1, color = "black", aes(fill = DPETASIP)) + 
  ggtitle(paste0("Percentage Asian in 20",year)) +
  scale_fill_gradient(low="blue", high="red") +
  coord_sf()
png(paste0(graph_path,"TexasRaces",year,".png"), width=800, height=600)
multiplot(p1, p2, p3, p4, cols=2)
dev.off()
}

# do a 4 by 4 white, black, hispanic, asian, other?

#there may be an increasing trend in the average score over time
#does the test change difficulty or kids get smarter?
# need one scale, scale changes acorss years
#join on test data
for (year in c(12,13,14,15,16,17,18,19)){
  hisp <- all_years[all_years$YEAR==year,c("DISTRICT","a1_all_rs")]
  print(head(hisp))
  names(hisp)[1] <- "DISTRICT_N"
  hisp_shp <- merge(aoi_boundary_texas, hisp, by='DISTRICT_N', all.x=TRUE)
  plot<-ggplot(hisp_shp) + 
    geom_sf(size=.5, color = "black", aes(fill = a1_all_rs)) + 
    scale_fill_gradient(low = "green", high = "blue",limits = c(3000,6000)) +
    ggtitle(paste0("Average Scale Score Algebra I 20",year)) + 
    coord_sf()
  png(paste0(graph_path,"a1_all_rs",year,".png"), width=648, height=432)
  print(plot)
  dev.off()
}