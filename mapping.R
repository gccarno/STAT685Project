### Shape file 

###packages
library(sf)
library(ggplot2)

aoi_boundary_texas <- st_read(
  "E:/STAT685/Data/shapefile/Current_Districts.shp")

ggplot() + 
  geom_sf(data = aoi_boundary_texas,size=1, color = "black", fill = "cyan1") + 
  ggtitle("Texas Schools") + 
  coord_sf()
