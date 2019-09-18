### Shape file 

###packages
library(sf)
library(ggplot2)

aoi_boundary_texas <- st_read(
  "N:\\SanAntonio\\Dept\\Completions\\Public\\Public_PVictor\\STATS\\TAMU\\STAT 685 - DIRECTED STUDIES\\SHP\\Current_Districts.shp")

g <- ggplot() + 
  geom_sf(data = aoi_boundary_texas, size = 1, color = "black", aes(fill = a1_migy_d)) + 
  ggtitle("AOI Boundary Plot") 
  # coord_sf() 

# py$link_sp


plotly::ggplotly(g) %>% 
  highlight(
    "plotly_hover",
    selected = attrs_selected(line = list(color = "black"))
  ) %>%
  widgetframe::frameWidget()
