#install.packages("tidyverse")
#install.packages("sf")
library(tidyverse) 
library(sf)

# load a geopackage or shapefile called "geodata"
geodata <- st_read("geodata.gpkg")
  
# map variable VAR of the geodata

ggplot(data = geodata) +
  geom_sf(aes(fill = VAR)  +
  scale_fill_viridis_c(option = "viridis"))

