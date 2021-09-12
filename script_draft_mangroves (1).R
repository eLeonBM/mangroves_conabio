library(sf)
library(tidyverse)

distribucion2015 <- st_read("data/Distribución/Distribucion_2015/mx_man15gw.shp")
land_use2020 <- st_read("data/Distribución/Uso-de-suelo_2020/mx_oc2020gw.shp")



land2020<- st_make_valid(land_use2020)
  dist2015 <- st_make_valid(distribucion2015) 

inters_test <- st_intersection(dist2015, land2020)


