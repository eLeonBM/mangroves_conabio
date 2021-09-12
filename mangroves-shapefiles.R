library(sp)
library(tidyverse)
library(rnaturalearth)
library(ggthemes)
library(hrbrthemes)
library(patchwork)
library(sf)
library(ggplot2)

dist2015 <- read_sf("data/Distribucion/Distribucion_2015/mx_man15gw.shp")
use2020 <- st_read("data/Distribucion/Uso-de-suelo_2020/mx_oc2020gw.shp")


## Transform coordinates
dist2015 <- st_transform(dist2015, crs = 6372)
use2020 <- st_transform(use2020, crs = 6372)



#6372 = Mexico
#4326= WGS84
######## correct geometry

simpl_use <- st_simplify(use2020,dTolerance = 2)
simpl_dist <- st_simplify(dist2015, dTolerance = 2)
simpl_dist <- st_transform(simpl_dist, crs = 4326)

st_crs(antropico)

sf::sf_use_s2(FALSE)


#Intersection 
antropico <- st_intersection(simpl_dist, simpl_use[simpl_use$Descrip == "Desarrollo antropico",])
antropico <- st_transform(antropico, crs = 6372)
antropico$area_ha<- st_area(antropico)/10000 #Area with Mexican CRS
antropico <- st_transform(antropico, crs = 4326) #wgs84 CRS
antropico$area_wgs84<- st_area(antropico)/10000 #Area with wgs84 CRS

antropic <- antropico %>% 
  as.data.frame() %>% 
  select(Descrip.1,COV_ID, area_ha) %>% 
  rename(antropic=area_ha
          
  )




agricola <- st_intersection(simpl_dist, simpl_use[simpl_use$Descrip == "Agricola - Pecuaria",])
agricola <- st_transform(agricola, crs = 6372)
agricola$area_ha<- st_area(agricola)/10000 #Area with Mexican CRS
agricola <- st_transform(agricola, crs = 4326) #wgs84 CRS
agricola$area_wgs84<- st_area(agricola)/10000 #Area with wgs84 CRS

agricola_area <- agricola %>% 
  as.data.frame() %>% 
  select(Descrip.1,COV_ID, area_ha) %>% 
  rename(agriculture=area_ha
         
  )


sin_vegetacion <- st_intersection(simpl_dist, simpl_use[simpl_use$Descrip == "Sin vegetacion",])
sin_vegetacion <- st_transform(sin_vegetacion, crs = 6372)
sin_vegetacion$area_ha<- st_area(sin_vegetacion)/10000 #Area with Mexican CRS
sin_vegetacion <- st_transform(sin_vegetacion, crs = 4326) #wgs84 CRS
sin_vegetacion$area_wgs84<- st_area(sin_vegetacion)/10000 #Area with wgs84 CRS

veg_loss <- sin_vegetacion %>%
  as.data.frame() %>% 
  select(Descrip.1,COV_ID, area_ha) %>% 
  rename(vegetation_loss=area_ha
         
  )

perturbado <- st_intersection(simpl_dist, simpl_use[simpl_use$Descrip == "Manglar perturbado",])
perturbado <- st_transform(perturbado, crs = 6372)
perturbado$area_ha<- st_area(perturbado)/10000 #Area with Mexican CRS
perturbado <- st_transform(perturbado, crs = 4326) #wgs84 CRS
perturbado$area_wgs84<- st_area(perturbado)/10000 #Area with wgs84 CRS



disturbed <- perturbado %>% 
  as.data.frame() %>% 
  select(Descrip.1,COV_ID, area_ha) %>% 
  rename(disturbed=area_ha
         
  )


agua <- st_intersection(simpl_dist, simpl_use[simpl_use$Descrip == "Cuerpos de agua",])
agua <- st_transform(agua, crs = 6372)
agua$area_ha<- st_area(agua)/10000 #Area with Mexican CRS
agua <- st_transform(agua, crs = 4326) #wgs84 CRS
agua$area_wgs84<- st_area(agua)/10000 #Area with wgs84 CRS



water <- agua %>% 
  as.data.frame() %>% 
  select(Descrip.1,COV_ID, area_ha) %>% 
  rename(water=area_ha
         
  )

st_crs(dist2015)
dist2015$area2_ha <- st_area(dist2015)/10000
names(dist2015)
  
mangrove_areas <- dist2015 %>% 
  as.data.frame() %>% 
  select(Descrip, COV_ID, area2_ha) %>% 
  rename(area2015_ha=area2_ha)







agua <-st_collection_extract(agua, "POLYGON")
perturbado <-st_collection_extract(perturbado, "POLYGON")
antropico <- st_collection_extract(antropico, "POLYGON")
agricola <-st_collection_extract(agricola, "POLYGON")
sin_vegetacion <-st_collection_extract(sin_vegetacion, "POLYGON")

st_write(antropico, "data/output/intersection_2015-2020/antropico.shp")
st_write(agua, "data/output/intersection_2015-2020/agua.shp")
st_write(perturbado, "data/output/intersection_2015-2020/perturbado.shp")
st_write(sin_vegetacion, "data/output/intersection_2015-2020/sin_vegetacion.shp")
st_write(agricola, "data/output/intersection_2015-2020/agricola.shp")

