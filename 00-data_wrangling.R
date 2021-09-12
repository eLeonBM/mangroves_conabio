library(sp)
library(tidyverse)
library(patchwork)
library(sf)


# Load data------

## COnabio's Mexican Mangrove Distribution (2015)
dist2015 <- read_sf("data/Distribucion/Distribucion_2015/mx_man15gw.shp")

## COnabio's Coastal Land Use Cover (2020)
use2020 <- st_read("data/Distribucion/Uso-de-suelo_2020/mx_oc2020gw.shp")



## Transform coordinates
dist2015 <- st_transform(dist2015, crs = 6372)
use2020 <- st_transform(use2020, crs = 6372)

#6372 = Mexico
#4326= WGS84


## Correct Geometry, simplifying polygons

simpl_use <- st_simplify(use2020,dTolerance = 2)
simpl_dist <- st_simplify(dist2015, dTolerance = 2)



sf::sf_use_s2(FALSE)


# Intersections------
unique(use2020$Descrip)

## Mangrove 2015 vs Mangrove 2020
manglar <- st_intersection(simpl_dist, simpl_use[simpl_use$Descrip == "Manglar",])
manglar <- st_transform(manglar, crs = 6372)
manglar$area_ha<- st_area(manglar)/10000 #Area with Mexican CRS

mangrove <- manglar %>%
  as.data.frame() %>% 
  select(Descrip.1,COV_ID, area_ha) %>% 
  rename(area2020=area_ha
         
  )

## Anthropic Developement Mangrove change
antropico <- st_intersection(simpl_dist, simpl_use[simpl_use$Descrip == "Desarrollo antropico",])#Intersection between 2015's mangrove cover and 2020's land use cover
antropico <- st_transform(antropico, crs = 6372)
antropico$area_ha<- st_area(antropico)/10000 #Area with Mexican CRS
# antropico <- st_transform(antropico, crs = 4326) #wgs84 CRS
# antropico$area_wgs84<- st_area(antropico)/10000 #Area with wgs84 CRS

antropic <- antropico %>% 
  as.data.frame() %>% 
  select(Descrip.1,COV_ID, area_ha) %>% 
  rename(antropic=area_ha
         
          
  )

##Agricultural and Livestock Mangrove change
agricola <- st_intersection(simpl_dist, simpl_use[simpl_use$Descrip == "Agricola - Pecuaria",])
agricola <- st_transform(agricola, crs = 6372)
agricola$area_ha<- st_area(agricola)/10000 #Area with Mexican CRS
# agricola <- st_transform(agricola, crs = 4326) #wgs84 CRS
# agricola$area_wgs84<- st_area(agricola)/10000 #Area with wgs84 CRS

agricultural <- agricola %>% 
  as.data.frame() %>% 
  select(Descrip.1,COV_ID, area_ha) %>% 
  rename(agriculture=area_ha
         
  )

## Vegetation loss Mangrove change
sin_vegetacion <- st_intersection(simpl_dist, simpl_use[simpl_use$Descrip == "Sin vegetacion",])
sin_vegetacion <- st_transform(sin_vegetacion, crs = 6372)
sin_vegetacion$area_ha<- st_area(sin_vegetacion)/10000 #Area with Mexican CRS

veg_loss <- sin_vegetacion %>%
  as.data.frame() %>% 
  select(Descrip.1,COV_ID, area_ha) %>% 
  rename(vegetation_loss=area_ha
         
  )
## Other vegetation Mangrove change
otra_vegetacion <- st_intersection(simpl_dist, simpl_use[simpl_use$Descrip == "Otra vegetacion",])
otra_vegetacion <- st_transform(otra_vegetacion, crs = 6372)
otra_vegetacion$area_ha<- st_area(otra_vegetacion)/10000 #Area with Mexican CRS

other_veg <- otra_vegetacion %>%
  as.data.frame() %>% 
  select(Descrip.1,COV_ID, area_ha) %>% 
  rename(other_vegetation=area_ha
         
  )

## Other wetlands Mangrove change
humedal <- st_intersection(simpl_dist, simpl_use[simpl_use$Descrip == "Otros humedales",])
humedal <- st_transform(humedal, crs = 6372)
humedal$area_ha<- st_area(humedal)/10000 #Area with Mexican CRS

wetlands <- humedal %>%
  as.data.frame() %>% 
  select(Descrip.1,COV_ID, area_ha) %>% 
  rename(other_wetlands=area_ha
         
  )


## Disturbed Mangrove Change
perturbado <- st_intersection(simpl_dist, simpl_use[simpl_use$Descrip == "Manglar perturbado",])
perturbado <- st_transform(perturbado, crs = 6372)
perturbado$area_ha<- st_area(perturbado)/10000 #Area with Mexican CRS

disturbed <- perturbado %>% 
  as.data.frame() %>% 
  select(Descrip.1,COV_ID, area_ha) %>% 
  rename(disturbed=area_ha
         
  )

## Water Bodies Mangrove change
agua <- st_intersection(simpl_dist, simpl_use[simpl_use$Descrip == "Cuerpos de agua",])
agua <- st_transform(agua, crs = 6372)
agua$area_ha<- st_area(agua)/10000 #Area with Mexican CRS

water <- agua %>% 
  as.data.frame() %>% 
  select(Descrip.1,COV_ID, area_ha) %>% 
  rename(water=area_ha
         
  )



## Mangrove Distribution 2015 

dist2015$area2_ha <- st_area(dist2015)/10000
names(dist2015)
  
area2015 <- dist2015 %>% 
  as.data.frame() %>% 
  select(Descrip, COV_ID, area2_ha) %>% 
  rename(area2015_ha=area2_ha)


# Area change 2015-2020 ----

## Remove extra vectors
# rm(antropico)
# rm(agricola)
# rm(agua)
# rm(sin_vegetacion)
# rm(perturbado)
# rm(simpl_dist)
# rm(simpl_use)
# rm(dist2015)
# rm(use2020)


## Delete duplicate area values by COV_ID
mangrove <- mangrove %>% 
  group_by(COV_ID) %>% 
  mutate(area2020= sum(area2020)) %>% 
  distinct(COV_ID, .keep_all = TRUE)

antropic <- antropic %>% 
  group_by(COV_ID) %>% 
  mutate(antropic= sum(antropic)) %>% 
  distinct(COV_ID, .keep_all = TRUE)

agricultural <- agricultural %>% 
  group_by(COV_ID) %>% 
  mutate(agriculture= sum(agriculture)) %>% 
  distinct(COV_ID, .keep_all = TRUE)

disturbed <- disturbed %>% 
  group_by(COV_ID) %>% 
  mutate(disturbed= sum(disturbed)) %>% 
  distinct(COV_ID, .keep_all = TRUE)

water <- water %>% 
  group_by(COV_ID) %>% 
  mutate(water= sum(water)) %>% 
  distinct(COV_ID, .keep_all = TRUE)

veg_loss <- veg_loss %>% 
  group_by(COV_ID) %>% 
  mutate(vegetation_loss= sum(vegetation_loss)) %>% 
  distinct(COV_ID, .keep_all = TRUE)

other_veg <- other_veg %>% 
  group_by(COV_ID) %>% 
  mutate(other_vegetation= sum(other_vegetation)) %>% 
  distinct(COV_ID, .keep_all = TRUE)

wetlands <- wetlands %>% 
  group_by(COV_ID) %>% 
  mutate(other_wetlands= sum(other_wetlands)) %>% 
  distinct(COV_ID, .keep_all = TRUE)

## Merge areas dfs
mangroves_area <- merge(area2015, mangrove[, c("COV_ID", "area2020")],  by = "COV_ID", all=T) 

mangroves_area <- merge(mangroves_area, antropic[, c("COV_ID", "antropic")],  by = "COV_ID", all=T) 
  
mangroves_area <- merge(mangroves_area,  agricultural[, c("COV_ID", "agriculture")], by="COV_ID", all=T)

mangroves_area <- merge(mangroves_area,  disturbed[, c("COV_ID", "disturbed")], by="COV_ID", all=T)

mangroves_area <- merge(mangroves_area,  water[, c("COV_ID", "water")], by="COV_ID", all=T)

mangroves_area <- merge(mangroves_area,  veg_loss[, c("COV_ID", "vegetation_loss")], by="COV_ID", all=T)

mangroves_area <- merge(mangroves_area,  other_veg[, c("COV_ID", "other_vegetation")], by="COV_ID", all=T)

mangroves_area <- merge(mangroves_area,  wetlands[, c("COV_ID", "other_wetlands")], by="COV_ID", all=T)

## Percentages 

area_loss <- mangroves_area %>% 
            replace(is.na(.), 0) %>% 
            mutate(area_loss=antropic+agriculture+disturbed+water+vegetation_loss+other_vegetation+other_wetlands,
                  percentage= area_loss* (100/area2015_ha))

## Bar plot

 
sum(area_loss$area2015_ha) # 775555.7 ha 2015
sum(area_loss$area2020) # 737831.4 ha 2020
sum(area_loss$antropic) # 1203.431 ha anthropic
sum(area_loss$agriculture) #662.8878 ha agriculture
sum(area_loss$water) # 6204.427 water bodies
sum(area_loss$vegetation_loss) # 197.7074 ha vegetation loss
sum(area_loss$disturbed) # 4246.749 ha disturbed
sum(area_loss$other_vegetation) # 6015.239 Other Vegetation
sum(area_loss$other_wetlands) # 19193.76 Other Wetlands

year <- c(rep("2015", 1), rep("2020", 8))
class <-  c(rep("Mangrove", 2), "Anthropic", "Agriculture and Livestock", "Water Bodies", "Vegetation Loss", "Disturbed", "Other Vegetation", "Other Wetlands")
area_ha <-  c(775555.7, 737831.4,1203.431, 662.8878, 6204.427, 197.7074, 4246.749, 6015.239, 19193.76)
area_plot <- data.frame(year, class, area_ha)

library(viridisLite)
library(hrbrthemes)

area_plot %>% 
  mutate(class=factor(c("Mangrove", "Mangrove", "Other Wetlands","Water Bodies", "Other Vegetation","Disturbed", "Anthropic",  "Agriculture and Livestock",  "Vegetation Loss"),
                        levels=c(   "Vegetation Loss", "Agriculture and Livestock","Anthropic","Disturbed", "Other Vegetation","Water Bodies","Other Wetlands","Mangrove"   ))) %>% 
  ggplot(aes(fill=class, y=area_ha, x=year))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_viridis(discrete= T)+
  theme_ipsum()+
  ggtitle("Mexican Mangrove Cover Loss")+
  labs(x="Year", y= "Area (%)", subtitle = "2015-2020")+
  theme(axis.title.x = element_text(face="bold", hjust=0.5, size=18),
        axis.title.y = element_text(face="bold", hjust=0.5, size=18)
    
  )



# agua <-st_collection_extract(agua, "POLYGON")
# perturbado <-st_collection_extract(perturbado, "POLYGON")
# antropico <- st_collection_extract(antropico, "POLYGON")
# agricola <-st_collection_extract(agricola, "POLYGON")
# sin_vegetacion <-st_collection_extract(sin_vegetacion, "POLYGON")
# 
# st_write(antropico, "data/output/intersection_2015-2020/antropico.shp")
# st_write(agua, "data/output/intersection_2015-2020/agua.shp")
# st_write(perturbado, "data/output/intersection_2015-2020/perturbado.shp")
# st_write(sin_vegetacion, "data/output/intersection_2015-2020/sin_vegetacion.shp")
# st_write(agricola, "data/output/intersection_2015-2020/agricola.shp")

