require(tidyverse)
require(raster)
require(ggsci)
require(rgdal)
library(rmapshaper)
library(sf)
library(rgeos)

slmap = readOGR("LKA_adm","LKA_adm0")
bdmap = readOGR("BGD_adm","BGD_adm0")
pamap = readOGR("PAK_adm","PAK_adm0")
npmap = readOGR("NPL_adm","NPL_adm0")
indiamap = readOGR("India","India_2011")
statemap = readOGR("India States","IndiaStates_2011")
districtmap = readOGR("India Districts","IndiaDistricts_2011")

districtmap$ST_NM = as.character(districtmap$ST_NM)
districtmap$ST_NM[339] = "Ladakh"
#districtmap$ST_NM[districtmap$DISTRICT == "Leh (Ladakh)"] = "Ladakh"
districtmap$DISTRICT = paste(districtmap$ST_NM,districtmap$DISTRICT,sep = "-")

t = districtmap[districtmap@data$ST_NM %in% c("Ladakh"),]
t@data = t@data %>% dplyr::select(2)
statemap = statemap - districtmap[districtmap@data$ST_NM %in% c("Ladakh"),]
statemap = rbind(statemap,t)

gujarat = districtmap[districtmap$ST_NM %in% c("Gujarat"),]
gj = st_as_sf(gujarat)
gj = st_union(gj)
gj = as_Spatial(gj)
gj = gBuffer(gj, byid=TRUE, width=0)
gj = ms_filter_islands(gj, min_area = 12391399)

statemap = statemap[!statemap$ST_NM %in% c("Gujarat"),]


crs(slmap) = NA
crs(bdmap) = NA
crs(pamap) = NA
crs(npmap) = NA


fullmap = slmap + statemap
fullmap@data$ST_NM = as.character(fullmap@data$ST_NM)
fullmap@data$ST_NM[1] = "Sri Lanka"

bdmap = bdmap - indiamap
pamap = pamap - indiamap
pamap = ms_filter_islands(pamap, min_area = 12391399903)
npmap = npmap - indiamap

othersmap = bdmap + pamap + npmap + gj

fullmap = othersmap + fullmap

fullmap@data$ST_NM[1:4] = c("Nepal","Pakistan","Bangladesh","Gujarat")

fullmapcountries = npmap + bdmap + pamap + slmap + gBuffer(indiamap, byid=TRUE, width=0)
fullmapcountries@data$Country = c("Nepal","Pakistan","Bangladesh","Sri Lanka","India",
                                  "Nepal","Pakistan","Bangladesh")

#fullmap = ms_filter_islands(fullmap, min_area = 12391399903)
#fullmapcountries = ms_filter_islands(fullmap, min_area = 12391399903)



save(fullmap,fullmapcountries,file = "south_asia_map.RData")
