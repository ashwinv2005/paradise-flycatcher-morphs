library(tidyverse)

## load maps

library(sf)
load("maps_sf.Rdata")

him_map = st_read("himalaya/outline.shp") %>%
  st_make_valid()

sf_use_s2(FALSE)

pak_map = st_read("world-administrative-boundaries-countries.geojson") %>%
  st_make_valid() %>%
  dplyr::select(english_short) %>%
  filter(english_short %in% c("Pakistan")) %>%
  st_intersection(him_map) %>%
  st_difference(india_sf) %>%
  dplyr::select(english_short)

sf_use_s2(TRUE)

pak_map = pak_map %>%
  st_cast("MULTIPOLYGON", warn = FALSE) |>
  st_cast("POLYGON") %>%
  mutate(english_short = "Pakistan Himalaya") %>%
  rename(STATE.NAME = english_short, STATE.GEOM = geometry) %>%
  mutate(COUNTRY = "Pakistan") %>%
  relocate(STATE.NAME, .after = last_col())

pak_map = st_crop(
  pak_map,
  xmin = 71.38,
  xmax = 180,      # eastern edge of world
  ymin = -90,
  ymax = 90
)

pak_map = pak_map[1,]

sf_use_s2(FALSE)

uzb_map = st_read("world-administrative-boundaries-countries.geojson") %>%
  st_make_valid() %>%
  dplyr::select(english_short) %>%
  filter(english_short %in% c("Uzbekistan")) %>%
  dplyr::select(english_short)

sf_use_s2(TRUE)

uzb_map = uzb_map %>%
  mutate(english_short = "Uzbekistan Himalaya") %>%
  rename(STATE.NAME = english_short, STATE.GEOM = geometry) %>%
  mutate(COUNTRY = "Uzbekistan") %>%
  relocate(STATE.NAME, .after = last_col())

uzb_map = st_crop(
  uzb_map,
  xmin = 65.89,
  xmax = 180,      # eastern edge of world
  ymin = -90,
  ymax = 90
)

worldmap = st_read("world-administrative-boundaries-countries.geojson") %>%
  dplyr::select(english_short) %>%
  filter(english_short %in% c("Nepal","Bangladesh","Sri Lanka",
                              "Tajikistan","Kazakhstan")) %>%
  rename(STATE.NAME = english_short, STATE.GEOM = geometry) %>%
  mutate(COUNTRY = STATE.NAME) %>%
  relocate(STATE.NAME, .after = last_col())

ka_dists = c("Kodagu","Hassan","Mysuru","Mandya","Chamarajanagara",
                "Ramanagara","Bangalore","Bengaluru Rural","Kolar",
                "Tumakuru","Chikkaballapura")

ap_dists = c("Anantapuramu","Chittoor",
                "YSR Kadapa")

ka_sf = dists_sf %>%
  filter(STATE.NAME == "Karnataka") %>%
  mutate(STATE.NAME = if_else(DISTRICT.NAME %in% ka_dists, 
                              "Southern Karnataka",STATE.NAME)) %>%
  group_by(STATE.NAME) %>% 
  reframe(STATE.GEOM = st_union(DISTRICT.GEOM)) %>% 
  st_as_sf() %>%
  mutate(COUNTRY = "India") %>%
  relocate(STATE.NAME, .after = last_col())
  

ap_sf = dists_sf %>%
  filter(STATE.NAME == "Andhra Pradesh") %>%
  mutate(STATE.NAME = if_else(DISTRICT.NAME %in% ap_dists, 
                              "Southern Andhra Pradesh",STATE.NAME)) %>%
  group_by(STATE.NAME) %>% 
  reframe(STATE.GEOM = st_union(DISTRICT.GEOM)) %>% 
  st_as_sf() %>%
  mutate(COUNTRY = "India") %>%
  relocate(STATE.NAME, .after = last_col())


map_to_use_sf = states_sf %>% dplyr::select(-AREA) %>%
  filter(!STATE.NAME %in% c("Karnataka","Andhra Pradesh")) %>%
  mutate(COUNTRY = "India") %>%
  relocate(STATE.NAME, .after = last_col()) %>%
  bind_rows(worldmap) %>%
  bind_rows(pak_map) %>%
  bind_rows(uzb_map) %>%
  bind_rows(ka_sf) %>%
  bind_rows(ap_sf)

map_to_use_sf = map_to_use_sf %>%
  mutate(REGION = case_when(
    STATE.NAME %in% c("Pakistan Himalaya","Uzbekistan Himalaya",
                      "Kazakhstan","Tajikistan",
                      "Jammu and Kashmir","Ladakh",
                      "Himachal Pradesh","Uttarakhand","Nepal") ~ "Himalaya", 
    STATE.NAME %in% c("Punjab","Haryana","Rajasthan","Delhi",
                "Uttar Pradesh","Chandigarh") ~ "North",
    STATE.NAME %in% c("Jharkhand","West Bengal",
                "Bangladesh","Tripura","Bihar") ~ "East",
    STATE.NAME %in% c("Chhattisgarh","Odisha","Gujarat","Madhya Pradesh","Maharashtra","Goa",
                "Telangana","Karnataka","Andhra Pradesh",
                "Dadra and Nagar Haveli","Daman and Diu") ~ "Central",
    STATE.NAME %in% c("Southern Karnataka","Southern Andhra Pradesh","Kerala",
                "Tamil Nadu","Puducherry") ~ "South",
    STATE.NAME %in% c("Sri Lanka") ~ "Sri Lanka",
    TRUE ~ NA
  )) %>%
  st_make_valid()


## load IPF data

data_eBird = read.csv("data_eBird_curated.csv")
data_iNat = read.csv("data_iNaturalist.csv")
data_old = read.csv("data_all_2019.csv")

data_ipf = data_old %>%
  bind_rows(data_eBird) %>%
  bind_rows(data_iNat)

data_ipf = st_as_sf(data_ipf,
                  coords = c("LONGITUDE", "LATITUDE"),
                  crs = 4326,
                  remove = FALSE)

data_ipf = st_transform(data_ipf, st_crs(map_to_use_sf)) %>%
  st_make_valid()

sf_use_s2(FALSE)
data_ipf = data_ipf %>% st_join(map_to_use_sf)
sf_use_s2(TRUE)

empty_pak = read.csv("empty_pak.csv")

match_rows = data_ipf %>% 
  mutate(row_id = row_number()) %>% 
  semi_join(empty_pak, by = c("LATITUDE", "LONGITUDE")) %>% 
  pull(row_id)

# Update ONLY those rows
data_ipf[match_rows, "STATE.NAME"] = "Pakistan Himalaya"
data_ipf[match_rows, "COUNTRY"]    = "Pakistan"
data_ipf[match_rows, "REGION"]     = "Himalaya"

empty_him = read.csv("empty_him.csv")

match_rows = data_ipf %>% 
  mutate(row_id = row_number()) %>% 
  semi_join(empty_him, by = c("LATITUDE", "LONGITUDE")) %>% 
  pull(row_id)

# Update ONLY those rows
data_ipf[match_rows, "REGION"] = "Himalaya"

empty_nor = read.csv("empty_nor.csv")

match_rows = data_ipf %>% 
  mutate(row_id = row_number()) %>% 
  semi_join(empty_nor, by = c("LATITUDE", "LONGITUDE")) %>% 
  pull(row_id)

# Update ONLY those rows
data_ipf[match_rows, "REGION"] = "North"

empty_eas = read.csv("empty_eas.csv")

match_rows = data_ipf %>% 
  mutate(row_id = row_number()) %>% 
  semi_join(empty_eas, by = c("LATITUDE", "LONGITUDE")) %>% 
  pull(row_id)

# Update ONLY those rows
data_ipf[match_rows, "REGION"] = "East"

empty_cen = read.csv("empty_cen.csv")

match_rows = data_ipf %>% 
  mutate(row_id = row_number()) %>% 
  semi_join(empty_cen, by = c("LATITUDE", "LONGITUDE")) %>% 
  pull(row_id)

# Update ONLY those rows
data_ipf[match_rows, "REGION"]  = "Central"

empty_sou = read.csv("empty_sou.csv")

match_rows = data_ipf %>% 
  mutate(row_id = row_number()) %>% 
  semi_join(empty_sou, by = c("LATITUDE", "LONGITUDE")) %>% 
  pull(row_id)

# Update ONLY those rows
data_ipf[match_rows, "REGION"] = "South"

empty_sri = read.csv("empty_sri.csv")

match_rows = data_ipf %>% 
  mutate(row_id = row_number()) %>% 
  semi_join(empty_sri, by = c("LATITUDE", "LONGITUDE")) %>% 
  pull(row_id)

# Update ONLY those rows
data_ipf[match_rows, "REGION"] = "Sri Lanka"

data_ipf = data_ipf %>%
  mutate(fort = if_else(day <= 15,1,2))

map_to_use_sf = map_to_use_sf %>% filter(!COUNTRY %in% c("Kazakhstan"))

## global IPF data

rawpath = "ebd_aspfly1_relNov-2025.txt"

# select only necessary columns
preimp = c("LATITUDE","LONGITUDE","OBSERVATION.DATE",
           "SAMPLING.EVENT.IDENTIFIER","GROUP.IDENTIFIER")

nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

data = data %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         year = year(OBSERVATION.DATE),
         month = month(OBSERVATION.DATE),
         day = day(OBSERVATION.DATE)) %>%
  mutate(fort = if_else(day <= 15,1,2)) %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
  group_by(group.id) %>% slice(1) %>% ungroup %>%
  dplyr::select(-SAMPLING.EVENT.IDENTIFIER,-GROUP.IDENTIFIER)


data = st_as_sf(data,
                    coords = c("LONGITUDE", "LATITUDE"),
                    crs = 4326,
                    remove = FALSE)

data = st_transform(data, st_crs(map_to_use_sf)) %>%
  st_make_valid()

sf_use_s2(FALSE)
data = data %>% st_join(map_to_use_sf) %>%
  filter(!is.na(REGION))
sf_use_s2(TRUE)

map_proj = st_transform(map_to_use_sf, 3857) %>%
  st_make_valid()

grid25 = st_make_grid(
  map_proj,
  cellsize = 25000,     # 25 km = 25,000 m
  square = TRUE,
  what = "polygons"
) %>%
  st_make_valid()

# keep only grid over your area
grid25 = st_intersection(grid25, map_proj)
grid25 = st_as_sf(grid25) |>
  dplyr::mutate(id = row_number())

data = st_transform(data, st_crs(grid25)) %>%
  st_make_valid()

sf_use_s2(FALSE)
data_range = data %>% st_join(grid25) %>%
  filter(!is.na(id)) %>% distinct(id) %>% pull()
sf_use_s2(TRUE)

## sampling data from within IPF range

rawpath = "ebd_IN_smp_relNov-2025_sampling.txt"
rawpath1 = "ebd_PK_smp_relNov-2025_sampling.txt"
rawpath2 = "ebd_TJ_smp_relNov-2025_sampling.txt"
rawpath3 = "ebd_BD_smp_relNov-2025_sampling.txt"
rawpath4 = "ebd_NP_smp_relNov-2025_sampling.txt"
rawpath5 = "ebd_KZ_smp_relNov-2025_sampling.txt"
rawpath6 = "ebd_UZ_smp_relNov-2025_sampling.txt"
rawpath7 = "ebd_LK_smp_relNov-2025_sampling.txt"


# select only necessary columns
preimp = c("LATITUDE","LONGITUDE","OBSERVATION.DATE",
           "SAMPLING.EVENT.IDENTIFIER","GROUP.IDENTIFIER",
           "ALL.SPECIES.REPORTED")

nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data_samp = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))
data_samp1 = read.delim(rawpath1, colClasses = nms, sep = "\t", header = T, quote = "", 
                       stringsAsFactors = F, na.strings = c(""," ",NA))
data_samp2 = read.delim(rawpath2, colClasses = nms, sep = "\t", header = T, quote = "", 
                       stringsAsFactors = F, na.strings = c(""," ",NA))
data_samp3 = read.delim(rawpath3, colClasses = nms, sep = "\t", header = T, quote = "", 
                       stringsAsFactors = F, na.strings = c(""," ",NA))
data_samp4 = read.delim(rawpath4, colClasses = nms, sep = "\t", header = T, quote = "", 
                       stringsAsFactors = F, na.strings = c(""," ",NA))
data_samp5 = read.delim(rawpath5, colClasses = nms, sep = "\t", header = T, quote = "", 
                       stringsAsFactors = F, na.strings = c(""," ",NA))
data_samp6 = read.delim(rawpath6, colClasses = nms, sep = "\t", header = T, quote = "", 
                       stringsAsFactors = F, na.strings = c(""," ",NA))
data_samp7 = read.delim(rawpath7, colClasses = nms, sep = "\t", header = T, quote = "", 
                       stringsAsFactors = F, na.strings = c(""," ",NA))

data_samp = data_samp %>%
  bind_rows(data_samp1) %>%
  bind_rows(data_samp2) %>%
  bind_rows(data_samp3) %>%
  bind_rows(data_samp4) %>%
  bind_rows(data_samp5) %>%
  bind_rows(data_samp6) %>%
  bind_rows(data_samp7) %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         year = year(OBSERVATION.DATE),
         month = month(OBSERVATION.DATE),
         day = day(OBSERVATION.DATE)) %>%
  mutate(fort = if_else(day <= 15,1,2)) %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
  group_by(group.id) %>% slice(1) %>% ungroup %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  dplyr::select(-SAMPLING.EVENT.IDENTIFIER,-GROUP.IDENTIFIER,-ALL.SPECIES.REPORTED)

data_samp = st_as_sf(data_samp,
                coords = c("LONGITUDE", "LATITUDE"),
                crs = 4326,
                remove = FALSE)

data_samp = st_transform(data_samp, st_crs(grid25)) %>%
  st_make_valid()

sf_use_s2(FALSE)
grid25 = grid25 %>% dplyr::select(id)
data_samp = data_samp %>% st_join(grid25) %>%
  filter(!is.na(id))
sf_use_s2(TRUE)

data_samp = data_samp %>% filter(id %in% data_range)

data_samp = st_transform(data_samp, st_crs(map_to_use_sf)) %>%
  st_make_valid()

sf_use_s2(FALSE)
data_samp = data_samp %>% st_join(map_to_use_sf) %>%
  filter(!is.na(REGION))
sf_use_s2(TRUE)

data_ipf = data_ipf %>% st_drop_geometry()
data = data %>% st_drop_geometry()
data_samp = data_samp %>% st_drop_geometry() %>%
  dplyr::select(-id)

write.csv(data_ipf,"data_morphs.csv",row.names = F)
write.csv(data,"data_ipf_eBird.csv",row.names = F)
write.csv(data_samp,"data_sampling_eBird.csv",row.names = F)

save(map_to_use_sf,file = "base_map.RData")