## global IPF data

rawpath = "ebd_aspfly1_relNov-2025.txt"

require(lubridate)
require(tidyverse)

# select only necessary columns
preimp = c("COMMON.NAME","COUNTRY",
           "STATE","COUNTY",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE",
           "SAMPLING.EVENT.IDENTIFIER","SPECIES.COMMENTS","GROUP.IDENTIFIER")

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
  filter(COUNTRY %in% c("Bangladesh","India","Pakistan","Nepal","Sri Lanka")) %>%
  select(-COUNTRY)

data0 = data

data$source = "eBird"
data$type = "Comments"

data_white = data %>%
  filter(grepl('\\bwhite',SPECIES.COMMENTS, ignore.case = TRUE) | 
           grepl('\\bwhit',SPECIES.COMMENTS, ignore.case = TRUE) | 
             grepl('\\bw\\b',SPECIES.COMMENTS, ignore.case = TRUE))

data_white = data_white %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
  group_by(COMMON.NAME,group.id) %>% slice(1) %>% ungroup %>%
  mutate(morph = "W")

data_white = data_white %>%
  filter(!grepl('transition',SPECIES.COMMENTS, ignore.case = TRUE))


data_rufous = data %>%
  filter(grepl('\\brufous',SPECIES.COMMENTS, ignore.case = TRUE) |
           grepl('\\brufus',SPECIES.COMMENTS, ignore.case = TRUE)  | 
           grepl('\\bcinnamon',SPECIES.COMMENTS, ignore.case = TRUE)  | 
           grepl('\\bred',SPECIES.COMMENTS, ignore.case = TRUE)  | 
           grepl('\\bbrown',SPECIES.COMMENTS, ignore.case = TRUE)  | 
           grepl('\\br\\b',SPECIES.COMMENTS, ignore.case = TRUE))

data_rufous = data_rufous %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
  group_by(COMMON.NAME,group.id) %>% slice(1) %>% ungroup %>%
  mutate(morph = "R")

data_rufous = data_rufous %>%
  filter(!grepl('transition',SPECIES.COMMENTS, ignore.case = TRUE))

data_intermediate = data %>%
  filter(grepl('\\bintermediate',SPECIES.COMMENTS, ignore.case = TRUE)  | 
           grepl('\\bmixed',SPECIES.COMMENTS, ignore.case = TRUE)  | 
           grepl('\\bmix',SPECIES.COMMENTS, ignore.case = TRUE)  | 
           grepl('\\baberrant',SPECIES.COMMENTS, ignore.case = TRUE)  | 
           grepl('\\btransition',SPECIES.COMMENTS, ignore.case = TRUE))

data_intermediate = data_intermediate %>%
  filter(!grepl('\\bmixed feeding\\b',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bmixed foraging\\b',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bmixed species\\b',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bmixed flock\\b',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bmixed hunting\\b',SPECIES.COMMENTS, ignore.case = TRUE))

data_intermediate = data_intermediate %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
  group_by(COMMON.NAME,group.id) %>% slice(1) %>% ungroup %>%
  mutate(morph = "I")


### load eBird photo data and remove duplicates from comment data

data = data %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
  group_by(COMMON.NAME,SAMPLING.EVENT.IDENTIFIER) %>% slice(1) %>% ungroup
data_groups = data %>% distinct(SAMPLING.EVENT.IDENTIFIER,group.id)

data_eBird_photos = read.csv("ipf_eBird_2025.csv")

photo_to_rem_duplicates = data_eBird_photos %>%
  left_join(data_groups)

# isolate morphs to remove duplicates

photo_to_rem_duplicates_R = photo_to_rem_duplicates %>%
  filter(morph == "R")
data_rufous = data_rufous %>%
  filter(!group.id %in% photo_to_rem_duplicates_R$group.id)

photo_to_rem_duplicates_W = photo_to_rem_duplicates %>%
  filter(morph == "W")
data_white = data_white %>%
  filter(!group.id %in% photo_to_rem_duplicates_W$group.id)

photo_to_rem_duplicates_I = photo_to_rem_duplicates %>%
  filter(morph == "I")
data_intermediate = data_intermediate %>%
  filter(!group.id %in% photo_to_rem_duplicates_I$group.id)

## bringing together comment data

data_eBird_comments = data_white %>%
  bind_rows(data_rufous) %>%
  bind_rows(data_intermediate) %>%
  dplyr::select(source,type,group.id,SAMPLING.EVENT.IDENTIFIER,day,month,year,morph,SPECIES.COMMENTS) %>%
  mutate(rating = NA, aberration = NA)

## preparing photo data

sample_date = data %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
  distinct(SAMPLING.EVENT.IDENTIFIER,group.id,OBSERVATION.DATE)

sample_latlong = data %>%
  distinct(SAMPLING.EVENT.IDENTIFIER,LATITUDE,LONGITUDE)

data_eBird_photos = data_eBird_photos %>%
  left_join(sample_date) %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         year = year(OBSERVATION.DATE),
         month = month(OBSERVATION.DATE),
         day = day(OBSERVATION.DATE)) %>%
  mutate(SPECIES.COMMENTS = NA) %>%
  dplyr::select(source,type,group.id,SAMPLING.EVENT.IDENTIFIER,
                day,month,year,morph,SPECIES.COMMENTS,rating,aberration)
  
data_eBird = data_eBird_photos %>%
  bind_rows(data_eBird_comments) %>%
  left_join(sample_latlong)

write.csv(data_eBird,"data_eBird.csv",row.names = F)