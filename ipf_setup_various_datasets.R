## global IPF data

rawpath = "ebd_aspfly1_relJun-2025.txt"

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
         daym = day(OBSERVATION.DATE)) %>%
  filter(COUNTRY %in% c("Bangladesh","India","Pakistan","Nepal","Sri Lanka")) %>%
  select(-COUNTRY)

data0 = data

data$source = "eBird comments"

data_white = data %>%
  filter(grepl('\\bwhite',SPECIES.COMMENTS, ignore.case = TRUE) | 
             grepl('\\bw\\b',SPECIES.COMMENTS, ignore.case = TRUE))
data_white = data_white %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
  group_by(COMMON.NAME,group.id) %>% slice(1) %>%
  mutate(morph = "W")


data_rufous = data %>%
  filter(grepl('\\brufous',SPECIES.COMMENTS, ignore.case = TRUE) |
           grepl('\\brufus',SPECIES.COMMENTS, ignore.case = TRUE)  | 
           grepl('\\bred',SPECIES.COMMENTS, ignore.case = TRUE)  | 
           grepl('\\bbrown',SPECIES.COMMENTS, ignore.case = TRUE)  | 
           grepl('\\br\\b',SPECIES.COMMENTS, ignore.case = TRUE))
data_rufous = data_rufous %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
  group_by(COMMON.NAME,group.id) %>% slice(1) %>%
  mutate(morph = "R")

data_intermediate = data %>%
  filter(grepl('\\bintermediate',SPECIES.COMMENTS, ignore.case = TRUE)  | 
           grepl('\\bmixed',SPECIES.COMMENTS, ignore.case = TRUE)  | 
           grepl('\\baberrant',SPECIES.COMMENTS, ignore.case = TRUE)  | 
           grepl('\\btransition',SPECIES.COMMENTS, ignore.case = TRUE))

data_intermediate = data_intermediate %>%
  filter(!grepl('\\bmixed feeding\\b',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bmixed species\\b',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bmixed flock\\b',SPECIES.COMMENTS, ignore.case = TRUE) &
           !grepl('\\bmixed hunting\\b',SPECIES.COMMENTS, ignore.case = TRUE))

data_intermediate = data_intermediate %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
  group_by(COMMON.NAME,group.id) %>% slice(1) %>%
  mutate(morph = "I")


### load eBird photo data and remove duplicates from comment data

data = data %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
  group_by(COMMON.NAME,group.id) %>% slice(1) %>% ungroup
data_groups = data %>% distinct(SAMPLING.EVENT.IDENTIFIER,group.id)

eBird_photo_data = read.csv("ipf_data_jun2025_ebird.csv")

photo_to_rem_duplicates = eBird_photo_data %>%
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

