require(lubridate)
require(tidyverse)

data_iNat_photos = read.csv("ipf_iNat_2025.csv")

data_iNat_photos = data_iNat_photos %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE, format = "%b %d, %Y"), 
         year = year(OBSERVATION.DATE),
         month = month(OBSERVATION.DATE),
         day = day(OBSERVATION.DATE)) %>%
  dplyr::select(source,type,group.id,SAMPLING.EVENT.IDENTIFIER,
                day,month,year,morph,SPECIES.COMMENTS,rating,aberration,LATITUDE,LONGITUDE)

write.csv(data_iNat_photos,"data_iNaturalist.csv",row.names = F)