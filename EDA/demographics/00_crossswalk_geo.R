rm(list=ls())
library(tidyverse)
library(sf)

cPUMA <- read_sf("./data/ipums_puma_2010/ipums_puma_2010.dbf") %>%
    filter(!(STATEFIP %in% c("72", "02", "15"))) %>%
    st_set_precision(1e5)
migPUMA <- read_sf(
    "./data/ipums_migpuma_pwpuma_2010/ipums_migpuma_pwpuma_2010.dbf") %>%
    filter(!(MIGPLAC %in% c("072", "002", "015"))) %>%
    st_set_precision(1e5)

test <- st_intersection(cPUMA, migPUMA)
savecp <- which(sapply(test[1:nrow(cPUMA)], function(v) 1 %in% v))
validShapes <- sapply(
    lapply(test$geometry, class), 
    function(v) ("POLYGON" %in% v) | ("MULTIPOLYGON" %in% v))

cwPUMA <- test[validShapes,]

# Sanity check to make sure we found a match everywhere
nrow(cwPUMA) == nrow(cPUMA)
all(cPUMA$PUMA %in% cwPUMA$PUMA)

cwPUMA %>%
    as_tibble() %>%
    select(PUMA, MIGPUMA, STATEFIP) %>%
    saveRDS("./data/puma2migpuma.RDS")
