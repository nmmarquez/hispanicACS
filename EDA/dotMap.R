rm(list=ls())
library(tidycensus)
library(acs)
library(jsonlite)
library(tidyverse)
library(stringr)
library(DT)
library(sf)
library(plotly)
library(tigris)
library(leaflet)
library(mapview)
library(leafgl)

# Read in the API key so we can access the census data  
apiKey <- read_json("keys/acs.json")
census_api_key(apiKey$api_key)

# We want to grab all the variables that relate to pop counts that distinguish
# between latino and non-latino populations and group by to reduce the number
# of racial groups to just 4.
varsDF <- load_variables(2017, "acs5") %>%
    filter(concept == "HISPANIC OR LATINO ORIGIN BY RACE") %>%
    select(-concept) %>%
    rename(variable = name) %>%
    .[c(1, 3:12),] %>%
    mutate(race = case_when(
        variable == "B03002_001" ~ "Total",
        grepl("White", label) ~ "White",
        grepl("Black", label) ~ "Black",
        grepl("Indian|Asian|Pacific", label) ~ "Asian",
        variable == "B03002_012" ~ "Hispanic",
        TRUE ~ "Other"))

# Use the tidycensus to pull in their latest available 5 year ACS since it has 
# the most detailed geographies outside of census years
popAcsDF <- get_acs(
    state = "CA", county = c("Orange", "Los Angeles"), 
    geography="block group", # I want county level data
    variables=varsDF$variable, # iwant the variables from this list
    year=2017, # from the 2014 acs
    geometry=TRUE) %>%
    left_join(select(varsDF, variable, race))

# these counts aint great but it aint terrible either enough for a demo for sure
(scaleDF <- popAcsDF %>%
    as_tibble() %>%
    mutate(Total = race == "Total") %>% 
    group_by(GEOID, Total) %>%
    summarize(N=sum(estimate)) %>%
    summarize(error=diff(N), Nest=nth(N, 2), weight=N[2]/N[1]) %>%
    mutate(perr=abs(error)/Nest) %>%
    filter(is.finite(perr)) %>%
    arrange(-perr))

# remove the total count and adjust for the scaling factor since we tend to be
# over-estimating populations. We can also remove populations that are zero
# or NAN which would have happened if the total count was 0
polyDF <- popAcsDF %>%
    filter(race != "Total") %>%
    group_by(race, GEOID) %>%
    summarize(N=sum(estimate)) %>%
    left_join(scaleDF, by = "GEOID") %>%
    mutate(N=round(weight*N)) %>%
    select(-error, -Nest, -weight, -perr) %>%
    filter(N != 0 & !is.na(N))

# For each block group race we want to sample some points. This takes a long 
# time so lets do it once and be done with it.
if(!file.exists("data/pointDF.Rds")){
    pointDF <- st_sample(polyDF, polyDF$N)
    saveRDS(pointDF, "data/pointDF.Rds")
}

pointDF <- readRDS("data/pointDF.Rds")
sfDF <- st_sf(
    tibble(race = unlist(lapply(1:nrow(polyDF), function(i){
        rep(polyDF$race[i], polyDF$N[i])}))), 
    geometry = pointDF) %>%
    mutate(clrs=case_when(
        race == "White" ~ "Red",
        race == "Black" ~ "Blue",
        race == "Asian" ~ "Purple",
        race == "Hispanic" ~ "Green",
        TRUE ~ "Yellow"
    ))

minisfDF <- sample_frac(sfDF, .1)
colMat <- t(col2rgb(minisfDF$clrs))/255

#options(viewer = NULL) # view in browser

m <- leaflet() %>%
    addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%
    addGlPoints(
        data = minisfDF, group = "pts", opacity = .3,
        weight = 3, color = colMat) %>%
    setView(lng = -118.2, lat = 34.0, zoom = 8)

mapshot(m, "data/dotmap.html", selfcontained = FALSE)
