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

apiKey <- read_json("~/.keys.json")
census_api_key(apiKey$acs)

ageVarsA1 <- paste0("B01001_0", sprintf("%02d", c(3:25, 27:49)))
ageVarsD1 <- paste0("P01100", sprintf("%02d", 1:31))

extractMidpoint <- function(x){
    mps <- lapply(str_extract_all(x, "\\d+"), as.numeric)
    mps <- lapply(1:length(mps), function(i){
        z <- mps[[i]]
        if(length(z) == 1 & grepl("under|<", tolower(x[i]))){
            z <- 0:4
        }
        min(z):max(z)
    })
    mps
}

acsAgeDF <- load_variables(2016, "acs5") %>%
    filter(name %in% ageVarsA1) %>%
    mutate(ages=extractMidpoint(label)) %>%
    mutate(ages=lapply(ages, function(z){
        if(length(z) == 2){
            if(all(z == 65:66)){
                z <- 65:69
            }
        }
        if(length(z) == 3){
            if(all(z == 67:69)){
                z <- 65:69
            }
        }
        z
    })) %>%
    mutate(group=as.numeric(as.factor(sapply(ages, min)))) %>%
    rename(variable=name)

acsGroupsDF <- acsAgeDF %>%
    select(ages, group) %>%
    unique %>%
    mutate(ageMean=sapply(ages, function(x){
        mean(c(x, last(x)+1))
    }))

censusAgeDF <- load_variables(1990, "sf1") %>%
    filter(name %in% ageVarsD1) %>%
    mutate(ages=extractMidpoint(label)) %>%
    mutate(group=sapply(ages, function(a){
        which(sapply(acsGroupsDF$ages, function(z){
            all(a %in% z)
        }))
    })) %>%
    select(-ages) %>%
    left_join(acsGroupsDF, by="group") %>%
    rename(variable=name)
    

# downolad data the capture output stuff isnt neccesary its just to get rid 
# of the output
ageCensusDF <- get_decennial(
    geography="county", # I want county level data
    variables=ageVarsD1, # iwant the variables from this list
    year=1990, # from the 2014 acs
    geometry=FALSE) %>%
    left_join(select(censusAgeDF, -ages, -label, -concept), by="variable") %>%
    group_by(GEOID, NAME, group, ageMean) %>%
    summarize(estimate=sum(value)) %>%
    ungroup

ageAcsDF <- get_acs(
    geography="county", # I want county level data
    variables=ageVarsA1, # iwant the variables from this list
    year=2016, # from the 2014 acs
    geometry=TRUE) %>%
    left_join(select(acsAgeDF, -ages, -label, -concept), by="variable") %>%
    left_join(select(acsGroupsDF, -ages), by="group") %>%
    group_by(GEOID, NAME, group, ageMean) %>%
    summarize(estimate=sum(estimate)) %>%
    ungroup

geometryDF <- ageAcsDF %>%
    select(geometry, GEOID, NAME) %>%
    unique

diffNA <- function(x){
    z <- NA
    if(length(x) == 2){
        z <- diff(x)
    }
    z
}

agingDF <- left_join(geometryDF, rbind(
    ageAcsDF %>%
        as_tibble() %>%
        select(-geometry) %>%
        mutate(ageWeight=ageMean*estimate) %>%
        group_by(GEOID, NAME) %>%
        summarize(mAge=sum(ageWeight)/sum(estimate)) %>%
        ungroup() %>%
        mutate(year=2014),
    ageCensusDF %>%
        mutate(ageWeight=ageMean*estimate) %>%
        group_by(GEOID, NAME) %>%
        summarize(mAge=sum(ageWeight)/sum(estimate)) %>%
        ungroup() %>%
        mutate(year=1990)) %>%
    arrange(GEOID, year) %>%
    group_by(GEOID) %>%
    summarize(meanAgeDiff=diffNA(mAge)), by="GEOID") %>%
    mutate(meanAgeDiffQ=cut_number(meanAgeDiff, 6))

pal2 <- function(x, lo=-7, hi=18){
    pal <- colorNumeric(palette = "Spectral", domain=c(lo,hi), reverse=TRUE)
    pal(seq(lo, hi, length.out = x))
}

mapview(agingDF, zcol="meanAgeDiffQ", col.regions=pal2)

rbind(
    ageAcsDF %>%
        as_tibble() %>%
        select(-geometry) %>%
        mutate(ageWeight=ageMean*estimate) %>%
        summarize(mAge=sum(ageWeight)/sum(estimate)) %>%
        mutate(year=2014),
    ageCensusDF %>%
        mutate(ageWeight=ageMean*estimate) %>%
        summarize(mAge=sum(ageWeight)/sum(estimate)) %>%
        mutate(year=1990))
