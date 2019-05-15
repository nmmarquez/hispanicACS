rm(list=ls())
library(tidyverse)
library(ipumsr)
library(data.table)
library(sf)
library(XML)
library(httr)

setwd("~/Documents/hispanicDemographics/data/")

# ddi <- read_ipums_ddi("usa_00002.xml")
# data <- read_ipums_micro(ddi)
# write_csv(data, "./usa_00002.csv")

DF <- as_tibble(fread("./usa_00002.csv")) %>%
    mutate(Race=case_when(
        HISPAN %in% 1:4 ~ "Hispanic",
        RACE == 1 ~ "White",
        RACE == 2 ~ "Black",
        TRUE ~ NA_character_)) %>%
    mutate(FB=YRIMMIG != 0) %>%
    mutate(`Age Group`=(cut_interval(AGE, length=5, labels=F)-1)*5)

spDF <- read_sf("./ipumsShape/ipums_puma_2010.shp")

wikiTables <- paste0(
    "https://en.wikipedia.org/wiki/",
    "Federal_Information_Processing_Standard_state_code") %>%
    GET() %>%
    content("text") %>%
    {readHTMLTable(doc=., header=T)} %>%
    .[[1]] %>%
    mutate_if(is.factor, as.character) %>%
    mutate(STATEFIP=as.numeric(`Numeric code`)) %>%
    rename(State=`Alpha code`) %>%
    as_tibble()

stateRegions <- bind_rows(list(
    tibble(
        Name=c("California", "Arizona", "New Mexico", "Texas"),
        Cluster="Border Land"),
    
    tibble(
        Name=c("Washington", "Oregon", "Nevada", "Utah", "Idaho"),
        Cluster="NorthWest"),
    
    tibble(
        Name=c("Montana", "North Dakota", "South Dakota", "Minnesota",
                "Iowa", "Wyoming", "Nebraska", "Colorado", "Kansas", 
                "Oklahoma", "Missouri"),
        Cluster="Great Plains"),
    
    tibble(
        Name=c("Louisiana", "Alabama", "Mississippi", "Tennessee", 
                "Kentucky", "Arkansas"),
        Cluster="South"),
    
    tibble(
        Name=c("Florida", "Georgia", "South Carolina", 
                "North Carolina", "Virginia", "West Virginia",
                "Maryland", "District of Columbia", "Delaware"),
        Cluster="SouthEast"),
    
    tibble(
        Name=c("Maine", "New Jersey", "Pennsylvania", "New York",
                "Connecticut", "Massachusetts", "Rhode Island", 
                "Vermont", "New Hampshire"),
        Cluster="NorthEast"),
    
    tibble(
        Name=c("Wisconsin", "Illinois", "Michigan", "Ohio",
                "Indiana"),
        Cluster="Great Lakes"))) %>%
    left_join(wikiTables, by="Name") %>%
    select(Name, Cluster, State, STATEFIP)

# The distribution of migrants is narrower
# DF %>%
#     filter(!is.na(Race)) %>%
#     group_by(YEAR, Race, FB, `Age Group`) %>%
#     summarize(Count=n()) %>%
#     mutate(Proportion=Count/sum(Count)) %>%
#     ggplot(aes(x=`Age Group`, y=Proportion, group=FB, color=FB, fill=FB)) +
#     geom_col(position = "dodge") +
#     facet_grid(Race ~ YEAR) +
#     theme_classic()

# and is also older (mostly because of less migrant babies)
# DF %>%
#     group_by(PUMA, STATEFIP, YEAR, Race, FB) %>%
#     summarize(Age=mean(AGE), N=n()) %>%
#     filter(!is.na(Race)) %>%
#     group_by(YEAR, Race, FB) %>%
#     mutate(p=N/sum(N)) %>%
#     summarize(Age=sum(p*Age)) %>%
#     arrange(YEAR, Race, FB) %>%
#     as.data.frame

# Lets check out how age descrepancy between general population and FB hispanics
# change over time and region

analyzeDF <- DF %>%
    filter(YEAR > 1980) %>%
    filter(!(FB & (Race != "Hispanic"))) %>%
    group_by(YEAR, FB, PUMA, STATEFIP) %>%
    summarize(Age=mean(AGE)) %>%
    ungroup() %>%
    arrange(YEAR, STATEFIP, PUMA, FB) %>%
    group_by(YEAR, STATEFIP, PUMA) %>%
    summarize(
        ageDiff = last(Age) - first(Age),
        observations = n()
    ) %>%
    filter(observations == 2) %>%
    left_join(stateRegions, by="STATEFIP")