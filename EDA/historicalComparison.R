rm(list=ls())

library(ipumsr)
library(tidyverse)
library(plotly)
library(tidycensus)
library(jsonlite)
library(survey)

apiKey <- read_json("keys/acs.json")
census_api_key(apiKey$api_key)
varsCenDF <- load_variables(2010, "sf1")


ddi <- read_ipums_ddi("data/usa_00012.xml")
DF <- read_ipums_micro(ddi) %>%
    mutate(Race=case_when(
        HISPAN %in% 1:4 ~ "Hispanic",
        RACE == 1 ~ "White",
        RACE == 2 ~ "Black",
        TRUE ~ NA_character_)) %>%
    mutate(BPLC = case_when(
        BPL < 100 ~ "Native",
        BPL %in% c(210, 250, 260) ~ "CentralAmerica/Caribbean",
        BPL == 300 ~ "SouthAmerica",
        BPL == 200 ~ "Mexico",
        TRUE ~ "Other"
    )) %>%
    mutate(BPLCR = case_when(
        BPL < 100 & HISPAN %in% 1:4 ~ "Native Hispanic",
        BPL < 100 ~ "Native Non-Hispanic",
        BPL %in% c(210, 250, 260) ~ "Central America/Caribbean",
        BPL == 300 ~ "South America",
        BPL == 200 ~ "Mexico",
        TRUE ~ "Other"
    )) %>%
    mutate(FB=YRIMMIG != 0) %>%
    mutate(`Age Group`=(cut_interval(AGE, length=5, labels=F)-1)*5)



stck <- DF %>%
    filter(STATEFIP <= 56) %>%
    group_by(YEAR, BPLC) %>%
    summarize(N = sum(PERWT)) %>%
    filter(BPLC != "Native") %>%
    mutate(Pr = N/sum(N)) %>%
    ggplot(aes(
        x = YEAR, y = N, fill=BPLC, group=BPLC,
        text = paste0("\nPercent: ", round(100*Pr, 2)))) +
    geom_area() +
    theme_classic()

ggplotly(stck)
