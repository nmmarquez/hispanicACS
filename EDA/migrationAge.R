# We want to compare the probability of being a recent migrant among different 
# ethnic groups and national identities in the united States while adjusting for
# the age at which individuals are migrating. Because the age of Mexican 
# nationals living in the United States is older than the adult population in
# the United States we want to make sure taht we are accurately capturing
# patterns.

rm(list=ls())
library(tidyverse)
library(ipumsr)
library(data.table)
library(sf)
library(XML)
library(httr)
library(plotly)

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
    as_tibble() %>%
    filter(State %in% c("WA", "CA", "OR"))

allDF <- as_tibble(fread("./data/usa_00007.csv")) %>%
    filter(STATEFIP %in% wikiTables$STATEFIP) %>%
    mutate(Race=case_when(
        HISPAN %in% 1:4 ~ "Hispanic",
        RACE == 1 ~ "White",
        RACE == 2 ~ "Black",
        RACE == 4 ~ "Asian",
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

migRatesRace <- allDF %>%
    #filter(YEAR>=2010) %>%
    group_by(`Age Group`, Race, YEAR) %>%
    select(`Age Group`, Race, YEAR, PERWT, MIGCOUNTY1) %>%
    summarize(MigRate=sum((MIGCOUNTY1 != 0) * PERWT) / sum(PERWT)) %>%
    ungroup() %>%
    ggplot(aes(x=`Age Group`, y=MigRate, group=Race, color=Race)) +
    geom_line(alpha=.8) +
    theme_classic() +
    facet_wrap(~YEAR) +
    labs(y="Migration Rate", title = "Moved From One County to Another")

migRatesOrig <- allDF %>%
    #filter(YEAR>=2010) %>%
    group_by(`Age Group`, BPLCR, YEAR) %>%
    select(`Age Group`, BPLCR, YEAR, PERWT, MIGCOUNTY1) %>%
    summarize(MigRate=sum((MIGCOUNTY1 != 0) * PERWT) / sum(PERWT)) %>%
    ungroup() %>%
    ggplot(aes(x=`Age Group`, y=MigRate, group=BPLCR, color=BPLCR)) +
    geom_line(alpha=.8) +
    theme_classic() +
    facet_wrap(~YEAR) +
    labs(y="Migration Rate", title = "Moved From One County to Another")

ggplotly(migRatesRace)
ggplotly(migRatesOrig)
