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
library(srvyr)

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
    filter(STATEFIP <= 57 & State != "")

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
    right_join(wikiTables, by="Name") %>%
    select(Cluster, STATEFIP)

ddi <- read_ipums_ddi("./data/usa_00015.xml")
DF <- read_ipums_micro(ddi)

allDF <- DF %>%
    filter(YEAR >= 2010) %>%
    filter(STATEFIP < 56) %>%
    mutate(STRATAYEAR = paste(STRATA, YEAR, sep = "_")) %>%
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
        BPL < 100 & RACE == 1 ~ "Native White",
        BPL < 100 & RACE == 2 ~ "Native Black",
        BPL < 100 & RACE == 4 ~ "Native Asian",
        BPL < 100 ~ "Native Other",
        BPL %in% c(210, 250, 260) ~ "Central America/Caribbean",
        BPL == 300 ~ "South America",
        BPL == 200 ~ "Mexico",
        TRUE ~ "Other Migrant"
    )) %>%
    mutate(FB=YRIMMIG != 0) %>%
    mutate(`Age Group`=(cut_interval(AGE, length=5, labels=F)-1)*5) %>%
    left_join(stateRegions, by="STATEFIP") %>%
    as_survey(weights = PERWT)
    #as_survey(ids = CLUSTER, weights = PERWT, strata = STRATAYEAR, nest = TRUE)

if(!file.exists("./results/migRaceDF.Rds")){
    migRaceDF <- allDF %>%
        rename(Age_Group = `Age Group`) %>%
        group_by(Age_Group, Race, YEAR, Cluster) %>%
        mutate(hasMigrated = MIGCOUNTY1 != 0) %>%
        summarize(MigRate = survey_mean(
            hasMigrated, proportion = TRUE, vartype = "ci", na.rm = TRUE)) %>% 
        filter(!is.na(MigRate))

    saveRDS(migRaceDF, "./results/migRaceDF.Rds")
}

if(!file.exists("./results/migOrigDF.Rds")){
    migOrigDF <- allDF %>%
        rename(Age_Group = `Age Group`) %>%
        group_by(Age_Group, BPLCR, YEAR, Cluster) %>%
        mutate(hasMigrated = MIGCOUNTY1 != 0) %>%
        summarize(MigRate = survey_mean(
            hasMigrated, proportion = TRUE, vartype = "ci", na.rm = TRUE)) %>% 
        filter(!is.na(MigRate))
    
    saveRDS(migOrigDF, "./results/migOrigDF.Rds")
}


migOrigDF <- readRDS("./results/migOrigDF.Rds")
migRaceDF <- readRDS("./results/migRaceDF.Rds")

migRaceDF %>%
    filter(Race != "Black" & (Cluster %in% c("South", "Border Land"))) %>%
    filter(Age_Group <= 60 & Race != "Asian" & Age_Group >=10) %>%
    ggplot(aes(
        x=Age_Group, y=MigRate, color=Race, group=Race, fill=Race)) +
    geom_line() +
    geom_ribbon(aes(ymin=MigRate_low, ymax=MigRate_upp, color=NULL), alpha=.3) +
    theme_classic() +
    facet_grid(Cluster~YEAR) +
    labs(y="Migration Rate", title = "Moved From One County to Another")

migOrigDF %>%
    filter(Age_Group <= 55 & Age_Group >=10) %>%
    filter(BPLCR %in% c("Mexico", "Native Hispanic", "Native White")) %>% 
    filter(Cluster %in% c("South", "Border Land")) %>%
    ggplot(aes(
        x=Age_Group, y=MigRate, color=BPLCR, group=BPLCR, fill=BPLCR)) +
    geom_line() +
    geom_ribbon(aes(ymin=MigRate_low, ymax=MigRate_upp, color=NULL), alpha=.3) +
    theme_classic() +
    facet_grid(Cluster~YEAR) +
    labs(y="Migration Rate", title = "Moved From One County to Another")

migOrigDF %>%
    filter(Age_Group <= 55 & Age_Group >=10) %>%
    filter(BPLCR %in% c("Mexico", "Native Hispanic")) %>%
    filter(Cluster %in% c("NorthWest", "SouthEast")) %>%
    ggplot(aes(
        x=Age_Group, y=MigRate, color=Cluster, group=Cluster, fill=Cluster)) +
    geom_line() +
    geom_ribbon(aes(ymin=MigRate_low, ymax=MigRate_upp, color=NULL), alpha=.3) +
    theme_classic() +
    facet_grid(BPLCR~YEAR) +
    labs(y="Migration Rate", title = "Moved From One County to Another")


(migRatesRace <- migRaceDF %>%
    filter(Race != "Black") %>%
    filter(Age_Group <= 60 & Race != "Asian" & Age_Group >=10) %>%
    ggplot(aes(
        x=Age_Group, y=MigRate, color=Race, group=Race, fill=Race)) +
    geom_line() +
    geom_ribbon(aes(ymin=MigRate_low, ymax=MigRate_upp, color=NULL), alpha=.3) +
    theme_classic() +
    facet_grid(Cluster~YEAR) +
    labs(y="Migration Rate", title = "Moved From One County to Another"))

(migRatesOrig <- migOrigDF %>%
    filter(Age_Group <= 55 & Age_Group >=10) %>%
    filter(BPLCR %in% c("Mexico", "Native Hispanic", "Native White")) %>%
    ggplot(aes(
        x=Age_Group, y=MigRate, color=BPLCR, group=BPLCR, fill=BPLCR)) +
    geom_line() +
    geom_ribbon(aes(ymin=MigRate_low, ymax=MigRate_upp, color=NULL), alpha=.3) +
    theme_classic() +
    facet_grid(Cluster~YEAR) +
    labs(y="Migration Rate", title = "Moved From One County to Another"))

list(migRatesRace = migRatesRace, mmigRatesOrig = migRatesOrig) %>%
    saveRDS("results/migAgePlots.Rds")

# (migRatesOrig <- allDF %>%
#     rename(Age_Group = `Age Group`) %>%
#     group_by(Age_Group, BPLCR, YEAR) %>%
#     mutate(hasMigrated = MIGCOUNTY1 != 0) %>%
#     summarize(MigRate = survey_mean(
#         hasMigrated, proportion = TRUE, vartype = "ci", na.rm = TRUE)) %>% 
#     filter(!is.na(MigRate)) %>%
#     ggplot(aes(
#         x=Age_Group, y=MigRate, color=BPLCR, group=BPLCR, fill=BPLCR)) +
#     geom_line() +
#     geom_ribbon(aes(ymin=MigRate_low, ymax=MigRate_upp, color=NULL), alpha=.3) +
#     theme_classic() +
#     facet_wrap(~YEAR) +
#     labs(y="Migration Rate", title = "Moved From One County to Another"))
# 
# ggplotly(migRatesRace)
# ggplotly(migRatesOrig)

