rm(list=ls())
library(tidyverse)
library(ipumsr)
library(data.table)
library(sf)
library(XML)
library(httr)
library(plotly)
library(srvyr)
library(circlize)
library(chorddiag)

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
    select(Cluster, STATEFIP) %>%
    mutate(STATEFIP = sprintf("%02d", STATEFIP))

anyl_groups <- c("Native Hispanic", "Native White", "Mexico", "Other Migrant")

cwPUMA <- read_rds("./data/puma2migpuma.RDS")

allDF <- read_ipums_ddi("./data/usa_00019.xml") %>%
    read_ipums_micro() %>%
    # remove individuals not in contigous US
    filter(STATEFIP < 56 & !(STATEFIP %in% c(2, 15))) %>%
    # remove individuals who lived abroad last year or whose migration 
    # info is unknown ubless they are from Mexico
    filter(!(MIGRATE1D %in% c(0, 20, 40)) | MIGPLAC1 == 200) %>%
    mutate(MIGPLAC1 = if_else(MIGPLAC1 == 200, 99, as.double(MIGPLAC1))) %>%
    # remove individuals who migrated from alaska, hawaii, or puerto rico
    filter(!(MIGPLAC1 %in% c(110, 2, 15))) %>%
    mutate(STATEFIP = sprintf("%02d", STATEFIP)) %>%
    mutate(MIGPLAC1 = sprintf("%02d", MIGPLAC1)) %>%
    mutate(PUMA = str_c(STATEFIP, sprintf("%05d", PUMA))) %>%
    # merge on the subjects current year MIGPUMA
    left_join(
        cwPUMA %>%
            mutate(
                PUMA = str_c(STATEFIP, PUMA),
                MIGPUMA = str_c(STATEFIP, MIGPUMA)) %>%
            select(PUMA, MIGPUMA),
        by = "PUMA") %>%
    mutate(MIGPUMA1 = sprintf("%05d", MIGPUMA1)) %>%
    mutate(MIGPUMA1 = ifelse(MIGPUMA1 == "00000", NA, MIGPUMA1)) %>%
    mutate(MIGPUMA1 = str_c(MIGPLAC1, MIGPUMA1)) %>%
    # MERGE on Geographic information from last years location
    left_join(
        cwPUMA %>%
            mutate(
                STATEFIP1 = STATEFIP,
                PUMA1 = str_c(STATEFIP, PUMA),
                MIGPUMA1 = str_c(STATEFIP, MIGPUMA)) %>%
            select(MIGPUMA1, STATEFIP1) %>% 
            unique(),
        by = "MIGPUMA1") %>%
    # Megre on cluster information
    left_join(stateRegions, by="STATEFIP") %>%
    # Megre on cluster information from year prior
    left_join(
        rename(stateRegions, STATEFIP1 = STATEFIP, Cluster1 = Cluster),
        by="STATEFIP1") %>% 
    # define ethinic/nativity status
    mutate(BPLCR = case_when(
        BPL < 100 & HISPAN %in% 1:4 ~ "Native Hispanic",
        BPL < 100 & RACE == 1 ~ "Native White",
        BPL < 100 & RACE == 2 ~ "Native Black",
        BPL < 100 & RACE == 4 ~ "Native Asian",
        BPL < 100 ~ "Native Other",
        #BPL %in% c(210, 250, 260) ~ "Central America/Caribbean",
        #BPL == 300 ~ "South America",
        BPL == 200 ~ "Mexico",
        TRUE ~ "Other Migrant"
    )) %>%
    mutate(`Age Group`=(cut_interval(AGE, length=5, labels=F)-1)*5) %>%
    # Keep only individuals in the analysis groups
    filter(BPLCR %in% anyl_groups) %>%
    # define a migrated variable
    mutate(hasMigrated = MIGRATE1D >= 24) %>%
    mutate(SEX = ifelse(SEX == 1, "Male", "Female")) %>%
    mutate(MEX1 = MIGPLAC1 == "99") %>%
    mutate(MEX1 = ifelse(hasMigrated, MEX1, NA)) %>%
    select(
        YEAR, Cluster, Cluster1, STATEFIP, STATEFIP1, MIGPUMA, MIGPUMA1, MEX1,
        AGE, `Age Group`, SEX, hasMigrated, BPLCR, PERWT)

mycolor <- viridis::viridis(7, alpha = 1, begin = 0, end = 1, option = "D")
mycolor <- mycolor[sample(1:7)]

allDF %>%
    filter(!is.na(Cluster1) & !is.na(Cluster)) %>%
    group_by(Cluster, Cluster1) %>%
    summarize(value = sum(PERWT)) %>%
    chordDiagram(grid.col = mycolor)
title("US Region Migration")

allDF %>%
    filter(!is.na(Cluster1) & !is.na(Cluster)) %>%
    group_by(Cluster, Cluster1) %>%
    summarize(value = sum(PERWT)) %>%
    mutate(value = ifelse(Cluster == Cluster1, 0, value)) %>%
    chordDiagram(grid.col = mycolor)
title("US Region Migration")


layout(matrix(1:4, 2, 2))

allDF %>%
    filter(!is.na(Cluster1) & !is.na(Cluster) & BPLCR == "Native White") %>%
    group_by(Cluster, Cluster1) %>%
    summarize(value = sum(PERWT)) %>%
    mutate(value = ifelse(Cluster == Cluster1, 0, value)) %>%
    chordDiagram(grid.col = mycolor)
title("Native White")

allDF %>%
    filter(!is.na(Cluster1) & !is.na(Cluster) & BPLCR == "Other Migrant") %>%
    group_by(Cluster, Cluster1) %>%
    summarize(value = sum(PERWT)) %>%
    mutate(value = ifelse(Cluster == Cluster1, 0, value)) %>%
    chordDiagram(grid.col = mycolor)
title("Other Migrant")

allDF %>%
    filter(!is.na(Cluster1) & !is.na(Cluster) & BPLCR == "Native Hispanic") %>%
    group_by(Cluster, Cluster1) %>%
    summarize(value = sum(PERWT)) %>%
    mutate(value = ifelse(Cluster == Cluster1, 0, value)) %>%
    chordDiagram(grid.col = mycolor)
title("Native Hispanic")

allDF %>%
    filter(!is.na(Cluster1) & !is.na(Cluster) & BPLCR == "Mexico") %>%
    group_by(Cluster, Cluster1) %>%
    summarize(value = sum(PERWT)) %>%
    mutate(value = ifelse(Cluster == Cluster1, 0, value)) %>%
    chordDiagram(grid.col = mycolor)
title("Mexico")

layout(matrix(1:4, 2, 2))

allDF %>%
    filter(!is.na(Cluster1) & !is.na(Cluster) & BPLCR == "Native White") %>%
    group_by(Cluster, Cluster1) %>%
    summarize(value = sum(PERWT)) %>%
    chordDiagram(grid.col = mycolor)
title("Native White")

allDF %>%
    filter(!is.na(Cluster1) & !is.na(Cluster) & BPLCR == "Other Migrant") %>%
    group_by(Cluster, Cluster1) %>%
    summarize(value = sum(PERWT)) %>%
    chordDiagram(grid.col = mycolor)
title("Other Migrant")

allDF %>%
    filter(!is.na(Cluster1) & !is.na(Cluster) & BPLCR == "Native Hispanic") %>%
    group_by(Cluster, Cluster1) %>%
    summarize(value = sum(PERWT)) %>%
    chordDiagram(grid.col = mycolor)
title("Native Hispanic")

allDF %>%
    filter(!is.na(Cluster1) & !is.na(Cluster) & BPLCR == "Mexico") %>%
    group_by(Cluster, Cluster1) %>%
    summarize(value = sum(PERWT)) %>%
    chordDiagram(grid.col = mycolor)
title("Mexico")

layout(matrix(1, 1, 1))

# lets do a dataset of people who leave an area
# test <- allDF %>%
#     filter(hasMigrated) %>%
#     mutate(changedCluster = Cluster != Cluster1) %>%
#     as_survey(weights = PERWT)
# 
# lm1 <- svyglm(
#     changedCluster ~ BPLCR, design = test, family = "binomial")
# lm2 <- svyglm(
#     changedCluster ~ BPLCR + Cluster1, design = test, family = "binomial")
# lm3 <- svyglm(
#     changedCluster ~ BPLCR * Cluster1, design = test, family = "binomial")

# pop change due to migration
bind_rows(lapply(na.omit(unique(stateRegions$Cluster)), function(c){
    allDF %>%
        filter(Cluster == c | Cluster1 == c) %>%
        filter(Cluster != Cluster1) %>%
        mutate(PERWT = ifelse(Cluster == c, PERWT, -PERWT)) %>%
        group_by(YEAR, BPLCR) %>%
        summarize(diff = sum(PERWT)) %>%
        ungroup() %>%
        mutate(Cluster = c)})) %>% 
    print(n=100)

allDF %>%
    filter(!is.na(MIGPUMA) & !is.na(MIGPUMA1)) %>%
    group_by(YEAR, MIGPUMA, MIGPUMA1) %>%
    summarize(N = sum(PERWT)) %>%
    arrange(-N)

crossMigPuma <- allDF %>%
    filter(!is.na(MIGPUMA) & !is.na(MIGPUMA1)) %>%
    filter(MIGPUMA != MIGPUMA1) %>%
    group_by(YEAR, MIGPUMA, MIGPUMA1, BPLCR) %>%
    summarize(N = sum(PERWT)) %>%
    arrange(-N) %>%
    ungroup() %>%
    filter(YEAR == 2018)

crossMigPuma %>%
    filter(BPLCR == "Native White")

crossMigPuma %>%
    filter(BPLCR == "Native Hispanic")

crossMigPuma %>%
    filter(BPLCR == "Mexico")


# Growth of FBMI pop due to internal vs external immigration
# basically everyone has been here for more than a year
allDF %>%
    filter(hasMigrated & BPLCR == "Mexico") %>%
    group_by(YEAR, MEX1) %>%
    summarize(N = sum(PERWT)) %>%
    summarize(diff = N[1]/N[2])

allDF %>%
    filter(hasMigrated & BPLCR == "Mexico") %>%
    group_by(YEAR, Cluster, MEX1) %>%
    summarize(N = sum(PERWT)) %>%
    summarize(diff = N[1]/N[2]) %>%
    print(n=30)

migNatDF <- allDF %>%
    as_tibble() %>% 
    filter(BPLCR %in% anyl_groups) %>% 
    rename(Age_Group = `Age Group`) %>%
    group_by(Age_Group, BPLCR, YEAR, SEX) %>%
    #mutate(hasMigrated = MIGRATE1D >= 24) %>%
    summarize(MigRate = sum(hasMigrated * PERWT) / sum(PERWT)) %>%
    filter(!is.na(MigRate)) %>%
    ungroup()

migNatCIDF <- allDF %>%
    filter(BPLCR %in% anyl_groups) %>% 
    rename(Age_Group = `Age Group`) %>%
    group_by(Age_Group, BPLCR, YEAR, SEX) %>%
    #mutate(hasMigrated = MIGRATE1D >= 24) %>%
    summarize(MigRate = survey_mean(
    hasMigrated, proportion = TRUE, vartype = "ci", na.rm = TRUE)) %>% 
    filter(!is.na(MigRate))

migCIDF <- allDF %>%
    filter(BPLCR %in% anyl_groups) %>% 
    rename(Age_Group = `Age Group`) %>%
    group_by(YEAR) %>%
    #mutate(hasMigrated = MIGRATE1D >= 24) %>%
    summarize(MigRate = survey_mean(
        hasMigrated, proportion = TRUE, vartype = "ci", na.rm = TRUE)) %>% 
    filter(!is.na(MigRate))

migAgeCIDF <- allDF %>%
    rename(Age_Group = `Age Group`) %>%
    group_by(YEAR, Age_Group) %>%
    # mutate(hasMigrated = MIGRATE1D >= 24) %>%
    summarize(MigRate = survey_mean(
        hasMigrated, proportion = TRUE, vartype = "ci", na.rm = TRUE)) %>% 
    filter(!is.na(MigRate))

# migNatClusterCIDF <- allDF %>%
#     filter(BPLCR %in% anyl_groups) %>% 
#     rename(Age_Group = `Age Group`) %>%
#     group_by(Age_Group, BPLCR, YEAR, SEX, Cluster) %>%
#     mutate(hasMigrated = MIGRATE1D >= 24) %>%
#     summarize(MigRate = survey_mean(
#         hasMigrated, proportion = TRUE, vartype = "ci", na.rm = TRUE)) %>% 
#     filter(!is.na(MigRate))

migNatClusterDF <- allDF %>%
    as_tibble() %>%
    filter(BPLCR %in% anyl_groups) %>%
    rename(Age_Group = `Age Group`) %>%
    group_by(Age_Group, BPLCR, YEAR, SEX, Cluster) %>%
    #mutate(hasMigrated = MIGRATE1D >= 24) %>%
    summarize(MigRate = sum(hasMigrated * PERWT) / sum(PERWT)) %>%
    filter(!is.na(MigRate)) %>%
    ungroup()

plotList <- list()

plotList$nat_trend <- migCIDF %>%
    ggplot(aes(x=YEAR, y = MigRate, ymin = MigRate_low, ymax = MigRate_upp)) +
    geom_line() +
    geom_ribbon(alpha=.3) +
    theme_classic()

(plotList$age_mig <- migAgeCIDF %>%
    ggplot(aes(
        x=Age_Group, y = MigRate, ymin = MigRate_low, ymax = MigRate_upp,
        fill=YEAR, group=YEAR)) +
    geom_line(aes(color=YEAR)) +
    geom_ribbon(aes(fill=YEAR), alpha=.3) +
    #scale_fill_distiller(palette = "Spectral") +
    #scale_color_distiller(palette = "Spectral") +
    theme_classic())

(plotList$sex_bplcr_mig <- filter(migNatDF) %>%
    mutate(SEX = ifelse(SEX == 1, "Male", "Female")) %>%
    ggplot(aes(x = Age_Group, y = MigRate, color = BPLCR)) +
    geom_line() +
    theme_classic() +
    facet_grid(YEAR~SEX))

(plotList$sex_bplcr_ci_mig <- filter(migNatCIDF, Age_Group < 80) %>%
    mutate(SEX = ifelse(SEX == 1, "Male", "Female")) %>%
    ggplot(aes(
        x = Age_Group, y = MigRate, ymin = MigRate_low,
        ymax = MigRate_upp, fill=BPLCR)) +
    geom_line(aes(color = BPLCR)) +
    geom_ribbon(alpha=.3) +
    theme_classic() +
    facet_grid(YEAR~SEX))

(plotList$age_bplcr_trend_mig <- migNatCIDF %>%
    filter(Age_Group <= 30 & Age_Group >= 15) %>%
    mutate(SEX = ifelse(SEX == 1, "Male", "Female")) %>%
    ggplot(aes(
        x = YEAR, y = MigRate, ymin = MigRate_low,
        ymax = MigRate_upp, fill=BPLCR)) +
    geom_line(aes(color = BPLCR)) +
    geom_ribbon(alpha=.3) +
    theme_classic() +
    facet_grid(Age_Group~SEX))

# filter(migNatClusterCIDF, Age_Group < 60 & YEAR == 2017 & Age_Group > 0) %>%
#     filter(BPLCR %in% c("Mexico", "Native White")) %>%
#     mutate(SEX = ifelse(SEX == 1, "Male", "Female")) %>%
#     ggplot(aes(
#         x = Age_Group, y = MigRate, ymin = MigRate_low,
#         ymax = MigRate_upp, fill=BPLCR)) +
#     geom_line(aes(color = BPLCR)) +
#     geom_ribbon(alpha=.3) +
#     theme_classic() +
#     facet_grid(Cluster~SEX, scales = "free_y") +
#     ggtitle("Year: 2017")

(plotList$cluster_white_compare <- migNatClusterDF %>%
    filter(Age_Group < 60 & Age_Group > 0) %>%
    filter(!is.na(Cluster)) %>% 
    filter(BPLCR %in% c("Mexico", "Native White")) %>%
    mutate(SEX = ifelse(SEX == 1, "Male", "Female")) %>%
    ggplot(aes(
        x = Age_Group, y = MigRate, fill=BPLCR)) +
    geom_line(aes(color = BPLCR)) +
    #geom_ribbon(alpha=.3) +
    theme_classic() +
    facet_grid(Cluster~SEX+YEAR, scales = "free_y"))

(plotList$cluster_hisp_compare <- migNatClusterDF %>%
    filter(Age_Group < 60 & Age_Group > 0) %>%
    filter(!is.na(Cluster)) %>% 
    filter(BPLCR %in% c("Mexico", "Native Hispanic")) %>%
    mutate(SEX = ifelse(SEX == 1, "Male", "Female")) %>%
    ggplot(aes(
        x = Age_Group, y = MigRate, fill=BPLCR)) +
    geom_line(aes(color = BPLCR)) +
    #geom_ribbon(alpha=.3) +
    theme_classic() +
    facet_grid(Cluster~SEX+YEAR, scales = "free_y"))

saveRDS(plotList, "./plots/edaPlots.RDS")

