rm(list=ls())
library(tidyverse)
library(ipumsr)

DF <- read_ipums_ddi("./data/usa_00023.xml") %>%
    read_ipums_micro() %>%
    filter(BPL == 200 | BPL <= 99 | BPL == 210 | BPL == 300) %>%
    filter(AGE >= 18 & AGE <= 65) %>%
    mutate(AGEG = cut(AGE, c(17, 29, 39, 49, 65))) %>%
    mutate(MX = BPL == 200) %>%
    mutate(BPLCR = case_when(
        BPL < 100 & HISPAN %in% 1:4 ~ "Native Hispanic",
        # BPL < 100 & RACE == 1 ~ "Native White",
        # BPL < 100 & RACE == 2 ~ "Native Black",
        # BPL < 100 & RACE == 4 ~ "Native Asian",
        BPL < 100 ~ "Native Other",
        # BPL %in% c(210, 250, 260) ~ "Central America/Caribbean",
        # BPL == 300 ~ "South America",
        BPL == 200 ~ "Mexico",
        TRUE ~ "Other Migrant")) %>%
    mutate(EDU = case_when(
        EDUC < 6  ~ "Below High School",
        EDUC == 6 ~ "High School",
        EDUC > 6 & EDUC < 10 ~ "Some College",
        EDUC >= 10 ~ "College",
        TRUE ~ NA_character_
    )) %>%
    mutate(EDU = fct_relevel(
        EDU, 
        c("Below High School", "High School", "Some College", "College"))) %>%
    mutate(UNDOC = 
               CITIZEN >= 3 &
               YRIMMIG >= 1990 &
               EDUC <= 7 &
               SCHOOL != 2 & 
               !(CLASSWKRD %in% 24:28)) %>%
    mutate(CHILD = NCHILD != 0, MARRIED = MARST <= 2) %>%
    mutate(LEFTARIZONA = MIGPLAC1 == 4 & STATEFIP != 4, CIT = CITIZEN < 3) %>%
    mutate(ETHCIT1 = case_when(
        BPL < 100 & HISPAN %in% 1:4 ~ "Native Latino",
        BPL < 100 & RACE == 1 ~ "Native White",
        BPL >= 150 & CIT & HISPAN %in% 1:4 ~ "FB Latino Citizen",
        BPL >= 150 & !CIT & HISPAN %in% 1:4 ~ "FB Latino Non-Citizen",
        TRUE ~ NA_character_)) %>%
    mutate(ETHCIT1 = fct_relevel(
        ETHCIT1,
        c("Native White", "Native Latino", 
          "FB Latino Citizen", "FB Latino Non-Citizen")
    )) %>%
    mutate(ETHCIT2 = case_when(
        BPL < 100 & HISPAN %in% 1:4 ~ "Native Hispanic",
        BPL < 100 & RACE == 1 ~ "Native White",
        (BPL %in% c(200, 210, 300)) & CIT ~ "FB Latino Citizen",
        (BPL %in% c(200, 210, 300)) & UNDOC ~ "FB Latino Undocumented",
        (BPL %in% c(200, 210, 300)) & !CIT ~ "FB Latino Non-Citizen",
        TRUE ~ NA_character_)) %>%
    # Right now lets stick to CA and NV because we are doing MX pop
    mutate(LEFTCONTROL = MIGPLAC1 %in% c(6, 32)) %>%
    filter(LEFTARIZONA | (MIGPLAC1 %in% c(0,4) & STATEFIP == 4)) %>%
    # filter(LEFTARIZONA | (STATEFIP == 4)) %>%
    select(
        YEAR, SEX, AGEG, UNDOC, CHILD, MARRIED, BPLCR,
        LEFTARIZONA, CIT, EDU, ETHCIT1, PERWT) %>%
    mutate(YEAR = as.factor(YEAR))
    
# Model Close to Ellis et all 
subDF <- DF %>%
    filter(YEAR != "2005" & YEAR != "2010" & YEAR != "2011") %>%
    filter(!is.na(ETHCIT1))

subDF %>%
    group_by(ETHCIT1, YEAR) %>%
    summarize(pout = sum(LEFTARIZONA*PERWT)/sum(PERWT)) %>%
    ggplot(aes(x = ETHCIT1, y = pout, fill = YEAR)) +
    theme_classic() +
    geom_col(position = "dodge")

summary(glm1 <- glm(
    LEFTARIZONA ~ SEX + AGEG + EDU + MARRIED * CHILD + YEAR * ETHCIT1, 
    data = subDF, family = "binomial"))
