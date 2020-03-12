rm(list=ls())
library(tidyverse)
library(ipumsr)
library(data.table)
library(sf)
library(XML)
library(httr)
library(plotly)
library(srvyr)
library(survey)
library(mvtnorm)

predict.opt <- function(model, newdata, sefit=NULL, trans=boot::inv.logit){
    X <- model.matrix(model$formula[-2], newdata)
    rez <- c(X %*% model$coefficients)
    if(is.null(sefit)){
        return(rez)
    }
    betas <- rmvnorm(sefit, model$coefficients, vcov(model))
    preds <- X %*% t(betas)
    tibble(
        fit = trans(rez),
        lwr = trans(apply(preds, 1, quantile, probs=.025)),
        upr = trans(apply(preds, 1, quantile, probs=.975)))
}

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

plotList <- list()

anyl_groups <- c("Native Hispanic", "Native White", "Mexico")

allDF <- read_ipums_ddi("./data/usa_00017.xml") %>%
    read_ipums_micro() %>%
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
        #BPL %in% c(210, 250, 260) ~ "Central America/Caribbean",
        #BPL == 300 ~ "South America",
        BPL == 200 ~ "Mexico",
        TRUE ~ "Other Migrant"
    )) %>%
    filter(BPLCR %in% anyl_groups) %>%
    mutate(FB=YRIMMIG != 0) %>%
    mutate(`Age Group`=(cut_interval(AGE, length=5, labels=F)-1)*5) %>%
    left_join(stateRegions, by="STATEFIP") %>%
    filter(!(MIGRATE1D %in% c(0, 20, 40))) %>%
    mutate(SEX = ifelse(SEX == 1, "Male", "Female"), YEAR = as.factor(YEAR)) %>%
    mutate(hasMigrated = as.integer(MIGRATE1D >= 24), AGE2 = AGE^2) %>%
    filter(!is.na(Cluster))
    # as_survey(weights = PERWT)

summary(lm1 <- svyglm(
    hasMigrated ~ YEAR, design = allDF, family = "binomial"))

summary(lm2 <- svyglm(
    hasMigrated ~ YEAR + SEX, design = allDF, family = "binomial"))

summary(lm3 <- svyglm(
    hasMigrated ~ YEAR + SEX + BPLCR, design = allDF, family = "binomial"))

summary(lm4 <- svyglm(
    hasMigrated ~ YEAR + SEX + BPLCR + Cluster,
    design = allDF, family = "binomial"))

summary(lm5 <- svyglm(
    hasMigrated ~ YEAR + SEX + BPLCR * Cluster,
    design = allDF, family = "binomial"))

summary(lm6 <- svyglm(
    hasMigrated ~ YEAR + SEX + BPLCR * Cluster + AGE + AGE2,
    design = allDF, family = "binomial"))

summary(lm7 <- svyglm(
    hasMigrated ~ SEX + YEAR * BPLCR * Cluster + AGE + AGE2,
    design = allDF, family = "binomial"))

summary(lm8 <- svyglm(
    hasMigrated ~ SEX + YEAR + BPLCR * Cluster * AGE + AGE2 * BPLCR * Cluster,
    design = allDF, family = "binomial"))

summary(lm9 <- svyglm(
    hasMigrated ~ YEAR + SEX * BPLCR * Cluster * AGE + 
        AGE2 * BPLCR * Cluster * SEX,
    design = allDF, family = "binomial"))

summary(lm10 <- svyglm(
    hasMigrated ~ YEAR + SEX * BPLCR + BPLCR * Cluster * AGE + 
        AGE2 * BPLCR * Cluster,
    design = allDF, family = "binomial"))

BIC(lm8, lm10, maximal = lm10)

plotList$lm1 <- allDF %>%
    as_tibble() %>% select(YEAR) %>% unique() %>% 
    bind_cols(predict.opt(lm1, ., sefit=1000)) %>%
    ggplot(aes(x = YEAR, y = fit, ymin = lwr, ymax = upr)) +
    geom_point() +
    geom_errorbar() +
    theme_classic() +
    labs(x="Year", y="Probability of Migrating") +
    ggtitle("Inter-Puma Migration Probabilties")

plotList$lm2 <- allDF %>%
    as_tibble() %>% select(YEAR, SEX) %>% unique() %>% 
    bind_cols(predict.opt(lm2, ., sefit=1000)) %>%
    ggplot(aes(x = YEAR, y = fit, ymin = lwr, ymax = upr, color=SEX)) +
    geom_point() +
    geom_errorbar() +
    theme_classic() +
    labs(x="Year", y="Probability of Migrating") +
    ggtitle("Inter-Puma Migration Probabilties")

plotList$lm3 <- allDF %>%
    as_tibble() %>% 
    select(YEAR, SEX, BPLCR) %>% unique() %>% 
    bind_cols(predict.opt(lm3, ., sefit=1000)) %>%
    filter(YEAR == 2018 & SEX == "Female") %>%
    ggplot(aes(x = BPLCR, y = fit, ymin = lwr, ymax = upr, color=SEX)) +
    geom_point() +
    geom_errorbar() +
    theme_classic() +
    labs(x="Year", y="Probability of Migrating") +
    ggtitle("Inter-Puma Migration Probabilties: 2018")

plotList$lm4 <- allDF %>%
    as_tibble() %>%
    select(YEAR, SEX, BPLCR, Cluster) %>% unique() %>% na.omit() %>%
    bind_cols(predict.opt(lm4, ., sefit=1000)) %>%
    filter(YEAR == 2018 & SEX == "Female") %>%
    ggplot(aes(x = Cluster, y = fit, ymin = lwr, ymax = upr, color=BPLCR)) +
    geom_point() +
    geom_errorbar() +
    theme_classic() +
    labs(x="Year", y="Probability of Migrating") +
    ggtitle("Inter-Puma Migration Probabilties: 2018")

plotList$lm5 <- allDF %>%
    as_tibble() %>%
    select(YEAR, SEX, BPLCR, Cluster) %>% unique() %>% na.omit() %>%
    bind_cols(predict.opt(lm5, ., sefit=1000)) %>%
    filter(YEAR == 2018 & SEX == "Female") %>%
    ggplot(aes(x = Cluster, y = fit, ymin = lwr, ymax = upr, color=BPLCR)) +
    geom_point() +
    geom_errorbar() +
    theme_classic() +
    labs(x="Year", y="Probability of Migrating") +
    ggtitle("Inter-Puma Migration Probabilties: 2018")

plotList$lm6 <- allDF %>%
    as_tibble() %>%
    select(YEAR, SEX, BPLCR, Cluster) %>% unique() %>% na.omit() %>%
    mutate(AGE = 25, AGE2 = AGE^2) %>%
    bind_cols(predict.opt(lm6, ., sefit=1000)) %>%
    filter(YEAR == 2018 & SEX == "Female") %>%
    ggplot(aes(x = Cluster, y = fit, ymin = lwr, ymax = upr, color=BPLCR)) +
    geom_point() +
    geom_errorbar() +
    theme_classic() +
    labs(x="Year", y="Probability of Migrating") +
    ggtitle("Inter-Puma Migration Probabilties: 2018 for 25 year olds")

(plotList$lm8 <- allDF %>%
    as_tibble() %>%
    select(YEAR, SEX, BPLCR, Cluster) %>% unique() %>% na.omit() %>%
    mutate(AGE = 25, AGE2 = AGE^2) %>%
    bind_cols(predict.opt(lm8, ., sefit=1000)) %>%
    filter(YEAR == 2018 & SEX == "Female") %>%
    ggplot(aes(x = Cluster, y = fit, ymin = lwr, ymax = upr, color=BPLCR)) +
    geom_point() +
    geom_errorbar() +
    theme_classic() +
    labs(x="Year", y="Probability of Migrating") +
    ggtitle("Inter-Puma Migration Probabilties: 2018 for 25 year olds"))

(plotList$lm10 <- allDF %>%
    as_tibble() %>%
    select(YEAR, SEX, BPLCR, Cluster) %>% unique() %>% na.omit() %>%
    mutate(AGE = 25, AGE2 = AGE^2) %>%
    bind_cols(predict.opt(lm10, ., sefit=1000)) %>%
    filter(YEAR == 2018 & SEX == "Female") %>%
    ggplot(aes(x = Cluster, y = fit, ymin = lwr, ymax = upr, color=BPLCR)) +
    geom_point() +
    geom_errorbar() +
    theme_classic() +
    labs(x="Year", y="Probability of Migrating") +
    ggtitle("Inter-Puma Migration Probabilties: 2018 for 25 year olds"))

(plotList$age_model_plot <- allDF %>%
    as_tibble() %>%
    select(YEAR, SEX, BPLCR, Cluster) %>% unique() %>% na.omit() %>%
    mutate(Key = 1) %>%
    left_join(tibble(AGE = 15:65, AGE2 = (15:65)^2, Key = 1), by = "Key") %>%
    bind_cols(predict.opt(lm10, ., sefit=1000)) %>%
    filter(YEAR == 2018 & Cluster %in% c("South", "Border Land")) %>%
    ggplot(aes(x = AGE, y = fit, ymin = lwr, ymax = upr, group=BPLCR)) +
    geom_line(aes(color=BPLCR)) +
    geom_ribbon(aes(fill=BPLCR), alpha = .4) +
    theme_classic() +
    labs(x="Age", y="Probability of Migrating") +
    ggtitle("Inter-Puma Migration Probabilties: 2018 for Females") +
    facet_grid(SEX~Cluster))

saveRDS(plotList, "./plots/basiclmPlots.RDS")
