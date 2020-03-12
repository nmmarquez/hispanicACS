rm(list=ls())
library(tidyverse)
library(ipumsr)
library(data.table)
library(sp)
library(sf)
library(XML)
library(httr)
library(leaflet)
library(maps)

DF <- st_as_sf(map("county", plot = FALSE, fill = TRUE)) %>%
    left_join(county.fips %>%
        rename(ID = polyname) %>%
        mutate(FIPS = sprintf("%05d", fips)), by = "ID") %>%
    select(-fips, -ID) %>%
    right_join(
        read_nhgis("./data/countyHistory/nhgis0030_csv.zip") %>%
            rename(MX = AJ7BD) %>%
            select(YEAR, STATE, STATEFP, COUNTY, COUNTYFP, MX) %>%
            mutate(FIPS = str_c(STATEFP, COUNTYFP)) %>%
            mutate(YEAR = ifelse(YEAR == "2008-2012", "2010", YEAR)) %>%
            mutate(YEAR = as.numeric(YEAR)), by = "FIPS")
        

DF %>%
    # For log scaling
    mutate(MX = MX + 1) %>%
    ggplot(aes(fill = MX)) +
    geom_sf(lwd = 0) +
    theme_void() +
    scale_fill_distiller(palette = "Spectral") +
    facet_wrap(~YEAR) +
    ggtitle("Mexican Born Population Living in The United States by County") +
    labs(fill="")

DF %>%
    # For log scaling
    mutate(MX = MX + 1) %>%
    ggplot(aes(fill = MX)) +
    geom_sf(lwd = 0) +
    theme_void() +
    scale_fill_distiller(
        palette = "Spectral", trans = "log", breaks = 10^c(0, 2, 4, 6),
        labels = c("1", "100", "10,000", "1,000,000")) +
    facet_wrap(~YEAR) +
    ggtitle("Mexican Born Population Living in The United States by County") +
    labs(fill="")

DF %>%
    # For log scaling
    mutate(MX = MX + 1) %>%
    filter(YEAR == 1980 | YEAR == 2010) %>%
    arrange(FIPS, YEAR) %>%
    group_by(FIPS) %>%
    summarize(ratio = last(MX) / first(MX)) %>%
    mutate(Growth = cut(ratio, breaks = c(-Inf, 1, 10, 100, 1000, 10000))) %>%
    ggplot(aes(fill = Growth)) +
    geom_sf(lwd = 0) +
    theme_void() +
    scale_fill_viridis_d() +
    ggtitle(
        "Change in Mexican Born Population in United States by County: 1980-2010") +
    labs(fill="")

popDF <- matrix(c(c(2000, 	 8664000, 	12204000),
         c(2010, 	11708000, 	21208000),
         c(2015, 	11508000, 	24250000),
         c(2017, 	11190000, 	25444000)),
        nrow = 4, ncol = 3, byrow = TRUE,
       dimnames = list(NULL, c("Year", "Mexican Born", "US Born"))) %>%
    as_tibble() %>%
    pivot_longer(-Year, names_to = "Mexican", values_to = "Population")

popDF %>%
    ggplot(aes(x = Year, y = Population, color = Mexican)) +
    geom_line() +
    theme_classic() +
    scale_y_continuous(
        breaks = c(10000000, 15000000, 20000000, 25000000),
        labels = c(10, 15, 20, 25)) +
    labs(color = "", y = "Population (in Millions)") +
    ggtitle("Mexican Heritage Population in the United States")
