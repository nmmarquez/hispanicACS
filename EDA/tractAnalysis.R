---
    output: 
    html_document:
    includes:
    in_header: assets/huskyheader.html
before_body: assets/huskynavbar.html
---
    # Changes in west Coast Population Compostion
    #### Neal Marquez
    
    
```{r setup, include=FALSE}
knitr::opts_chunk$set(
    echo=FALSE, warning=FALSE, message=FALSE)
```

```{r}
rm(list=ls())
library(tidycensus)
library(acs)
library(jsonlite)
library(tidyverse)
library(stringr)
library(DT)
library(sf)
library(plotly)
library(latex2exp)
library(tigris)
library(leaflet)
library(mapview)

pal2 <- function(x, lo=0, hi=1){
    pal <- colorNumeric(palette = "Spectral", domain=c(lo,hi), reverse=TRUE)
    pal(seq(lo, hi, length.out = x))
}

# api key needs to be your own ACS api key!
# you can get one here http://api.census.gov/data/key_signup.html
apiKey <- unlist(read_json("../keys/acs.json"))
census_api_key(apiKey)

myVars <- c(
    totalPopulation         = "B01003_001",
    whitePopulation         = "B01001H_001",
    fb1                     = "B05003I_005",
    fb2                     = "B05003I_010",
    fb3                     = "B05003I_016",
    fb4                     = "B05003I_021",
    hispanicPopulation      = "B01001I_001")

subVars <- paste0(c("hispanic", "white", "other"), "Population")
counties <- paste0("060", c(LA=73, OC=59, SD=37, SB=65, RV=71))
# what happens if I do a global environment y here?
# y <- 2010

# tract analysis
if(!file.exists("../data/caDF.Rds")){
caDF <- rbind(
    get_acs(
        geography = "tract",
        variables = myVars, geometry = TRUE,
        state="CA", year=2010) %>%
        mutate(year=2010),
    get_acs(
        geography = "tract",
        variables = myVars, geometry = TRUE,
        state="CA", year=2016) %>%
        mutate(year=2016))

    caDF %>%
        mutate(county=str_sub(GEOID, 1, 5)) %>%
        select(-moe) %>%
        spread("variable", "estimate") %>%
        mutate(otherPopulation=totalPopulation
               -whitePopulation-hispanicPopulation) %>%
        mutate(fbHispanicPopulation=fb1+fb2+fb3+fb4) %>%
        select(-fb1:-fb4) %>%
        saveRDS("../data/caDF.Rds")
}

caDF <- read_rds("../data/caDF.Rds")

# looking only out the Socal area lets take a look at some summary stats
caDF %>%
    filter(county %in% counties & year == 2016) %>%
    as_tibble() %>%
    select(-year, -geometry) %>%
    summarize_if(is.numeric, sum) 

# lets check out the hispanic population percentage across counties
caDF %>%
    filter(county %in% counties & year == 2016) %>%
    mutate(pHispanic=hispanicPopulation/totalPopulation) %>%
    mapview(zcol="pHispanic", col.regions=pal2, popup=popupTable(.))
```