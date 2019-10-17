rm(list=ls())
library(tidyverse)
library(ipumsr)
library(data.table)
library(sp)
library(sf)
library(XML)
library(httr)
library(leaflet)

plotList <- list()

ggplotRegression <- function (formula, data) {
    fit <- lm(formula, data)
    x_var <- paste0('`', names(fit$model)[2], '`')
    y_var <- paste0('`', names(fit$model)[1], '`')
    ggplot(data, aes_string(x = x_var, y = y_var)) + 
        geom_point(aes(color=Name)) +
        stat_smooth(method = "lm", col = "red") +
        geom_text(
            label = paste("R2 = ",signif(summary(fit)$r.squared, 5),
                          "Intercept =",signif(fit$coef[[1]],5 ),
                          " Slope =",signif(fit$coef[[2]], 5)),
            x=-Inf, y=Inf, hjust=0, vjust=1)
    
}

spdf2leaf <- function(df, col="data", label=NULL, reverse=TRUE, zcenter=FALSE){
    lab_label <- ifelse(is.null(label), col, label)
    cols <- grep("geometry", names(df), value = T, invert = T)
    df <- mutate(df, data = as.data.frame(df)[,col])
    
    # pop up info
    popup <- sapply(cols, function(c){
        paste0(c, ": ", as.data.frame(df)[,c])}) %>%
        apply(1, function(z) paste0(z, collapse = "<br>"))
    
    # color palette
    pal <- colorNumeric(
        palette = "Spectral", 
        domain=as.data.frame(df)$data, 
        reverse=reverse)
    if(zcenter){
        pal <- colorNumeric(
            palette = "Spectral", 
            domain=c(
                -max(abs(as.data.frame(df)$data)),
                max(abs(as.data.frame(df)$data))),
            reverse=reverse)
    }
    
    
    # see map
    map1<-leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data=df, fillColor=~pal(data), color="#b2aeae", weight=0.3,
                    fillOpacity=0.7, smoothFactor=0.2, popup=popup) %>%
        addLegend("bottomright", pal=pal, values=as.data.frame(df)$data,
                  title = lab_label, opacity = 1)
    map1
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
    filter(grepl("State;", `Status\n`))

allDF <- as_tibble(fread("./data/usa_00004.csv")) %>%
    filter(STATEFIP %in% wikiTables$STATEFIP) %>%
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

# Download from https://usa.ipums.org/usa/volii/boundaries.shtml
spComplexDF <- read_sf("./data/ipumsCPUMA/ipums_cpuma0010.shp") %>%
    filter(!(State %in% c("Puerto Rico", "Alaska", "Hawaii")))

spDF <- spComplexDF %>%
    as('Spatial') %>%
    rgeos::gSimplify(tol=1100) %>%
    st_as_sf() %>%
    st_transform(crs="+proj=longlat +datum=WGS84") %>%
    mutate(CPUMA0010 = spComplexDF$CPUMA0010)

plotList$geoMX <- allDF %>%
    filter(YEAR == 2017) %>%
    group_by(CPUMA0010) %>%
    summarize(Pop=sum(PERWT * (BPLCR == "Mexico"))) %>%
    mutate(pPop=Pop/sum(Pop)) %>%
    {left_join(spDF, ., by="CPUMA0010")} %>%
    spdf2leaf(col = "pPop", label = "Population<br>Distribution<br>2017")

plotList$geoAgeMX <- allDF %>%
    filter(BPLCR == "Mexico" & YEAR == 2017) %>%
    group_by(CPUMA0010) %>%
    summarize(Age=sum(PERWT*AGE)/sum(PERWT)) %>%
    {left_join(spDF, ., by="CPUMA0010")} %>%
    spdf2leaf(col = "Age", label = "Population Age<br>Geographies")

plotList$geoDeltaPMX <- allDF %>%
    filter(YEAR == 2017 | YEAR == 2000) %>%
    group_by(YEAR, CPUMA0010) %>%
    summarize(Pop=sum(PERWT * (BPLCR == "Mexico"))) %>%
    mutate(pPop=Pop/sum(Pop)) %>%
    group_by(CPUMA0010) %>%
    arrange(CPUMA0010, YEAR) %>%
    summarize(deltapPop=diff(pPop)) %>%
    {left_join(spDF, ., by="CPUMA0010")} %>%
    spdf2leaf(col = "deltapPop", label = "Population<br>Change<br>2000 to 2017")

plotList$geoDeltaMX <- allDF %>%
    filter(YEAR == 2017 | YEAR == 2010) %>%
    group_by(YEAR, CPUMA0010) %>%
    summarize(Pop=sum(PERWT * (BPLCR == "Mexico"))) %>%
    group_by(CPUMA0010) %>%
    arrange(CPUMA0010, YEAR) %>%
    summarize(deltapPop=diff(Pop)) %>%
    {left_join(spDF, ., by="CPUMA0010")} %>%
    spdf2leaf(col = "deltapPop", label = "Population<br>Change<br>2000 to 2017")

plotList$ageByDist <- allDF %>%
    filter(YEAR == 2017 | YEAR == 2000) %>%
    group_by(YEAR, CPUMA0010) %>%
    summarize(Pop=sum(PERWT * (BPLCR == "Mexico"))) %>%
    mutate(pPop=Pop/sum(Pop)) %>%
    group_by(CPUMA0010) %>%
    arrange(CPUMA0010, YEAR) %>%
    summarize(deltaPop=diff(Pop)) %>%
    left_join(
        allDF %>%
            filter(BPLCR == "Mexico" & YEAR == 2017) %>%
            group_by(CPUMA0010) %>%
            summarize(Age=sum(PERWT*AGE)/sum(PERWT))
    ) %>%
    ggplot(aes(x=deltaPop, y=Age, text=CPUMA0010)) +
    geom_point() +
    geom_smooth() +
    theme_classic()

plotList$geoCACA <- allDF %>%
    filter(BPLCR == "Central America/Caribbean" & YEAR == 2017) %>%
    group_by(CPUMA0010) %>%
    summarize(Pop=sum(PERWT)) %>%
    mutate(pPop=Pop/sum(Pop)) %>%
    {left_join(spDF, ., by="CPUMA0010")} %>%
    spdf2leaf(col = "pPop", label = "Population\nDistribution")

plotList$geoAgeCACA <- allDF %>%
    filter(BPLCR == "Central America/Caribbean" & YEAR == 2017) %>%
    group_by(CPUMA0010) %>%
    summarize(Age=sum(PERWT*AGE)/sum(PERWT)) %>%
    {left_join(spDF, ., by="CPUMA0010")} %>%
    spdf2leaf(col = "Age", label = "Population\nDistribution")

DF <- allDF %>%
    filter(AGE > 15 & AGE <= 60)

recentDeltaDF <- DF %>%
    filter(YEAR %in% c(2010, 2017)) %>%
    mutate(weight=PERWT) %>%
    group_by(CPUMA0010, YEAR, BPLCR) %>%
    summarize(est=sum(weight), age=sum(weight*AGE)/sum(weight)) %>%
    ungroup %>%
    ## TODO: Figure out a better way to do this
    right_join(as_tibble(expand.grid(
        BPLCR=unique(.$BPLCR),
        CPUMA0010=unique(.$CPUMA0010), 
        YEAR=unique(.$YEAR)))) %>%
    mutate(est=ifelse(is.na(est), 0, est)) %>%
    arrange(BPLCR, CPUMA0010, YEAR) %>%
    group_by(BPLCR, CPUMA0010) %>%
    summarize(
        PG=diff(est),
        PGR=diff(est)/first(est),
        POP2010=first(est),
        POP2017=last(est),
        AGEDIFF=diff(age)) %>%
    ungroup %>%
    left_join(unique(select(DF, CPUMA0010, STATEFIP))) %>%
    left_join(select(wikiTables, Name, STATEFIP))

plotList$PGandAge <- recentDeltaDF %>%
    filter(BPLCR == "Mexico") %>%
    ggplot(aes(x=PG, y=AGEDIFF)) +
    geom_point(aes(color=Name)) +
    theme_classic() +
    geom_smooth()

(plotList$hist1 <- allDF %>%
        group_by(YEAR, BPLC, `Age Group`) %>%
        summarize(Count=sum(PERWT), ageW=mean(AGE)) %>%
        mutate(Proportion=Count/sum(Count), meanAge=sum(ageW*Proportion)) %>%
        ggplot(aes(x=`Age Group`, y=Proportion)) +
        geom_col(position = "dodge") +
        facet_grid(BPLC ~ YEAR) +
        geom_vline(aes(xintercept=meanAge), color="red", linetype=2) +
        theme_classic())

(plotList$chicMexGrow <- recentDeltaDF %>%
        select(BPLCR, CPUMA0010, PG, Name) %>%
        spread("BPLCR", "PG") %>%
        {ggplotRegression(Mexico ~ `Native Hispanic`, data=.)} +
        theme_classic())

(plotList$otherMexGrow <- recentDeltaDF %>%
        select(BPLCR, CPUMA0010, PG, Name) %>%
        spread("BPLCR", "PG") %>%
        {ggplotRegression(Mexico ~ `Native Non-Hispanic`, data=.)} +
        theme_classic())

ggplotly(plotList$chicMexGrow)

recentDeltaDF %>%
    filter(Name %in% c("California", "Washington", "Oregon")) %>%
    select(BPLCR, CPUMA0010, PG, Name) %>%
    spread("BPLCR", "PG") %>%
    {ggplotRegression(Mexico ~ `Native Hispanic`, data=.)} +
    theme_classic()

recentDeltaDF %>%
    filter(BPLCR == "Mexico") %>%
    ggplot(aes(x=PG)) +
    geom_histogram() +
    theme_classic()

recentDeltaDF %>%
    filter(BPLCR == "Mexico") %>%
    ggplot(aes(x=AGEDIFF)) +
    geom_histogram() +
    theme_classic() +
    geom_vline(xintercept = 7, color = "red", linetype = 2)
