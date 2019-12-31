rm(list=ls())
library(tidyverse)
library(ipumsr)
library(data.table)
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
    filter(grepl("State;", `Status\n`) | (State == "DC")) %>%
    filter(State != "AK" & State != "HI")
    #filter(State %in% c("WA", "CA", "OR"))

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

allDF <- as_tibble(fread("./data/usa_00004.csv")) %>%
    filter(STATEFIP %in% wikiTables$STATEFIP) %>%
    filter(YEAR >=2010) %>%
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
    mutate(`Age Group`=(cut_interval(AGE, length=5, labels=F)-1)*5) %>%
    left_join(stateRegions, by = "STATEFIP")

spDF <- read_sf("./data/ipumsCPUMA/ipums_cpuma0010.shp") %>%
    filter(!(State %in% c("Alaska", "Hawaii"))) %>%
    st_transform(crs="+proj=longlat +datum=WGS84")

DF <- allDF %>%
    filter(AGE > 15 & AGE <= 65)

(plotList$hist1 <- DF %>%
        filter(BPLCR == "Native Non-Hispanic" | BPLCR == "Mexico") %>%
        group_by(YEAR, BPLCR, Cluster, `Age Group`) %>%
        summarize(Count=sum(PERWT), ageW=weighted.mean(AGE, PERWT)) %>%
        mutate(Proportion=Count/sum(Count), meanAge=sum(ageW*Proportion)) %>%
        ggplot(aes(x=`Age Group`, y=Proportion, fill=BPLCR, color=BPLCR)) +
        geom_col(position = "dodge") +
        geom_vline(aes(xintercept=meanAge, color=BPLCR), linetype=2) +
        facet_grid(Cluster ~ YEAR) +
        theme_classic())

(plotList$hist2 <- DF %>%
        group_by(YEAR, BPLCR, `Age Group`) %>%
        summarize(Count=sum(PERWT), ageW=mean(AGE)) %>%
        mutate(Proportion=Count/sum(Count), meanAge=sum(ageW*Proportion)) %>%
        ggplot(aes(x=`Age Group`, y=Proportion)) +
        geom_col(position = "dodge") +
        facet_grid(BPLCR ~ YEAR) +
        geom_vline(aes(xintercept=meanAge), color="red", linetype=2) +
        theme_classic())

recentDeltaDF <- DF %>%
    filter(YEAR %in% c(2010, 2017)) %>%
    group_by(CPUMA0010, YEAR, BPLCR) %>%
    summarize(est=sum(PERWT)) %>%
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
        POP2017=last(est)) %>%
    ungroup %>%
    left_join(unique(select(DF, CPUMA0010, STATEFIP, Cluster))) %>%
    left_join(select(wikiTables, Name, STATEFIP))


recentDeltaDF %>%
    group_by(CPUMA0010) %>%
    mutate(TOTPOP2017 = sum(POP2017)) %>%
    ungroup() %>%
    filter(BPLCR == "Mexico" | BPLCR == "Native Hispanic") %>%
    ggplot(aes(x=POP2010, y=PGR, group=BPLCR, color=BPLCR)) +
    scale_x_log10() +
    geom_point(alpha=.6) +
    theme_classic() +
    geom_smooth(aes(fill=BPLCR)) +
    labs(x="2010 Population", y="Population Growth to 2017")

recentDeltaDF %>%
    group_by(CPUMA0010) %>%
    mutate(TOTPOP2017 = sum(POP2017)) %>%
    ungroup() %>%
    filter(BPLCR == "Mexico") %>%
    ggplot(aes(x=POP2010, y=PGR)) +
    scale_x_log10() +
    geom_point(aes(color=Cluster), alpha=.6) +
    theme_classic() +
    geom_smooth()

recentDeltaDF %>%
    ungroup() %>%
    filter(BPLCR == "Mexico") %>%
    filter(PG > 0) %>%
    pull(CPUMA0010) %>%
    {mutate(DF, GR=CPUMA0010 %in% .)} %>%
    filter(YEAR == 2017) %>%
    filter(BPLCR == "Mexico") %>%
    group_by(GR, `Age Group`) %>%
    summarize(Count=sum(PERWT), ageW=weighted.mean(AGE, PERWT)) %>%
    mutate(Proportion=Count/sum(Count), meanAge=sum(ageW*Proportion)) %>%
    ggplot(aes(x=`Age Group`, y=Proportion, fill=GR, color=GR)) +
    geom_col(position = "dodge") +
    geom_vline(aes(xintercept=meanAge, color=GR), linetype=2) +
    theme_classic() +
    labs(fill="Growing\nPopultion", color="Growing\nPopultion") +
    ggtitle("Difference in Mexican Age Sctruture for Growing and Declining Populations")

DF %>%
    filter(grepl("Hispanic", BPLCR) | BPLCR == "Mexico") %>%
    group_by(BPLCR, `Age Group`) %>%
    summarize(Count=sum(PERWT), ageW=weighted.mean(AGE, PERWT)) %>%
    mutate(Proportion=Count/sum(Count), meanAge=sum(ageW*Proportion)) %>%
    ggplot(aes(x=`Age Group`, y=Proportion, fill=BPLCR, color=BPLCR)) +
    geom_col(position = "dodge") +
    geom_vline(aes(xintercept=meanAge, color=BPLCR), linetype=2) +
    theme_classic() +
    ggtitle("Age Distribution")

DF %>%
    mutate(didMig = MIGCOUNTY1 != 0) %>%
    filter(didMig) %>%
    filter(grepl("Hispanic", BPLCR) | BPLCR == "Mexico") %>%
    group_by(BPLCR, `Age Group`) %>%
    summarize(Count=sum(PERWT), ageW=weighted.mean(AGE, PERWT)) %>%
    mutate(Proportion=Count/sum(Count), meanAge=sum(ageW*Proportion)) %>%
    ggplot(aes(x=`Age Group`, y=Proportion, fill=BPLCR, color=BPLCR)) +
    geom_col(position = "dodge") +
    geom_vline(aes(xintercept=meanAge, color=BPLCR), linetype=2) +
    theme_classic() +
    ggtitle("Age Distribution of Individuals who Migrated within Past Year")

DF %>%
    filter(BPLCR == "Mexico") %>%
    group_by(Cluster, YEAR) %>%
    summarize(N = sum(PERWT)) %>%
    summarise(diff(N))


recentDeltaDF %>%
    group_by(CPUMA0010) %>%
    mutate(TOTPOP2017 = sum(POP2017))


popGrowthDF <-  DF %>%
    filter(YEAR %in% c(2010, 2017)) %>%
    group_by(CPUMA0010, YEAR) %>%
    summarize(est=sum(PERWT)) %>%
    summarize(
        PG=diff(est),
        PGR=diff(est)/first(est),
        POP2010=first(est),
        POP2017=last(est))

DF %>%
    group_by(YEAR, BPLCR) %>%
    summarize(Prop=sum(PERWT*(MIGCOUNTY1!=0)) / sum(PERWT)) %>%
    filter(grepl("Hispanic", BPLCR) | BPLCR == "Mexico") %>%
    summarize(PrMig=mean(Prop))
    pull(PrMig)


# Total pop growth 
# 2680100
plotList$totalPopGrowth <- popGrowthDF %>%
    {right_join(spDF, ., by="CPUMA0010")} %>%
    spdf2leaf("PG", reverse=FALSE, zcenter=TRUE, label="Population<br>Growth")

(plotList$totalPopGrowthStatic <- popGrowthDF %>%
        mutate(
            PGG=cut(PG, seq(-50000, 150000, 50000),
                    labels=c("Pop Decline", "Small Pop Growth", "Pop Growth", "Large Pop Growth"))) %>%
        {right_join(spDF, ., by="CPUMA0010")} %>%
        ggplot() +
        geom_sf(aes(fill=PGG)) +
        theme_void() +
        scale_fill_brewer(palette = "Spectral", direction = 1, drop=F) +
        labs(fill="Population\nGrowth") +
        theme(panel.grid.major = element_line(colour = 'transparent')))

(plotList$hispPopGrowthStatic <- recentDeltaDF %>%
        filter(BPLCR == "Mexico") %>%
        mutate(
            PGG=cut(PG, seq(-50000, 150000, 50000),
                    labels=c("Pop Decline", "Small Pop Growth", "Pop Growth", "Large Pop Growth"))) %>%
        {right_join(spDF, ., by="CPUMA0010")} %>%
        ggplot() +
        geom_sf(aes(fill=PGG)) +
        theme_void() +
        scale_fill_brewer(palette = "Spectral", direction=1, drop=F) +
        labs(fill="Mexican Born\nPopulation\nGrowth") +
        theme(panel.grid.major = element_line(colour = 'transparent')))
