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
    filter(State %in% c("WA", "CA", "OR"))

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

DF <- allDF %>%
    filter(AGE > 15 & AGE <= 60)

# Download from https://usa.ipums.org/usa/volii/boundaries.shtml
spDF <- read_sf("./data/ipumsCPUMA/ipums_cpuma0010.shp") %>%
    filter(State %in% c("California", "Washington", "Oregon")) %>%
    st_transform(crs="+proj=longlat +datum=WGS84")

# We want to compare the changes in age composition across nativity in the USA

(plotList$hist1 <- DF %>%
    group_by(YEAR, BPLC, `Age Group`) %>%
    summarize(Count=sum(PERWT), ageW=mean(AGE)) %>%
    mutate(Proportion=Count/sum(Count), meanAge=sum(ageW*Proportion)) %>%
    ggplot(aes(x=`Age Group`, y=Proportion)) +
    geom_col(position = "dodge") +
    facet_grid(BPLC ~ YEAR) +
    geom_vline(aes(xintercept=meanAge), color="red", linetype=2) +
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

(plotList$hist3 <- DF %>%
    mutate(weight=PERWT) %>%
    group_by(YEAR, BPLCR, `Age Group`) %>%
    summarize(Count=sum(weight), ageW=mean(AGE)) %>%
    mutate(Proportion=Count/sum(Count), meanAge=sum(ageW*Proportion)) %>%
    ggplot(aes(x=`Age Group`, y=Count)) +
    geom_col(position = "dodge") +
    facet_grid(BPLCR ~ YEAR) +
    geom_vline(aes(xintercept=meanAge), color="red", linetype=2) +
    theme_classic() +
    scale_y_continuous(labels = scales::comma))

# lets look at the change in the Mexican born population in recent years
# We dont need to change the weights as both of them are a 1% sample

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
    left_join(unique(select(DF, CPUMA0010, STATEFIP))) %>%
    left_join(select(wikiTables, Name, STATEFIP))

recentLDeltaDF <- DF %>%
    filter(YEAR %in% c(2000, 2017)) %>%
    mutate(weight=PERWT) %>%
    group_by(CPUMA0010, YEAR, BPLCR) %>%
    summarize(est=sum(weight)) %>%
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
    left_join(unique(select(DF, CPUMA0010, STATEFIP))) %>%
    left_join(select(wikiTables, Name, STATEFIP))

popGrowthDF <-  DF %>%
    filter(YEAR %in% c(2010, 2017)) %>%
    group_by(CPUMA0010, YEAR) %>%
    summarize(est=sum(PERWT)) %>%
    summarize(
        PG=diff(est),
        PGR=diff(est)/first(est),
        POP2010=first(est),
        POP2017=last(est))

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

# Mexican born pop growth
# recentDeltaDF %>% filter(BPLCR == "Mexico") %>% pull("PG") %>% sum
# -263300
plotList$hispPopGrowth <- recentDeltaDF %>%
    filter(BPLCR == "Mexico") %>%
    {right_join(spDF, ., by="CPUMA0010")} %>%
    spdf2leaf("PG", reverse=FALSE, zcenter=TRUE, label="Mexican<br>Population<br>Growth")

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

(plotList$hispUSPopGrowthStatic <- recentDeltaDF %>%
    filter(BPLCR == "Native Hispanic") %>%
    mutate(
        PGG=cut(PG, seq(-50000, 150000, 50000),
                labels=c("Pop Decline", "Small Pop Growth", "Pop Growth", "Large Pop Growth"))) %>%
    {right_join(spDF, ., by="CPUMA0010")} %>%
    ggplot() +
    geom_sf(aes(fill=PGG)) +
    theme_void() +
    scale_fill_brewer(palette = "Spectral", direction=1, drop=F) +
    labs(fill="US Born Hispanic\nPopulation\nGrowth") +
    theme(panel.grid.major = element_line(colour = 'transparent')))

# Central American born pop growth
# recentDeltaDF %>% filter(BPLCR == "Central America/Caribbean") %>% pull("PG") %>% sum
# -62700

(plotList$cenMexGrow <- recentDeltaDF %>%
    select(BPLCR, CPUMA0010, PG, Name) %>%
    spread("BPLCR", "PG") %>%
    {ggplotRegression(Mexico ~ `Central America/Caribbean`, data=.)} +
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

# Plot locations that have reached historical record highs in Mexican FB population
(plotList$mexStatePeakTable <- DF %>%
    filter(BPLC == "Mexico" & YEAR >= 2000) %>%
    mutate(weight=PERWT) %>%
    group_by(CPUMA0010, YEAR, STATEFIP) %>%
    summarize(est=sum(weight)) %>%
    group_by(CPUMA0010) %>%
    filter(est == max(est)) %>%
    ungroup() %>%
    mutate(recentPeak=YEAR==2017) %>%
    left_join(select(wikiTables, Name, STATEFIP)) %>%
    group_by(Name) %>%
    summarise(n=sum(recentPeak), propPeak=sum(recentPeak)/n()))

(plotList$mexStatePeak <- DF %>%
    filter(BPLC == "Mexico" & YEAR >= 2000) %>%
    mutate(weight=PERWT) %>%
    group_by(CPUMA0010, YEAR) %>%
    summarize(est=sum(weight)) %>%
    filter(est == max(est)) %>%
    ungroup() %>%
    mutate(recentPeak=YEAR==2017) %>%
    {right_join(spDF, .)} %>%
    ggplot() +
    geom_sf(aes(fill=recentPeak)) +
    theme_void() +
    labs(fill="Recent High of\nMexican Migrants") +
    theme(panel.grid.major = element_line(colour = 'transparent')))

saveRDS(plotList, "./results/ipumsPlot.Rds")
