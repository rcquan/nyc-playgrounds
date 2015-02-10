#############################
# Ryan Quan
# GitHub Username: rcquan
# Twitter: @ryancquan
# Email: ryan.quan08@gmail.com
#
# The following code loads the NYC Playgrounds Data and
# shapefiles 
#############################

library(XML)
library(xlsx)
library(rgdal)
library(rgeos)
library(maptools)
library(ggplot2)
library(dplyr)
library(sp)
library(RColorBrewer)
library(stringr)

##################################
## Functions
##################################
stripLeadingZeros <- function(numericString) {
    gsub("(?<![0-9])0+", "", numericString, perl = TRUE)
}

isPark <- function(string) {
    str_detect(string, "park")
}

discretize <- function(num) {
    if (is.na(num)) {
        return("Park")
    } else if (num >= 0 & num < 0.5) {
        return("0.0 - 0.5")
    } else if (num >= 0.5 & num < 1) {
        return("0.5 - 1.0") 
    } else if (num >= 1 & num < 1.5) {
        return("1.0 - 1.5")
    } else if (num >= 1.5 & num < 2.0) {
        return("1.5 - 2.0") 
    } else if (num >= 2.0) {
        return("2.0 or greater")
    }
}

##################################
## Download Source Data
##################################
dir.create("data")
download.file("http://catalog.opendata.city/dataset/dd98ccda-d996-4080-87aa-ef8e4fd1f167/resource/9dd0cf1e-1513-43bc-aeda-00ceef520f2f/download/CSVMedianHouseholdIncomeCensusTract.CSV",
              "data/CSVMedianHouseholdIncomeCensusTract.CSV")
download.file("http://www.nyc.gov/html/dcp/download/census/nyc2010census_tabulation_equiv.xlsx",
              "data/nyc2010census_tabulation_equiv.xlsx")
download.file("http://www.nyc.gov/html/dcp/download/census/census2010/t_sf1_p2_nta.xlsx",
              "data/t_sf1_p2_nta.xlsx")
download.file("http://www.nyc.gov/html/dcp/download/bytes/nynta_14d.zip",
              "data/nynta_14d.zip")
unzip("data/nynta_14d.zip", exdir="data")
##################################
## Coordinate Reference System
##################################
proj4string <- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83")

##################################
## Median Household Income by NTA
##################################
census <- read.csv("data/CSVMedianHouseholdIncomeCensusTract.CSV", stringsAsFactors=FALSE)
censusToNTA <- read.xlsx("data/nyc2010census_tabulation_equiv.xlsx",
                       sheetIndex=1, startRow=6, header=FALSE)
names(censusToNTA) <- c("borough", "COUNTYFP10", "boroughCode", "TRACTCE10", "puma", "NTACode", "name")
## strip leading zeros to prep keys for merging
censusToNTA$COUNTYFP10 <- stripLeadingZeros(censusToNTA$COUNTYFP10)
censusToNTA$TRACTCE10 <- stripLeadingZeros(censusToNTA$TRACTCE10)
## get median household income by nta
ntaMHI <- merge(census, censusToNTA, by=c("COUNTYFP10", "TRACTCE10")) %>%
    group_by(NTACode) %>%
    summarize(ntaMHI = mean(MHI))

##################################
## Children Pop (Ages 0-9) by NTA
##################################
ntaPopulation <- read.xlsx("data/t_sf1_p2_nta.xlsx", 
                     sheetIndex=1, startRow=9, header=FALSE)
ntaPopulation <- ntaPopulation[, c(1, 3:5, 8:12)]
names(ntaPopulation) <- c("BoroName", "NTACode","NTAName", "totalPop", 
                          "Age3to4", "Age5", "Age6", "Age7to9", "Age10to11")

##################################
## Number of Playgrounds by NTA
##################################
doc <- xmlParse("http://www.nycgovparks.org/bigapps/DPR_Playgrounds_001.xml")
playgrounds <- xmlToDataFrame(doc, stringsAsFactors = FALSE)
playgrounds$lat <- as.numeric(playgrounds$lat)
playgrounds$lon <- as.numeric(playgrounds$lon)
playgrounds <- na.omit(playgrounds)
playgroundsSP <- SpatialPointsDataFrame(coords=playgrounds[, c("lon", "lat")],
                                        data=playgrounds,
                                        proj4string=proj4string)

## Spatial Join
nta <- readOGR("data/nynta_14d/", layer="nynta")
nta <- spTransform(nta, proj4string)
ntaPlaygrounds <- playgroundsSP %>%
    over(nta) %>%
    group_by(NTACode) %>%
    summarise(playgrounds = n())

##################################    
## Creating the Feature Set
##################################
ntaData <- ntaMHI %>%
    merge(ntaPopulation, by="NTACode") %>%
    merge(ntaPlaygrounds, by="NTACode", all=TRUE) %>%
    mutate(totalChildren = Age3to4 + Age5 + Age6 + Age7to9 + Age10to11,
           percentChildren = totalChildren / totalPop,
           playgroundsPer1KChildren = 1000 * (playgrounds / totalChildren)) %>%
    mutate(playgroundsPer1KChildren = ifelse(is.na(playgroundsPer1KChildren),
                                             0, playgroundsPer1KChildren),
           playgroundsPer1KChildren = ifelse(is.infinite(playgroundsPer1KChildren),
                                             0, playgroundsPer1KChildren),
           playgroundsPer1KChildren = ifelse(isPark(NTAName), 
                                             NA, playgroundsPer1KChildren)) %>%
    mutate(playgroundsPer1KResidents = (playgroundsPer1KChildren * totalChildren) / totalPop)

    
ntaData$discretePlaygrounds <- sapply(ntaData$playgroundsPer1KChildren, discretize)
ntaData$discreteResidents <- sapply(ntaData$playgroundsPer1KResidents, discretize)

##################################    
## GIS
##################################
## turn spatial data into data.frame for plotting
ntaGeom <- fortify(nta, region = "NTACode")
# find overlap of points and polygon
ntaGeom <- merge(ntaGeom, ntaData, by.x="id", by.y="NTACode")

##################################    
## Plotting (MAPS)
##################################
blank_theme <- theme(panel.background=element_blank(),
                     legend.position=c(0.20, 0.75),
                     legend.background=element_rect(colour="black"),
                     axis.ticks = element_blank(),
                     axis.text.x = element_blank(),
                     axis.text.y = element_blank(),
                     panel.grid.minor=element_blank(),
                     panel.grid.major=element_blank(),
                     axis.title.x = element_blank(),
                     axis.title.y = element_blank(),
                     panel.border = element_blank())

## Playgrounds Per 1K Residents, Continuous Scale
ggplot() +
    geom_map(data=ntaData, aes(map_id=NTACode, fill=playgroundsPer1KResidents), map=ntaGeom) +
    geom_path(data=ntaGeom, aes(x=long, y=lat, group=group), colour="black", size=0.25) +
    coord_map(projection="mercator") + 
    scale_fill_gradientn(name="Playgrounds per\n1K Residents", 
                         colours=brewer.pal(9, "Greens")) +
    blank_theme

## Playgrounds Per 1K Children, Continuous Scale
ggplot() +
    geom_map(data=ntaData, aes(map_id=NTACode, fill=playgroundsPer1KChildren), map=ntaGeom) +
    geom_path(data=ntaGeom, aes(x=long, y=lat, group=group), colour="black", size=0.25) +
    coord_map(projection="mercator") + 
    scale_fill_gradientn(name="Playgrounds per\n1K Children,\nAges 3-11", 
                         colours=brewer.pal(9, "Greens")) +
    blank_theme


## Playgrounds Per 1K Children, Discrete Scale
cols <- c(brewer.pal(5, "Greens"), "grey50")
names(cols) <- c("0.0 - 0.5", "0.5 - 1.0", "1.0 - 1.5", 
                 "1.5 - 2.0", "2.0 or greater", "Park")

ggplot() +
    geom_map(data=ntaData, aes(map_id=NTACode, fill=discretePlaygrounds), map=ntaGeom) +
    geom_path(data=ntaGeom, aes(x=long, y=lat, group=group), colour="black", size=0.25) +
    coord_map(projection="mercator") + 
    scale_fill_manual(name="Playgrounds per\n1K Children, Ages 3-11", values=cols) +
    blank_theme

## Total Children Population
ggplot() +
    geom_map(data=ntaData, aes(map_id=NTACode, fill=totalChildren), map=ntaGeom) +
    geom_path(data=ntaGeom, aes(x=long, y=lat, group=group), colour="black", size=0.25) +
    coord_map(projection="mercator") + 
    scale_fill_gradientn(name="Est. Population of\nChildren, Ages 3-11",
                         colours=brewer.pal(9, "Blues")) +
    blank_theme

## Median Household Income
ggplot() +
    geom_map(data=ntaData, aes(map_id=NTACode, fill=ntaMHI), map=ntaGeom) +
    geom_path(data=ntaGeom, aes(x=long, y=lat, group=group), colour="black", size=0.25) +
    coord_map(projection="mercator") + 
    scale_fill_gradientn(name="Median Household\nIncome, Dollars", colours=brewer.pal(9, "Blues")) +
    blank_theme

##################################    
## Plotting (TABLES)
##################################

mostPlaygrounds <- ntaData %>%
    select(NTAName, playgroundsPer1KChildren) %>%
    arrange(desc(playgroundsPer1KChildren)) %>%
    head(20)
mostPlaygrounds

leastPlaygrounds <- ntaData %>%
    select(NTAName, playgroundsPer1KChildren) %>%
    arrange(playgroundsPer1KChildren) %>%
    filter(playgroundsPer1KChildren != 0) %>%
    head(20)
leastPlaygrounds

## most children
ntaData %>%
    select(NTAName, totalChildren) %>%
    arrange(desc(totalChildren)) %>%
    head(10)

## highest MHI
ntaData %>%
    select(NTAName, ntaMHI) %>%
    arrange(desc(ntaMHI)) %>%
    head(10)

##################################    
## Plotting (CHARTS)
##################################

## Children vs. Playgrounds
ggplot(ntaData, aes(totalChildren, playgrounds)) +
    geom_point(size=3, alpha=0.75) +
    ylab("Playgrounds per 1K Children") +
    xlab("Population of Children, Ages 3-11") +
    theme_bw()

## MHI versus Playgrounds
ggplot(ntaData, aes(ntaMHI, playgroundsPer1KChildren)) +
    geom_point() +
    ylim(0, 5) + ylab("Playgrounds per 1K Children") +
    xlim(0, 175000) + xlab("Median Household Income") +
    theme_bw()

ntaData %>%
    group_by(BoroName) %>%
    summarize(cor = cor(ntaMHI, playgroundsPer1KChildren, use="complete.obs"))

## Children versus MHI
ggplot(ntaData, aes(ntaMHI, totalChildren)) +
    geom_point() +
    theme_bw()

## Most Playgrounds
ggplot(mostPlaygrounds, aes(reorder(NTAName, playgroundsPer1KChildren), 
                            playgroundsPer1KChildren)) + 
    geom_point(stat = "identity", size=3) + 
    xlab("NYC Neighborhood Tabulation Areas (NTAs)") +
    ylab("Playgrounds per 1K Children") +
    ylim(0, 6) +
    coord_flip() +
    theme_bw()

## Least Playgrounds
ggplot(leastPlaygrounds, aes(reorder(NTAName, -playgroundsPer1KChildren),
                             playgroundsPer1KChildren)) + 
    geom_point(stat = "identity", size=3) + 
    xlab("NYC Neighborhood Tabulation Areas (NTAs)") +
    ylab("Playgrounds per 1K Children") +
    ylim(0, 6) +
    coord_flip() +
    theme_bw()
