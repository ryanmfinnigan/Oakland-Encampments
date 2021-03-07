# Ryan Finnigan
# ryanmfinnigan@gmail.com
# April 2020

# This file imports and formats data on tent encampments in Oakland, CA. 
# The data were coded from Street View images in Google Maps, as described in my article:
# 
#   Ryan Finnigan. 2021. "The Growth and Shifting Spatial Distribution of Tent Encampments in Oakland, California."
#   The ANNALS of the American Academy of Political and Social Science
# 
# This file does the following: 
#   1. imports the data from a series of Excel spreadsheets, each corresponding to a census tract
#   2. transforms the data to use harmonized time periods
#   3. linearly interpolates missing values
#   4. imports spatial data for mapping tent encampments


### PREAMBLE
setwd("/Users/ryanfinnigan/Box Sync/encampments/google map data")
library(tidyverse)
library(readxl)
library(dplyr)
library(tigris)
library(sp)
library(rgdal)


### 1. IMPORTING FROM EXCEL FILES

# list of census tracts
tract.list <- c(4010, 4011, 4012, 4013, 4014, 4015, 4016, 4017, 4018, 4022, 4024, 4025, 4026, 4027, 4028, 4029, 4030, 4031, 4105, 9819, 9820, 9832)

# empty data frame for merged raw data
raw.data <- tibble()

# importing and appending each tract-specific Excel file
for (i in tract.list) {
 tract.file <- tibble(read_excel(paste0("tract", i, ".xlsx")))
 tract.file <- mutate(tract.file, tract = i)
 raw.data <- rbind(raw.data, tract.file)
 print(paste("tract", i, "merged"))
}
rm(tract.file)

# renaming variables
raw.data <- rename(raw.data, encampment.number = `Encampment number`)
raw.data <- rename(raw.data, maps.address = `Maps address`)
raw.data <- rename(raw.data, latitude = Latitude)
raw.data <- rename(raw.data, longitude = Longitude)
raw.data <- rename(raw.data, month = `Photo Month`)
raw.data <- rename(raw.data, year = `Photo Year`)
raw.data <- rename(raw.data, tents = `Number of Tents`)
raw.data <- rename(raw.data, rv = `Number of RVs`)
raw.data <- rename(raw.data, piles = `Piles of Belongings`)
raw.data <- rename(raw.data, obstructed = `Obstructed View`)

# creating unique identifier for each encampment
raw.data <- mutate(raw.data, id = 100*tract + encampment.number)

# single continuous time variable
raw.data <- mutate(raw.data, time = year + (month-.5)/12)

# reordering columns
var.order <- c("id", "tract", "encampment.number", "maps.address", "latitude", "longitude", "year", "month", "time", "tents", "rv", "piles", "obstructed")

raw.data <- relocate(raw.data, id, .before = encampment.number)
for (i in 2:length(var.order)) {
  raw.data <- relocate(raw.data, var.order[i], .after = var.order[i-1])
}

# subsetting columns
raw.data <- select(raw.data, all_of(var.order))
rm(i, tract.list, var.order)

# saving raw data
saveRDS(raw.data, file = "raw_encampment_data.rds")


### 2. HARMONIZED TIME PERIODS

# frequency table of observations by year and month
table(raw.data$month, raw.data$year)

# new data frame with observations for harmonized time periods, removing observations with obstructed views
trend.data <- filter(raw.data, (obstructed!=1 | is.na(obstructed)))

# new variable for harmonized time periods
trend.data <- mutate(trend.data, time.period = NA)
trend.data <- relocate(trend.data, time.period, .after = time)

# few observations in 2007

# almost all observed in early 2008
trend.data$time.period[trend.data$year==2008] <- 2008.3 

# very few observed in 2009--skipping
# no observations in 2010

# many viewed in early and late 2011
trend.data$time.period[trend.data$year==2011 & trend.data$month<=6] <- 2011.3
trend.data$time.period[trend.data$year==2011 & trend.data$month>=7] <- 2011.8

# no observations in 2012 and very few in 2013

# many viewed in Jan 2014 and around half in second half of 2014
trend.data$time.period[trend.data$year==2014 & trend.data$month==1] <- 2014.1 
trend.data$time.period[trend.data$year==2014 & trend.data$month>=6] <- 2014.6

# many observations throughout 2015
trend.data$time.period[trend.data$year==2015 & trend.data$month<=4] <- 2015.2
trend.data$time.period[trend.data$year==2015 & (trend.data$month>=5 & trend.data$month<=8)] <- 2015.5
trend.data$time.period[trend.data$year==2015 & trend.data$month>=9] <- 2015.9

# many observations throughout 2016
trend.data$time.period[trend.data$year==2016 & trend.data$month<=4] <- 2016.2
trend.data$time.period[trend.data$year==2016 & (trend.data$month>=5 & trend.data$month<=8)] <- 2016.5
trend.data$time.period[trend.data$year==2016 & trend.data$month>=9] <- 2016.9

# relatively few observations throughtout 2017
trend.data$time.period[trend.data$year==2017] <- 2017.5

# all observations in 2018 are in Jan or April
trend.data$time.period[trend.data$year==2018] <- 2018.2

# 2019 observations were mostly in February, with some in Mar-June (many repeats from Feb)
trend.data$time.period[trend.data$year==2019] <- 2019.2

# removing observations from time periods with very few observations
trend.data <- filter(trend.data, is.na(time.period)==FALSE)

## keeping only one obs per encampment in each time period

# calculating average time across observations within each period
time.avg <- summarize(group_by(trend.data, time.period), time.mean = mean(time))
# merging average time into trend.data
trend.data <- left_join(trend.data, time.avg)
rm(time.avg)

# time difference for each observation relative to the period average
trend.data <- mutate(trend.data, time.diff = time - time.mean)

# retaining only months closest to average within each harmonized time point
time.closest <- summarize(group_by(trend.data, time.period, id), time.min = abs(min(time.diff)), .groups = "keep")
# merging min time into trend.data
trend.data <- left_join(trend.data, time.closest)
rm(time.closest)
# keeping only observations closest to average time within each harmonized time point
trend.data <- filter(trend.data, abs(time.diff)==time.min)

# removing unneeded variables
trend.data$obstructed <- NULL
trend.data$time.mean <- NULL
trend.data$time.diff <- NULL
trend.data$time.min <- NULL


### 3. LINEARLY INTERPOLATING MISSING TIME PERIODS

# rows for all time periods
imp.data <- expand(trend.data, id, time.period)
imp.data <-left_join(imp.data, trend.data)

# creating tents and RVs variables for interpolated values
imp.data <- mutate(imp.data, tentsi = tents, rvi = rv)

# sorting by encampment and time
imp.data <- arrange(imp.data, id, time.period)

# interpolating missing numbers of tents and RVs using observations before and after
# looping over successive and preceding observations
# interpolations start with immediately preceding and following observations then move forward and back
for (i in 1:12) {
for (j in 1:12) {
  # placeholder variable a for interpolated tents
  imp.data <- imp.data %>% group_by(id) %>% mutate(a = (lead(tents, i)-lag(tents, j))/(lead(time, i)-lag(time, j)))
  # replacing missing values for tentsi with interpolated values
  imp.data$tentsi[is.na(imp.data$tentsi)] <- imp.data$a[is.na(imp.data$tentsi)]
  #resetting placeholder variable a
  imp.data$a <- NULL
  
  # placeholder variable b for interpolated RVs
  imp.data <- imp.data %>% group_by(id) %>% mutate(b = (lead(rv, i)-lag(rv, j))/(lead(time, i)-lag(time, j)))
  # replacing missing values for rvi with interpolated values
  imp.data$rvi[is.na(imp.data$rvi)] <- imp.data$b[is.na(imp.data$rvi)]
  #resetting placeholder variable b
  imp.data$b <- NULL
}
}
rm(i, j)
# rounding to whole numbers
imp.data$tentsi <- round(imp.data$tentsi, 0)
imp.data$rvi <- round(imp.data$rvi, 0)

## overly complicated "by hand" interpolations without using "group_by"

# linearly imputing tent count with waves prior and after
# if: tents missing in row i, but not i-1 or i+1
# if: id is the same in rows i-1 and i+1
# further looping using rows further forward and back if i-1 and/or i+1 are also missing
# rounding to whole number at the end
# for (i in 2:1231) {
# for (j in 1:10) {
# for (k in 1:10) {
#   if  ((i-k)>0 & (i+j)<=1232) {
#   if (is.na(imp.data$tentsi[i]) & is.na(imp.data$tents[i-k])==FALSE & is.na(imp.data$tents[i+j])==FALSE & imp.data$id[i-k]==imp.data$id[i+j]) {
#     imp.data$tentsi[i] <- (imp.data$tents[i+j]-imp.data$tents[i-k])/(imp.data$time[i+j]-imp.data$time[i-k])
#   }
#   }
# }
# }
# }
# imp.data$tentsi <- round(imp.data$tentsi, 0)
# rm(i, j, k)

# linearly imputing RV count with waves prior and after
# if: RV missing in row i, but not i-1 or i+1
# if: id is the same in rows i-1 and i+1
# further looping using rows further forward and back if i-1 and/or i+1 are also missing
# rounding to whole number at the end
# for (i in 2:1231) {
# for (j in 1:13) {
# for (k in 1:13) {
#   if  ((i-k)>0 & (i+j)<=1232) {
#   if (is.na(imp.data$rvi[i]) & is.na(imp.data$rv[i-k])==FALSE & is.na(imp.data$rv[i+j])==FALSE & imp.data$id[i-k]==imp.data$id[i+j]) {
#     imp.data$rvi[i] <- (imp.data$rv[i+j]-imp.data$rv[i-k])/(imp.data$time[i+j]-imp.data$time[i-k])
#   }
#   }
# }
# }
# }
# imp.data$rvi <- round(imp.data$rvi, 0)
# rm(i, j, k)

## end overly complicated interpolation

## filling in tract number for interpolated obs

# getting tract numbers
tract.numbers <- raw.data %>%
  group_by(id) %>%
  summarize(tract.num = first(tract))
# merging with data
imp.data <- left_join(imp.data, tract.numbers)
imp.data$tract <- imp.data$tract.num
imp.data$tract.num <- NULL
rm(tract.numbers)

# variables for tents + RVs, but only if at least one tent
imp.data <- mutate(imp.data, tentrv = tents+rv)
imp.data$tentrv[imp.data$tents==0] <- 0

imp.data <- mutate(imp.data, tentrvi = tentsi+rvi)
imp.data$tentrvi[imp.data$tentsi==0] <- 0

# saving data
saveRDS(imp.data, file = "harmonized_encampment_data.rds")


### 4. SPATIAL DATA

## tract-level tent+RV counts
# keeping three time points: early 2014, late 2016, early 2019
tract.tents <- imp.data %>% 
  ungroup() %>% 
  filter(time.period==2014.1 | time.period==2016.9 | time.period==2019.2) %>% 
  select(tract, time.period, tentrvi)
# collapsing to tract-level total counts of tents+RVs in each period
tract.tents <- tract.tents %>% 
  group_by(tract, time.period) %>% 
  summarize(num.tents = sum(tentrvi, na.rm = TRUE), .groups = "keep")
# reshaping from tract-period-level to tract-level format
tract.tents$time.period <- recode(tract.tents$time.period, `2014.1` = "tents2014", `2016.9` = "tents2016", `2019.2` = "tents2019")
tract.tents <- tract.tents %>% 
  pivot_wider(names_from = time.period, values_from = num.tents)

# retrieving Alameda county tract data/boundaries
alameda.tracts <- tracts(state = "CA", county = "Alameda")
writeOGR(alameda.tracts, "~/Box Sync/encampments", "alamedatracts", driver="ESRI Shapefile", overwrite_layer=TRUE)
alameda.tracts <- readOGR("~/Box Sync/encampments", "alamedatracts", encoding="ESRI Shapefile")

# tract IDs with numbers instead of strings
alameda.tracts$tract <- as.numeric(alameda.tracts$NAME)
alameda.tracts$NAME <- NULL

# merging tent data with Alameda tracts 
oak.tracts <- merge(alameda.tracts, tract.tents)
oak.tracts <- subset(oak.tracts, !is.na(oak.tracts$tents2019))

# importing national highway system, downloaded from:
# https://gisdata-caltrans.opendata.arcgis.com/datasets/8887c21dce1d45ad93cd78a10b24781b_0
all.highways <- readOGR("~/Downloads/National_Highway_System", "National_Highway_System", encoding="ESRI Shapefile")
all.highways <- spTransform(all.highways, CRS("+proj=longlat +ellps=GRS80 +no_defs"))

# highways going through Oakland
oak.highways <- subset(all.highways, all.highways$ROUTEID=="SHS_080_P" | all.highways$ROUTEID=="SHS_580_P" | all.highways$ROUTEID=="SHS_880_P" | all.highways$ROUTEID=="SHS_980_P" | all.highways$ROUTEID=="SHS_024_P")

rm(all.highways)


### 5. SAVING DATA FOR PUBLIC REPLICATION

# full data
save(raw.data, imp.data, oak.tracts, oak.highways, file = "encampment_data.RData")

# removing geographic identifiers for encampments
raw.data$maps.address <- NULL
raw.data$latitude <- NULL
raw.data$longitude <- NULL

imp.data$maps.address <- NULL
imp.data$latitude <- NULL
imp.data$longitude <- NULL

# public data
save(raw.data, imp.data, oak.tracts, oak.highways, file = "encampment_data_public.RData")
