# Code 5.0: Descriptive analysis ----

rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"
quad <- "analytical_data/quadrant/"
dis <- "analytical_data/district/"

## Open Data -----
crime <- "crime_data_RM_2017_2025.RData"
crime_count <- "crime_dis_RM_2017_2025.RData"
crime_place <- "crime_place_dis_RM_2017_2025.RData"
crime_time <- "crime_time_dis_RM_2017_2025.RData"
data_cov <- "climate_poverty_RM_2017_2025.RData"

crime <- rio::import(paste0(data_out, crime)) 
crime_count <- rio::import(paste0(data_out, quad, crime_count)) 
crime_place <- rio::import(paste0(data_out, quad, crime_place)) 
crime_time <- rio::import(paste0(data_out, quad, crime_time)) 
data_cov <- rio::import(paste0(data_out, quad, data_cov)) 

glimpse(crime)
glimpse(crime_count)

data_crime <- crime_count |> 
  left_join(data_cov, by=c("quadrant", "zone", "date"))

glimpse(data_crime)

## Spatial distribution of crimes -----

ggplot(data_cov) +
  geom_sf(aes(fill=quintil, geometry = geometry), lwd = 0.05, color = "white")

 ggplot(data_crime) +
  geom_sf(aes(fill=quintil, geometry = geometry), lwd = 0.05, color = "white")

## Spatial distribution of crimes -----


## Descriptive stats crimes -----

