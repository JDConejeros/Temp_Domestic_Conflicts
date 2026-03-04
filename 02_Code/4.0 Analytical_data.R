# 4.0 Analytical products district ----

rm(list=ls())

## Settings ----
source("02_Code/0.1 Functions.R")
source("02_Code/0.2 Settings.R")

# Data path 
data_inp <- "01_Data/Input/"
data_out <- "01_Data/Output/"

# Data files 
crimes <- "Crime_data_RM_2017_2025.RData"

## Load data ----

crimes <- rio::import(paste0(data_out, crimes))

## Analytical data ----

# Urban / Rural district
aux <- chilemapas::codigos_territoriales |> 
  mutate(nombre_comuna=stringr::str_to_title(nombre_comuna)) |> 
  filter(codigo_region==13) |> 
  mutate(codigo_comuna=as.numeric(codigo_comuna)) |> 
  filter(nombre_provincia=="Santiago" | codigo_comuna==13201) |> 
  distinct(codigo_comuna) |> 
  as_vector() 

crimes <- crimes |> 
  mutate(zone = if_else(code_district %in% aux, "Urban", "Rural")) |> 
  mutate(zone=factor(zone, levels=c("Rural", "Urban")))

# time_hms: morning / afternoon / night
crimes <- crimes |> 
  mutate(
    time_event = case_when(
    hour(time_hms) >= 6 & hour(time_hms) < 18 ~ "Daytime",
    #hour(time_hms) >= 12 & hour(time_hms) < 18 ~ "Afternoon",
    TRUE ~ "Nighttime"
  )) |> 
  mutate(time_event = factor(time_event, levels = c("Daytime", "Nighttime")))

# Save data
save(crimes, file=paste0(data_out, "analytical_data/", "quadrant/", "crime_data_RM_2017_2025", ".RData"))

## Two data: district-level and quadrant-level

# Sequence dates 
full_dates <- tibble(
  date = seq(
    min(crimes$date),
    max(crimes$date),
    by = "day"
  )
)
summary(full_dates) # 2017-01-01 - 2025-08-31  
full_dates # 3165

# Time: date, year, month, day 
# Cluster: quadrant 
# Urban / Rural quadrant 
# Counts: 
# group (7)
# place_category
# time_hms: morning / afternoon / night

#3165*52=164580

# Count group crimes ------

# Overall 
crime_count <- crimes |> 
  group_by(zone, code_district, district, quadrant, date, group) |> 
  summarise(crime_count = n()) |> 
  group_by(zone, code_district, district, quadrant, group) |> 
  complete(date = full_dates$date) |> 
  mutate(
  year = lubridate::year(date), 
  month = lubridate::month(date), 
  day = lubridate::day(date)) |> 
  mutate(
    year_month = format(date, "%Y-%m"),
    day_year = lubridate::yday(date),
    day_month = lubridate::day(date),
    day_week = lubridate::wday(date, label = TRUE, abbr = FALSE),
    weekends = if_else(day_week %in% c("Saturday", "Sunday"), 1, 0),
    season = case_when(
      (month == 12 & day_month >= 21) | (month %in% c(1, 2)) | (month == 3 & day_month <= 20) ~ "Summer",
      (month == 3 & day_month >= 21) | (month %in% c(4, 5)) | (month == 6 & day_month <= 20) ~ "Autumn",
      (month == 6 & day_month >= 21) | (month %in% c(7, 8)) | (month == 9 & day_month <= 20) ~ "Winter",
      (month == 9 & day_month >= 21) | (month %in% c(10, 11)) | (month == 12 & day_month <= 20) ~ "Spring"
    )
  ) |> 
  ungroup() |> 
  mutate(crime_count = replace_na(crime_count, 0)) |>
  pivot_wider(names_from = group, values_from = crime_count, values_fill = 0) |> 
  janitor::clean_names() 

# Day/Night 
crime_count_time <- crimes |> 
  group_by(time_event, zone, code_district, district, quadrant, date, group) |> 
  summarise(crime_count = n()) |> 
  group_by(time_event, zone, code_district, district, quadrant, group) |> 
  complete(date = full_dates$date) |> 
  mutate(
  year = lubridate::year(date), 
  month = lubridate::month(date), 
  day = lubridate::day(date)) |> 
  mutate(
    year_month = format(date, "%Y-%m"),
    day_year = lubridate::yday(date),
    day_month = lubridate::day(date),
    day_week = lubridate::wday(date, label = TRUE, abbr = FALSE),
    weekends = if_else(day_week %in% c("Saturday", "Sunday"), 1, 0),
    season = case_when(
      (month == 12 & day_month >= 21) | (month %in% c(1, 2)) | (month == 3 & day_month <= 20) ~ "Summer",
      (month == 3 & day_month >= 21) | (month %in% c(4, 5)) | (month == 6 & day_month <= 20) ~ "Autumn",
      (month == 6 & day_month >= 21) | (month %in% c(7, 8)) | (month == 9 & day_month <= 20) ~ "Winter",
      (month == 9 & day_month >= 21) | (month %in% c(10, 11)) | (month == 12 & day_month <= 20) ~ "Spring"
    )
  ) |> 
  ungroup() |> 
  mutate(crime_count = replace_na(crime_count, 0)) |>
  pivot_wider(names_from = group, values_from = crime_count, values_fill = 0) |> 
  janitor::clean_names() 

glimpse(crime_count_time) # Rows: 2,294,625

# Public/Private/Other
crime_count_place <- crimes |> 
  group_by(place_type, zone, code_district, district, quadrant, date, group) |> 
  summarise(crime_count = n()) |> 
  group_by(place_type, zone, code_district, district, quadrant, group) |> 
  complete(date = full_dates$date) |> 
  mutate(
  year = lubridate::year(date), 
  month = lubridate::month(date), 
  day = lubridate::day(date)) |> 
  mutate(
    year_month = format(date, "%Y-%m"),
    day_year = lubridate::yday(date),
    day_month = lubridate::day(date),
    day_week = lubridate::wday(date, label = TRUE, abbr = FALSE),
    weekends = if_else(day_week %in% c("Saturday", "Sunday"), 1, 0),
    season = case_when(
      (month == 12 & day_month >= 21) | (month %in% c(1, 2)) | (month == 3 & day_month <= 20) ~ "Summer",
      (month == 3 & day_month >= 21) | (month %in% c(4, 5)) | (month == 6 & day_month <= 20) ~ "Autumn",
      (month == 6 & day_month >= 21) | (month %in% c(7, 8)) | (month == 9 & day_month <= 20) ~ "Winter",
      (month == 9 & day_month >= 21) | (month %in% c(10, 11)) | (month == 12 & day_month <= 20) ~ "Spring"
    )
  ) |> 
  ungroup() |> 
  mutate(crime_count = replace_na(crime_count, 0)) |>
  pivot_wider(names_from = group, values_from = crime_count, values_fill = 0) |> 
  janitor::clean_names() 

glimpse(crime_count_place) # Rows: 3,367,560

# Save crime data 
save(crime_count, file=paste0(data_out, "analytical_data/", "quadrant/", "crime_quad_RM_2017_2025", ".RData"))
save(crime_count_time, file=paste0(data_out, "analytical_data/", "quadrant/", "crime_quad_time_RM_2017_2025", ".RData"))
save(crime_count_place, file=paste0(data_out, "analytical_data/", "quadrant/", "crime_quad_place_RM_2017_2025", ".RData"))

# Covariates ------

# Clean memory 
rm(list=(ls()))

# Data path 
data_inp <- "01_Data/Input/"
data_out <- "01_Data/Output/"

# Data
casen <- "Poverty_casen_RM_2017.RData"
ndvi_dis <- "NDVI_quadrant_data_RM_2017_2025.RData"
temp_dis <- "Temp_quadrant_data_RM_2017_2025.RData"

casen <- rio::import(paste0(data_out, casen))
ndvi <- rio::import(paste0(data_out, ndvi_dis))
temp <- rio::import(paste0(data_out, temp_dis))

# Other covariates: temperature, NDVI and poverty
glimpse(temp)
glimpse(ndvi)
glimpse(casen)

# Merge district-level data
ndvi <- ndvi |> 
  dplyr::select(com_code, quad_code, date, ndvi)

data_cov <- temp |> 
  left_join(ndvi, by=c("com_code", "quad_code", "date")) |> 
  left_join(casen, by=c("com_code"="comuna"))

glimpse(data_cov)

# Complete ndvi with mean between two dates
# Impute NA with interpolation and kalman filter 
# Missing daily NDVI values were imputed using a state-space model with Kalman smoothing, 
# which estimates the latent vegetation signal conditional on the observed MODIS composites, accounting for temporal dependence and measurement noise.
# Reference: Harvey, Andrew C. Forecasting, structural time series models and the Kalman filter. Cambridge university press, 1990

green <- data_cov |> 
  dplyr::select(com_code, quad_code, date, ndvi) |>
  group_by(com_code, quad_code) |>
  mutate(
    ndvi_kalman = na_kalman(
      ndvi,
      model = "StructTS",
      smooth = TRUE
    )
  ) |>
  ungroup()

glimpse(green)
summary(green)

data_cov <- data_cov |> 
  dplyr::select(-ndvi) 

data_cov <- data_cov |> 
  left_join(green, by = c("com_code", "quad_code", "date")) 

# Separte data and geometry to reduce memory usage
data_geo <- data_cov |> 
  dplyr::select(com_code, quadrant_type, quadrant, quad_code, geometry) |> 
  distinct()

# Remove geometry multipoligon 
data_cov <- data_cov |> 
  dplyr::select(-geometry)

glimpse(data_cov)

## Save district-level analytical data ----
save(data_cov, file=paste0(data_out, "analytical_data/", "quadrant/", "climate_poverty_RM_2017_2025", ".RData"))
save(data_geo, file=paste0(data_out, "analytical_data/", "quadrant/", "quad_geometry_2022", ".RData"))
