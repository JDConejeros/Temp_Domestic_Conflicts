# 4.0 Analytical products district ----

rm(list=ls())

## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

# Data files 

crimes <- "Crime_data_RM_2017_2025.RData"
temp_dis <- "Temp_district_data_RM_2017_2025.RData"
ndvi_dis <- "NDVI_district_data_RM_2017_2025.RData"
casen <- "Poverty_casen_RM_2017.RData"

## Load data ----

crimes <- rio::import(paste0(data_out, crimes))
temp_dis <- rio::import(paste0(data_out, temp_dis))
ndvi_dis <- rio::import(paste0(data_out, ndvi_dis))
casen <- rio::import(paste0(data_out, casen))

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
save(crimes, file=paste0(data_out, "analytical_data/", "district/", "Crime_data_RM_2017_2025", ".RData"))

## Two data: district-level and quadrant-level

# Sequence dates 
full_dates <- tibble(
  date = seq(
    min(crimes$date),
    max(crimes$date),
    by = "day"
  )
)

full_dates # 3165

# Time: date, year, month, day 
# Cluster: district (52) // quadrant 
# Urban / Rural district 
# Counts: 
# group (7)
# place_category
# time_hms: morning / afternoon / night

#3165*52=164580

# Count group crimes 
crime_count <- crimes |> 
  group_by(code_district, district, zone, date, group) |> 
  summarise(crime_count = n()) |> 
  group_by(code_district, district, zone, group) |> 
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

glimpse(crime_count)

# Count place_category 
places <- unique(crimes$place_category)
# 16781856
crime_place <- crimes |>
  group_by(code_district, district, zone, date, group, place_category) |>
  summarise(place_count = n()) |>
  group_by(code_district, district, zone, group) |>
  complete(place_category = places) |>
  group_by(code_district, district, zone, group, place_category) |>
  mutate(
    place_count = replace_na(place_count, 0)
  ) |>
  ungroup() |>
  pivot_wider(
    names_from  = place_category,
    values_from = place_count,
    values_fill = 0
  ) |>
  janitor::clean_names()

glimpse(crime_place)

# Count morning / afternoon / night
times <- unique(crimes$time_event)

crime_time <- crimes |>
  group_by(code_district, district, zone, date, group, time_event) |>
  summarise(time_count = n()) |>
  group_by(code_district, district, zone, group) |>
  complete(time_event= times) |>
  group_by(code_district, district, zone, group, time_event) |>
  mutate(
    time_count = replace_na(time_count, 0)
  ) |>
  ungroup() |>
  pivot_wider(
    names_from  = time_event,
    values_from = time_count,
    values_fill = 0
  ) |>
  janitor::clean_names()

glimpse(crime_time)

# Other covariates: temperature, NDVI and poverty
glimpse(temp_dis)
glimpse(ndvi_dis)
glimpse(casen)

# Merge district-level data
data_cov <- temp_dis |> 
  left_join(ndvi_dis, by=c("codigo_comuna", "nombre_comuna", "geometry", "geometry_id", "date")) |> 
  left_join(casen, by=c("codigo_comuna"="comuna"))

# Complete ndvi with mean between two dates
# Impute NA with interpolation and kalman filter 
# Missing daily NDVI values were imputed using a state-space model with Kalman smoothing, 
# which estimates the latent vegetation signal conditional on the observed MODIS composites, accounting for temporal dependence and measurement noise.
# Reference: Harvey, Andrew C. Forecasting, structural time series models and the Kalman filter. Cambridge university press, 1990

green <- data_cov |> 
  dplyr::select(codigo_comuna, date, ndvi) |>
  group_by(codigo_comuna) |>
  mutate(
    ndvi_kalman = na_kalman(
      ndvi,
      model = "StructTS",
      smooth = TRUE
    )
  ) |>
  ungroup()

glimpse(green)

data_cov <- data_cov |> 
  dplyr::select(-ndvi) |>
  left_join(green, by = c("codigo_comuna", "date"))

glimpse(data_cov)

## Save district-level analytical data ----
save(crime_count, file=paste0(data_out, "analytical_data/", "district/", "crime_dis_RM_2017_2025", ".RData"))
save(crime_place, file=paste0(data_out, "analytical_data/", "district/", "crime_place_dis_RM_2017_2025", ".RData"))
save(crime_time, file=paste0(data_out, "analytical_data/", "district/", "crime_time_dis_RM_2017_2025", ".RData"))
save(data_cov, file=paste0(data_out, "analytical_data/", "district/", "climate_poverty_RM_2017_2025", ".RData"))
