# Code 2: Temp data preparation ----

## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

## Open data ---- 

# ID file load
temp_qua_f1 <- "temperature_daily_quadrant.csv"
temp_dis_f1 <- "temperature_daily_district.csv"

# NDVI 
ndvi_qua_f2 <- "ndvi_daily_quadrant.csv"
ndvi_dis_f2 <- "ndvi_daily_district.csv"

# Shape data
data_qua <- rio::import(paste0(data_out, "Quadrant_data_geo_RM.RData"))
data_dis <- rio::import(paste0(data_out, "District_data_geo_RM.RData"))

# Temperature
temp_qua <- rio::import(paste0(data_out, "clim_data/", temp_qua_f1)) %>% janitor::clean_names() |> rename(geometry=geo)
temp_dis <- rio::import(paste0(data_out, "clim_data/", temp_dis_f1)) %>% janitor::clean_names()
#test <- rio::import(paste0(data_out, "clim_data/RM_ERA5Land_DailyTemp_2017-01-01-2025-08-31.csv")) %>% janitor::clean_names()

# NDVI
ndvi_qua <- rio::import(paste0(data_out, "clim_data/", ndvi_qua_f2)) %>% janitor::clean_names()
ndvi_dis <- rio::import(paste0(data_out, "clim_data/", ndvi_dis_f2)) %>% janitor::clean_names()

## Filter date range ----
# Filter temperature and NDVI data to period 2017-01-01 to 2025-08-31
start_date <- as.Date("2017-01-01")
end_date <- as.Date("2025-08-31")

temp_qua <- temp_qua |> 
  mutate(date = as.Date(date)) |> 
  filter(date >= start_date & date <= end_date) |> 
  dplyr::select(geometry_id, date, temperature_2m, temperature_2m_min, temperature_2m_max)

temp_dis <- temp_dis |> 
  mutate(date = as.Date(date)) |> 
  filter(date >= start_date & date <= end_date) |> 
  dplyr::select(geometry_id, date, temperature_2m, temperature_2m_min, temperature_2m_max)

ndvi_qua <- ndvi_qua |> 
  mutate(date = as.Date(date)) |> 
  filter(date >= start_date & date <= end_date) |> 
  dplyr::select(geometry_id, date, ndvi)

ndvi_dis <- ndvi_dis |> 
  mutate(date = as.Date(date)) |> 
  filter(date >= start_date & date <= end_date) |> 
  dplyr::select(geometry_id, date, ndvi)

## Create geometry mapping for districts ----
# The geometry_id in temp_dis is correlative (0, 1, 2...) and doesn't match codigo_comuna
# Since temp_dis doesn't contain geometry information, we match by the order used during export
# The order in Earth Engine export corresponds to the order geometries were processed
# This matches the order in data_dis when converted to FeatureCollection (sorted by codigo_comuna)

# Create mapping table: geometry_id -> codigo_comuna
# Match assumes geometries were processed in order sorted by codigo_comuna
# This is the order used in the Python script when creating the FeatureCollection
data_dis <- data_dis |> 
  mutate(geometry_id = row_number() - 1) 

data_qua <- data_qua |> 
  mutate(geometry_id = row_number() - 1) 

# Join with climate data 
temp_dis_join <- data_dis |> 
  left_join(temp_dis, by = "geometry_id")

temp_qua_join <- data_qua |> 
  left_join(temp_qua, by = "geometry_id")

ndvi_dis_join <- data_dis |> 
  left_join(ndvi_dis, by = "geometry_id")

ndvi_qua_join <- data_qua |> 
  left_join(ndvi_qua, by = "geometry_id")


## Save processed data ----
save(temp_dis_join, file = paste0(data_out, "Temp_district_data_RM_2017_2025.RData"))
save(temp_qua_join, file = paste0(data_out, "Temp_quadrant_data_RM_2017_2025.RData"))

save(ndvi_dis_join, file = paste0(data_out, "NDVI_district_data_RM_2017_2025.RData"))
save(ndvi_qua_join, file = paste0(data_out, "NDVI_quadrant_data_RM_2017_2025.RData"))


