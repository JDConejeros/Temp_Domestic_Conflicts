# Code 5.0: Descriptive analysis ----

rm(list=ls())
## Settings ----
source("02_Code/0.1 Functions.R")
source("02_Code/0.2 Settings.R")

# Data path 
data_out <- "01_Data/Output/"
quad <- "analytical_data/quadrant/"

## Open Data -----
crime_count <- "crime_quad_RM_2017_2025.RData"
data_cov <- "climate_poverty_RM_2017_2025.RData"
quad_geo <- "quad_geometry_2022.RData" 
#crime <- "crime_data_RM_2017_2025.RData"
#crime_time <- "crime_quad_time_RM_2017_2025"
#crime_place <- "crime_quad_place_RM_2017_2025"

crime_count <- rio::import(paste0(data_out, quad, crime_count)) 
data_cov <- rio::import(paste0(data_out, quad, data_cov)) 
quad_geo <- rio::import(paste0(data_out, quad, quad_geo)) 
#crime <- rio::import(paste0(data_out, crime)) 
#crime_place <- rio::import(paste0(data_out, quad, crime_place)) 
#crime_time <- rio::import(paste0(data_out, quad, crime_time)) 

data_cov <- data_cov |> 
  dplyr::select(-c(com_code, quadrant_type, quadrant)) |>
  left_join(quad_geo, by = "quad_code")

glimpse(crime_count)
glimpse(quad_geo)
glimpse(data_cov)

## Spatial distribution of crimes -----

## Crime counts by quadrant ----

ndv <- crime_count |> 
  group_by(zone, code_district, district, quadrant) |> 
  summarise(n_crimes = sum(domestic_violence)) |> 
  mutate(quintile = ntile(n_crimes, 5)) |> 
  mutate(q5 = factor(quintile, 
                  levels = 1:5, 
                  labels = c("Lower quantile", 
                             "2nd quantile",
                             "3nd quantile",
                             "4nd quantile",
                             "Upper quantile"
                            ))) 
  #filter(zone == "Urban")

glimpse(ndv)
glimpse(quad_geo)

map_ndv <- quad_geo |> 
  left_join(n_dv, by = c("com_code"="code_district", "quadrant"))

map_ndv |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill=q5)) 

stgo <- chilemapas::mapa_comunas |> 
  mutate(codigo_comuna = as.numeric(codigo_comuna)) |> 
  filter(codigo_comuna %in% unique(crime_count$cod_mun)) |> 
  left_join(crime_stgo, by=c("codigo_comuna"="cod_mun"))