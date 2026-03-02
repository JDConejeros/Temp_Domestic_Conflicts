# Code 1.1: Crime classification ----
rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

# Crime Inputs ----
crime_data <- rio::import(paste0(data_out, "Crime_process_RM_2017_2025.RData")); glimpse(crime_data)
crimes_groups <- rio::import(paste0(data_out, "Crime_types_groups.xlsx")); glimpse(crimes_groups)  
crimes_plances <- rio::import(paste0(data_out, "Crime_places_groups.xlsx")); glimpse(crimes_plances)  
quad_geo <- rio::import(paste0(data_out, "Info_geo_quadrant.RData")); glimpse(quad_geo)

# Joint data ----
crime_data <- crime_data |> 
  left_join(crimes_groups |> dplyr::select(-n), by = "crime") |> 
  left_join(crimes_plances |> dplyr::select(-n), by = "place")

unique(crime_data$family)
#[5] "Domestic violence"               
#[4] "Crimes against life or personal integrity"
#[2] "Violent robberies"                        
#[3] "Non-violent property crimes"              
#[6] "Weapon-related crimes"                    
#[7] "Incivilities"                             
#[8] "Drug-related crimes" 
#[1] "Other"                                    

unique(crime_data$place_category)
# "Public space"    "Residence"       
# "Transportation"  "Commercial space" "Work/study place"  "Other"   
unique(crime_data$place_type)
# [1] "Public"   "Private""Other"  

crime_data <- crime_data |> 
  mutate(
    family = factor(family, levels = c("Domestic violence", "Crimes against life or personal integrity", 
                                       "Violent robberies", "Non-violent property crimes", "Weapon-related crimes",
                                       "Incivilities", "Drug-related crimes", "Other")),
    place_category = factor(place_category, levels = c("Public space", "Residence", 
                                                       "Transportation", "Commercial space", 
                                                       "Work/study place", "Other")),
    place_type = factor(place_type, levels = c("Public", "Private", "Other"))
  )

glimpse(crime_data)
summary(crime_data)
glimpse(quad_geo)

quad_geo <- quad_geo |> 
  mutate(com_codigo=as.numeric(com_codigo)) |> 
  dplyr::select(
    uni_codigo,
    cua_codigo, 
    cua_descri, 
    cua_tipo,
    comuna, 
    com_codigo,
    unidad, 
    zona, 
    num_cuad, 
    uni_codigo,
    cod_aupol,
    cua_ano, 
    shape_area, 
    geometry      
        ) |>
  rename(
    quadrant = cua_descri,
    quadrant_type = cua_tipo,
    com = comuna,
    com_code = com_codigo,
    unit = unidad,
    zone = zona,
    quad_num = num_cuad,
    quad_code = cua_codigo
  )

glimpse(quad_geo)

# Join quad data 
crime_data <- crime_data |> 
  mutate(quadrant = stringr::str_replace(quadrant, "Sec.Rural", "Sector Rural")) |> 
  mutate(quadrant = stringr::str_replace(quadrant, "(?<=\\bCuadrante\\s)0([1-9])\\b", "\\1"))

units <- unique(crime_data$quadrant)[!unique(crime_data$quadrant) %in% unique(quad_geo$quadrant)]
table(unique(crime_data$quadrant) %in% unique(quad_geo$quadrant))
table(unique(quad_geo$quadrant) %in% unique(crime_data$quadrant))

# Edit manual quadrant to check the most similar 
crime_data <- crime_data |>
  mutate(
    quadrant = case_when(
      quadrant == "Cuadrante 122A" ~ "Cuadrante 122",
      quadrant == "Cuadrante 14"   ~ "Cuadrante 13",
      quadrant == "Cuadrante 141A" ~ "Cuadrante 141",
      quadrant == "Cuadrante 178"  ~ "Cuadrante 177",
      quadrant == "Cuadrante 237B" ~ "Cuadrante 237A",
      quadrant == "Cuadrante 238"  ~ "Cuadrante 239",
      quadrant == "Cuadrante 25C"  ~ "Cuadrante 25A",
      quadrant == "Cuadrante 26A"  ~ "Cuadrante 26",
      quadrant == "Cuadrante 36B"  ~ "Cuadrante 36",
      quadrant == "Cuadrante 36C"  ~ "Cuadrante 36",
      quadrant == "Cuadrante 37B"  ~ "Cuadrante 37",
      quadrant == "Cuadrante 38B"  ~ "Cuadrante 38",
      quadrant == "Cuadrante 39B"  ~ "Cuadrante 39",
      quadrant == "Cuadrante 59"   ~ "Cuadrante 59A",
      TRUE ~ quadrant
    )
  )

# Join geo data
crime_data <- crime_data 
glimpse(crime_data)

# District maps 
com_geo <- chilemapas::codigos_territoriales |> 
  mutate(codigo_comuna=as.numeric(codigo_comuna)) |> 
  filter(codigo_region==13) |> 
  dplyr::select(codigo_comuna, nombre_comuna) |> 
  left_join(chilemapas::mapa_comunas |> 
    dplyr::select(codigo_comuna, geometry) |> 
    mutate(codigo_comuna=as.numeric(codigo_comuna))
)

# Shape file
quad_geo_shp <- st_as_sf(quad_geo)
st_crs(quad_geo_shp)

com_geo_shp <- st_as_sf(com_geo)
st_crs(com_geo_shp)

## Save all data -----
save(crime_data, file = paste0(data_out, "Crime_data_RM_2017_2025.RData"))
save(quad_geo, file = paste0(data_out, "Quadrant_data_geo_RM.RData"))
save(com_geo, file = paste0(data_out, "District_data_geo_RM.RData"))

# Shape file export
st_write(quad_geo_shp, paste0(data_out, "quadrant_geo/","quad_geo.shp"), delete_layer = TRUE)
st_write(com_geo_shp, paste0(data_out, "district_geo/","district_geo.shp"), delete_layer = TRUE)
