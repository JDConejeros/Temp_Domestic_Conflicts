# Code 1: Crime data preparation ----
rm(list=ls())
## Settings ----
source("02_Code/0.1 Functions.R")
source("02_Code/0.2 Settings.R")

# Data path 
data_inp <- "01_Data/Input/"
data_out <- "01_Data/Output/"
crime <- "Crime_RM_2017_2025.xlsx"

## Crime data select ---- 
# Read all files .xlsb and bind per rows
sheet_names <- openxlsx::getSheetNames(paste0(data_inp, crime))
sheet_names

data_crime <- sheet_names |> 
  purrr::map_dfr(~ {
    rio::import(
      paste0(data_inp, crime), 
      sheet = .x
    ) |> 
      janitor::clean_names() 
      #dplyr::mutate(hora_del_delito = as.character(hora_del_delito))
  })

glimpse(data_crime) # Data crime in preparation 

## Crime data preparation ----

# Homogenize values columns district 
tab_reg <- data_crime |> 
  group_by(comuna) |>
  summarise(n = n()) |> 
  ungroup() |> 
  dplyr::select(comuna) |> 
  mutate(aux = if_else(comuna == "ÑUÑOA", "NUNOA", comuna)) |> 
  arrange(aux) 

# Only district in RM
comunas <- chilemapas::codigos_territoriales
comunas <- comunas %>% 
  mutate(codigo_comuna=as.numeric(codigo_comuna)) %>% 
  filter(codigo_region==13) |> 
  dplyr::select(codigo_comuna, nombre_comuna, codigo_region, nombre_region) |> 
  arrange(nombre_comuna)

comunas <- tab_reg |> 
  bind_cols(comunas) |> 
  dplyr::select(-aux)

data_crime <- data_crime |> 
  left_join(comunas, by = "comuna")

# Prepare covariates 
data_crime <- data_crime |> 
  mutate(
    date = lubridate::as_date(paste(ano_del_delito, mes_del_delito, dia_del_delito, sep = "-")),
    year = lubridate::year(date),
    month = lubridate::month(date),
    day = lubridate::day(date),
    hour = lubridate::hour(hora_del_delito),
    minute = lubridate::minute(hora_del_delito),
    time_hms = hms::as_hms(hora_del_delito),
    detention = if_else(clasificacion=="DETENCION", 1, 0),
    complaint = if_else(clasificacion=="DENUNCIA", 1, 0),
  ) |> 
  rename(
    district = nombre_comuna,
    code_district = codigo_comuna,
    reg = nombre_region,
    code_reg = codigo_region,
    crime = delito,
    place = lugar,
    quadrant = cuadrante
  ) |> 
  dplyr::select(date, year, month, day, hour, minute, time_hms,
         reg, code_reg, district, code_district,
         crime, place, quadrant,
         detention, complaint
    )

glimpse(data_crime)
summary(data_crime)

#> test$crime
# [1] "Consumo de bebidas alcohólicas en la vía pública"      
# [2] "Apropiación Indebida"                                  
# [3] "Consumo de bebidas alcohólicas en la vía pública"      
# [4] "Otros de los cuasidelitos"                             
# [5] "Consumo de bebidas alcohólicas en la vía pública"      
# [6] "Vigilancia privada no autorizada"                      
# [7] "Uso Fraudulento de Tarjetas de Crédito y Débito"       
# [8] "Contra la salud pública (art. 313 D al 315 y art. 317)"
# [9] "Aprehendidos por orden judicial"                       
#[10] "Consumo de bebidas alcohólicas en la vía pública"      

# Filter crimes without time: 5425904 obs
data_crime <- data_crime |> 
  filter(!is.na(hour))

# Homogenize values columns with type crime 
data_crime |> 
  group_by(crime) |>
  summarise(n = n()) |> 
  writexl::write_xlsx("01_Data/Output/Crime_types.xlsx")

# "typecrime"
#1   1_Robbery
#2   2_Larceny
#3   3_Vehicle theft
#4   4_Burglary
#5   5_Injuries
#6   6_Intrafamily violence

data_crime |> 
  group_by(place) |>
  summarise(n = n()) |> 
  writexl::write_xlsx("01_Data/Output/Crime_places.xlsx")

# Save data -----
save(data_crime, file = paste0(data_out, "Crime_process_RM_2017_2025.RData"))
