# Code 1.1: Crime classification ----
rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

# Crime types ----
crimes <- rio::import(paste0(data_out, "Crime_types.xlsx"))  # crime types

# https://cead.minsegpublica.gob.cl/wp-content/uploads/file-manager/nuevos-grupos-delictuales-ene-sep-2025.pdf
# Crime groupings.

## Explore unique crime names ----
unique_crimes <- unique(crimes$crime)
cat("Total unique crimes:", length(unique_crimes), "\n")
print(unique_crimes)

## Create classification mapping using case_when ----
crimes_classified <- crimes |> 
  mutate(
    crime_upper = toupper(crime),
    
    # Classify into subgrupo, grupo, and familia using case_when
    subgrupo = case_when(
      # CRIMES AGAINST LIFE OR PERSONAL INTEGRITY
      str_detect(crime_upper, "HOMICIDIO") & !str_detect(crime_upper, "FEMICIDIO") ~ "Homicidios",
      str_detect(crime_upper, "FEMICIDIO") ~ "Femicidios",
      str_detect(crime_upper, "VIOLACION") ~ "Violaciones",
      str_detect(crime_upper, "ABUSO SEXUAL") ~ "Abusos sexuales",
      str_detect(crime_upper, "ACOSO SEXUAL") ~ "Acosos sexuales",
      str_detect(crime_upper, "DELITO SEXUAL|DELITOS SEXUALES") ~ "Otros delitos sexuales",
      str_detect(crime_upper, "LESION.*GRAV.*SIMA") ~ "Lesiones graves o gravísimas",
      str_detect(crime_upper, "LESION.*MENOS GRAVE") ~ "Lesiones menos graves",
      str_detect(crime_upper, "LESION.*LEVE") ~ "Lesiones leves",
      str_detect(crime_upper, "^AMENAZA") & !str_detect(crime_upper, "VIF|VIOLENCIA INTRAFAMILIAR|ARMA") ~ "Amenazas",
      
      # VIOLENT ROBBERIES
      str_detect(crime_upper, "ROBO.*VIOLENTO.*VEHICULO|ROBO VIOLENTO DE VEHICULO") ~ "Robo violento de vehículo motorizado",
      str_detect(crime_upper, "ROBO.*VIOLENCIA|ROBO.*INTIMIDACION|ROBO CON VIOLENCIA|ROBO CON INTIMIDACION") ~ "Robos con violencia o intimidación",
      str_detect(crime_upper, "ROBO.*SORPRESA") ~ "Robo por sorpresa",
      
      # DOMESTIC VIOLENCE
      str_detect(crime_upper, "VIF.*LESION.*FISICA|VIOLENCIA INTRAFAMILIAR.*LESION.*FISICA") ~ "VIF con lesiones físicas",
      str_detect(crime_upper, "VIF.*LESION.*PSICOLOGICA|VIOLENCIA INTRAFAMILIAR.*LESION.*PSICOLOGICA") ~ "VIF con lesiones psicológicas",
      str_detect(crime_upper, "MALTRATO HABITUAL") ~ "Maltrato habitual",
      str_detect(crime_upper, "AMENAZA.*VIF|AMENAZA.*VIOLENCIA INTRAFAMILIAR") ~ "Amenazas en contexto de VIF",
      str_detect(crime_upper, "VIOLENCIA INTRAFAMILIAR|VIF") ~ "VIF no clasificada",
      
      # DRUG-RELATED CRIMES
      str_detect(crime_upper, "TRAFICO.*SUSTANCIA|TRAFICO.*DROGA") & !str_detect(crime_upper, "MICRO") ~ "Tráfico de sustancias",
      str_detect(crime_upper, "MICROTRAFICO|MICRO.*TRAFICO") ~ "Microtráfico de sustancias",
      str_detect(crime_upper, "ELABORACION.*SUSTANCIA|PRODUCCION.*SUSTANCIA|ELABORACION.*DROGA|PRODUCCION.*DROGA") ~ "Elaboración o producción de sustancias",
      str_detect(crime_upper, "LEY.*DROGA|LEY.*SUSTANCIA") ~ "Otras infracciones a la ley de drogas",
      
      # WEAPON-RELATED CRIMES
      str_detect(crime_upper, "DISPARO.*INJUSTIFICADO") ~ "Disparo injustificado",
      str_detect(crime_upper, "PORTE.*ARMA.*EXPLOSIVO|POSESION.*ARMA.*EXPLOSIVO|PORTE.*EXPLOSIVO|POSESION.*EXPLOSIVO") ~ "Porte / posesión de armas o explosivos",
      str_detect(crime_upper, "PORTE.*ARMA.*CORTANTE|PORTE.*ARMA.*PUNZANTE|ARMA.*CORTANTE|ARMA.*PUNZANTE") ~ "Porte de arma cortante o punzante",
      str_detect(crime_upper, "LEY.*ARMA") ~ "Otras infracciones a la ley de armas",
      
      # NON-VIOLENT PROPERTY CRIMES
      str_detect(crime_upper, "ROBO.*LUGAR.*HABITADO") ~ "Robo en lugar habitado",
      str_detect(crime_upper, "ROBO.*LUGAR.*NO.*HABITADO|ROBO.*LUGAR.*NO HABITADO") ~ "Robos en lugar no habitado",
      str_detect(crime_upper, "ROBO.*VEHICULO|ROBO.*AUTOMOVIL") & !str_detect(crime_upper, "VIOLENTO|OBJETO|DESDE") ~ "Robo de vehículo motorizado",
      str_detect(crime_upper, "ROBO.*OBJETO.*VEHICULO|ROBO.*DESDE.*VEHICULO") ~ "Robo de objetos de o desde vehículo",
      str_detect(crime_upper, "ROBO.*FUERZA.*COSA") ~ "Otros robos con fuerza en las cosas",
      str_detect(crime_upper, "^HURTO") ~ "Hurtos",
      str_detect(crime_upper, "RECEPTACION") ~ "Receptación",
      
      # INCIVILITIES
      str_detect(crime_upper, "AMENAZA.*ARMA.*FALTA") ~ "Amenaza con armas (falta)",
      str_detect(crime_upper, "RINA.*PUBLICA|RIÑA.*PUBLICA") ~ "Riña pública",
      str_detect(crime_upper, "CONSUMO.*DROGA.*VIA.*PUBLICA|CONSUMO.*SUSTANCIA.*VIA.*PUBLICA") ~ "Consumo de drogas en la vía pública",
      str_detect(crime_upper, "PORTE.*DROGA") & !str_detect(crime_upper, "MICROTRAFICO|TRAFICO") ~ "Porte de drogas",
      str_detect(crime_upper, "FALTA.*LEY.*DROGA") ~ "Otras faltas ley de drogas",
      str_detect(crime_upper, "CONSUMO.*ALCOHOL.*VIA.*PUBLICA|CONSUMO.*BEBIDA.*ALCOHOLICA.*VIA.*PUBLICA") ~ "Consumo de alcohol en la vía pública",
      str_detect(crime_upper, "^DANO") ~ "Daños",
      str_detect(crime_upper, "DESORDEN.*PUBLICO") ~ "Desórdenes públicos",
      str_detect(crime_upper, "ANIMAL.*SUELTO.*VIA.*PUBLICA") ~ "Animales sueltos en la vía pública",
      str_detect(crime_upper, "COMERCIO.*ILEGAL") ~ "Comercio ilegal",
      str_detect(crime_upper, "OFENSA.*PUDOR") ~ "Ofensas al pudor",
      
      TRUE ~ "Other"
    ),
    
    # Assign grupo based on subgrupo
    grupo = case_when(
      subgrupo %in% c("Homicidios", "Femicidios") ~ "Homicides and femicides",
      subgrupo %in% c("Violaciones", "Abusos sexuales", "Acosos sexuales", "Otros delitos sexuales") ~ "Rapes and sexual crimes",
      subgrupo == "Lesiones graves o gravísimas" ~ "Serious or very serious injuries",
      subgrupo == "Lesiones menos graves" ~ "Less serious injuries",
      subgrupo == "Lesiones leves" ~ "Minor injuries",
      subgrupo == "Amenazas" ~ "Threats",
      subgrupo %in% c("Robos con violencia o intimidación", "Robo violento de vehículo motorizado") ~ "Robberies with violence or intimidation",
      subgrupo == "Robo por sorpresa" ~ "Surprise robbery",
      subgrupo %in% c("VIF con lesiones físicas", "VIF con lesiones psicológicas", "Maltrato habitual", 
                      "Amenazas en contexto de VIF", "VIF no clasificada") ~ "Domestic violence",
      subgrupo %in% c("Tráfico de sustancias", "Microtráfico de sustancias", "Elaboración o producción de sustancias", 
                      "Otras infracciones a la ley de drogas") ~ "Crimes and simple drug law offenses",
      subgrupo %in% c("Disparo injustificado", "Porte / posesión de armas o explosivos", 
                      "Otras infracciones a la ley de armas") ~ "Crimes and simple weapon law offenses",
      subgrupo == "Porte de arma cortante o punzante" ~ "Carrying sharp or pointed weapon",
      subgrupo %in% c("Robo en lugar habitado", "Robos en lugar no habitado") ~ "Robberies in inhabited and uninhabited places",
      subgrupo %in% c("Robo de vehículo motorizado", "Robo de objetos de o desde vehículo") ~ "Robberies of vehicles and their accessories",
      subgrupo == "Otros robos con fuerza en las cosas" ~ "Other robberies with force",
      subgrupo == "Hurtos" ~ "Thefts",
      subgrupo == "Receptación" ~ "Receiving stolen goods",
      subgrupo %in% c("Amenaza con armas (falta)", "Riña pública") ~ "Threat, minor offense, or brawl",
      subgrupo %in% c("Consumo de drogas en la vía pública", "Porte de drogas", "Otras faltas ley de drogas", 
                      "Consumo de alcohol en la vía pública") ~ "Consumption of alcohol and drugs in public spaces",
      subgrupo == "Daños" ~ "Damages",
      subgrupo == "Desórdenes públicos" ~ "Public disorders",
      subgrupo %in% c("Animales sueltos en la vía pública", "Comercio ilegal", "Ofensas al pudor") ~ "Other incivilities",
      TRUE ~ "Other"
    ),
    
    # Assign familia based on subgrupo
    familia = case_when(
      subgrupo %in% c("Homicidios", "Femicidios", "Violaciones", "Abusos sexuales", "Acosos sexuales", 
                      "Otros delitos sexuales", "Lesiones graves o gravísimas", "Lesiones menos graves", 
                      "Lesiones leves", "Amenazas") ~ "Crimes against life or personal integrity",
      subgrupo %in% c("Robos con violencia o intimidación", "Robo violento de vehículo motorizado", 
                      "Robo por sorpresa") ~ "Violent robberies",
      subgrupo %in% c("VIF con lesiones físicas", "VIF con lesiones psicológicas", "Maltrato habitual", 
                      "Amenazas en contexto de VIF", "VIF no clasificada") ~ "Domestic violence",
      subgrupo %in% c("Tráfico de sustancias", "Microtráfico de sustancias", "Elaboración o producción de sustancias", 
                      "Otras infracciones a la ley de drogas") ~ "Drug-related crimes",
      subgrupo %in% c("Disparo injustificado", "Porte / posesión de armas o explosivos", 
                      "Otras infracciones a la ley de armas", "Porte de arma cortante o punzante") ~ "Weapon-related crimes",
      subgrupo %in% c("Robo en lugar habitado", "Robos en lugar no habitado", "Robo de vehículo motorizado", 
                      "Robo de objetos de o desde vehículo", "Otros robos con fuerza en las cosas", 
                      "Hurtos", "Receptación") ~ "Non-violent property crimes",
      subgrupo %in% c("Amenaza con armas (falta)", "Riña pública", "Consumo de drogas en la vía pública", 
                      "Porte de drogas", "Otras faltas ley de drogas", "Consumo de alcohol en la vía pública", 
                      "Daños", "Desórdenes públicos", "Animales sueltos en la vía pública", "Comercio ilegal", 
                      "Ofensas al pudor") ~ "Incivilities",
      TRUE ~ "Other"
    )
  ) |> 
  dplyr::select(-crime_upper) |> 
  rename(
    subgroup = subgrupo,
    group = grupo,
    family = familia
  )

## Check results ----
glimpse(crimes_classified)

# Distribution of classifications
crimes_classified |> writexl::write_xlsx("Data/Output/Crime_types_groups.xlsx")

# Places crimes -----
places <- rio::import(paste0(data_out, "Crime_places.xlsx"))  # crime places

## Explore unique place names ----
unique_places <- unique(places$place)
cat("Total unique places:", length(unique_places), "\n")
print(unique_places)

## Create place classification ----
places_classified <- places |> 
  mutate(
    place_upper = toupper(place),
    
    # Classify places into 5 categories
    place_category = case_when(
      # Public space / Espacio público
      str_detect(place_upper, "VIA.*PUBLICA|CALLE|AVENIDA|PLAZA|PARQUE|PLAZOLETA|PASEO|VEREDA|ACERA|ESPACIO.*PUBLICO|VIAL|CARRETERA|RUTA|CAMINO|PASARELA|PUENTE|PASEO.*PUBLICO|AREA.*VERDE|JARDIN.*PUBLICO|ALAMEDA|BOULEVARD|BANDERON|ESTACIONAMIENTO.*PUBLICO|ESTACIONAMIENTO.*CALLE|CRUCE.*SEMAFORO|CRUCE.*SEÑAL|CRUCE.*HABILITADO|CRUCE.*NO.*HABILITADO|CRUCE.*NO.*SEÑALIZADO|CRUCE.*REGULADO|CRUCE.*SEÑALIZADO|CRUCE.*SIN.*SEÑALIZACION|LARGO.*CALZADA|MAR.*PLAYA|MITAD.*CUADRA|MINISTERIO|MUNICIPALIDAD|MUSEO|REGISTRO.*CIVIL|ROTONDA|TRAMO.*VIA|TRAMO.*VIA.*RECTA|TRAMO.*VIA.*CURVA|TUNEL|VIA.*FERREA") ~ "Public space",
      
      # Residence / Residencia
      str_detect(place_upper, "CASA|HOGAR|VIVIENDA|DEPARTAMENTO|DEPTO|RESIDENCIA|DOMICILIO|HABITACION|CUARTO|PIEZA|SALA|COCINA|PATIO|JARDIN.*PARTICULAR|ESTACIONAMIENTO.*PARTICULAR|ESTACIONAMIENTO.*RESIDENCIAL|CONDOMINIO|EDIFICIO.*RESIDENCIAL|BLOCK|CAMPAMENTO|POBLACION|VILLA|CONJUNTO.*HABITACIONAL|LOTE|TERRENO.*PARTICULAR|INTERIOR.*INMUEBLE") ~ "Residence",
      
      # Work/study place / Lugar de trabajo/estudio
      str_detect(place_upper, "TRABAJO|OFICINA|FABRICA|TALLER|BODEGA|ALMACEN.*TRABAJO|CENTRO.*EDUCACIONAL|COLEGIO|ESCUELA|UNIVERSIDAD|INSTITUTO|JARDIN.*INFANTIL|SALA.*CLASE|AULA|BIBLIOTECA|LABORATORIO|GIMNASIO.*ESCOLAR|CANCHA.*ESCOLAR|ESTABLECIMIENTO.*EDUCACIONAL|ESTABLECIMIENTO.*EDUC|CENTRO.*FORMACION|LUGAR.*TRABAJO|SITIO.*TRABAJO|EMPRESA|NEGOCIO.*PARTICULAR") ~ "Work/study place",
      
      # Commercial space / Espacio comercial
      str_detect(place_upper, "COMERCIAL|TIENDA|SUPERMERCADO|MINIMARKET|FARMACIA|RESTAURANT|RESTORAN|CAFE|BAR|DISCOTECA|DISCOTTEQUE|PUB|LOCAL.*COMERCIAL|CENTRO.*COMERCIAL|MALL|GALERIA|MERCADO|FERIA|PUESTO|KIOSCO|BODEGA.*COMERCIAL|ALMACEN.*COMERCIAL|NEGOCIO|ESTABLECIMIENTO.*COMERCIAL|BANCO|CASA.*COMERCIAL|SUCURSAL|LOCAL|ESTACIONAMIENTO.*COMERCIAL|AUTOMOTORA|CASA.*DE.*CAMBIO|CENTRO.*DE.*PAGO|CINE|CIRCULO.*CLUB|CLANDESTINO|CLINICA|COMPAÑIA.*SEGURO|COMPAÑIA.*TELEFONICA|CONSTRUCTORA|DEP.*EXP.*BEBIDA|ESTADIO|FINANCIERA|FUNERARIA|HIPODROMO|HOSPITAL|HOTEL|MOTEL|IGLESIA|IMPRENTA|JOYERIA|PLANTA.*REVISION|SALON.*BELLEZA|SERVICENTRO|VIDEO.*CLUB") ~ "Commercial space",
      
      # Transportation / Transporte
      str_detect(place_upper, "VEHICULO|AUTOMOVIL|AUTO|MOTO|MOTOCICLETA|BICICLETA|BICI|BUS|MICRO|TAXI|COLECTIVO|TRANSPORTE.*PUBLICO|METRO|ESTACION.*METRO|ESTACION.*BUS|PARADERO|TERMINAL|AEROPUERTO|ESTACION.*FERROVIARIA|ESTACION.*FERROCARRIL|ESTACION.*TREN|ESTACIONAMIENTO.*VEHICULO|ESTACIONAMIENTO.*AUTOMOVIL|GARAJE|ESTACIONAMIENTO|PARKING|VEHICULO.*MOTORIZADO|CAMION|CAMIONETA|FURGON|FURGONETA|REMOLQUE|SEMIRREMOLQUE|VEHICULO.*PARTICULAR|VEHICULO.*PUBLICO|FERROCARRIL|NAVE.*AERONAVE|TERM.*LOCOM.*COLECTI") ~ "Transportation",
      
      TRUE ~ "Other"
    ),
    
    # Classify as Public or Private place
    place_type = case_when(
      place_category == "Public space" ~ "Public",
      place_category == "Transportation" ~ "Public",
      place_category == "Residence" ~ "Private",
      place_category == "Work/study place" ~ "Public",
      place_category == "Commercial space" ~ "Public",
      TRUE ~ "Other"
    )
  ) |> 
  dplyr::select(-place_upper)

## Check place classification results ----
glimpse(places_classified)

# Distribution of classifications
places_classified |> writexl::write_xlsx("Data/Output/Crime_places_groups.xlsx")

# Quad data ----
# Source: https://www.geoportal.cl/geoportal/catalog/35002/Plan%20Cuadrante
load_data <- "Data/Input/plan_cuadrante_2022"
quadrant_data <- st_read(paste0(load_data, "/layer_cuadrantes_20220309024642.shp")) |> 
  st_transform(crs = st_crs(4326)) |> 
  janitor::clean_names() |> 
  filter(zona == "METROPOLITANA")

glimpse(quadrant_data)

ggplot(data = quadrant_data) +
    geom_sf() + 
    theme_void() 

# Save quadrant data 
save(quadrant_data, file=paste0(data_out, "Info_geo_quadrant", ".RData"))
