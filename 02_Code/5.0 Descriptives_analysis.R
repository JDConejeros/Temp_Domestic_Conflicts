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
  left_join(ndv, by = c("com_code"="code_district", "quadrant"))

# Color Palette: (basada en paleta de desigualdad de dominicroye)
vi_pal <- c(
  "#E8F4F8",
  "#B3D9E6",
  "#7FB8D1",
  "#4A9BC4",
  "#1E5F8B")

map_ndv |> 
  #filter(zone == "Urban") |>
  ggplot() +
  geom_sf(aes(geometry = geometry, fill=q5), lwd = 0.5, color = "white") +
  scale_fill_manual(
    name = "Reporting Intrafamily Violence\n(2017-2025)",
    values = vi_pal,
    na.value = "gray95",
    na.translate = FALSE
    ) +
  theme_light() +
   theme(
    legend.position = c(0.8, 0.2),  
    legend.key.size = unit(0.6, "cm"), 
    legend.text = element_text(size = 10), 
    legend.title = element_text(size = 10, face = "bold"), 
    plot.margin = margin(t = 10, r = 10, b = 5, l = 10),
    panel.grid = element_blank(),
    strip.text.y = element_text(angle = 0),
    strip.background = element_rect(fill = NA, color = "gray70"),
    strip.text = element_text(color = "black"),
    strip.text.y.left = element_text(angle = 0)
  )

library("ggmap")

ggmap::register_google(key="AIzaSyBM1GkWmFypAVdIO7aHNRB0spuuQtqDVNw")
ggmap::register_stadiamaps(key="b8908535-0fbc-44be-a3f0-5bb5ee5c2890")

## Opciones de Mapas Disponibles ----

### get_stadiamap() - Tipos de Mapas (maptype):
# Basemaps completos:
#   - "stamen_terrain"           # Terreno con relieve
#   - "stamen_toner"             # Toner oscuro
#   - "stamen_toner_lite"         # Toner claro
#   - "stamen_watercolor"         # Estilo acuarela
#   - "alidade_smooth"           # Alidade suave
#   - "alidade_smooth_dark"       # Alidade suave oscuro
#   - "outdoors"                  # Exterior

# Capas de fondo:
#   - "stamen_terrain_background" # Fondo de terreno
#   - "stamen_toner_background"  # Fondo toner

# Capas transparentes (líneas y etiquetas):
#   - "stamen_terrain_lines"      # Líneas de terreno
#   - "stamen_terrain_labels"     # Etiquetas de terreno
#   - "stamen_toner_lines"        # Líneas toner
#   - "stamen_toner_labels"       # Etiquetas toner

# Ejemplos get_stadiamap():
# get_stadiamap(bbox, zoom = 12, maptype = "stamen_terrain") |> ggmap()
# get_stadiamap(bbox, zoom = 12, maptype = "stamen_toner") |> ggmap()
# get_stadiamap(bbox, zoom = 12, maptype = "stamen_toner_lite") |> ggmap()
# get_stadiamap(bbox, zoom = 12, maptype = "stamen_watercolor") |> ggmap()
# get_stadiamap(bbox, zoom = 12, maptype = "stamen_terrain_lines") |> ggmap()
# get_stadiamap(bbox, zoom = 12, maptype = "stamen_terrain_labels") |> ggmap()

### get_googlemap() - Tipos de Mapas (maptype):
#   - "terrain"    # Terreno con relieve y características físicas
#   - "satellite"   # Imágenes satelitales
#   - "roadmap"    # Mapa de carreteras estándar (por defecto)
#   - "hybrid"      # Híbrido: satelital con etiquetas de carreteras

# Parámetros adicionales get_googlemap():
#   - source: "google" o "stamen" (fuente del mapa)
#   - zoom: nivel de zoom (3 = continente, 21 = edificio, default = 10)
#   - scale: factor de densidad de píxeles (1, 2, o 4)
#   - format: formato de imagen ("png8", "gif", "jpg", "jpg-baseline", "png32")
#   - color: "color" o "bw" (blanco y negro)
#   - language: idioma de etiquetas (ej: "es", "en")

# Ejemplos get_googlemap():
get_googlemap("Región Metropolitana", zoom = 11, maptype = "terrain", scale=4) |> ggmap()
get_googlemap("Región Metropolitana", zoom = 11, maptype = "satellite", scale=4) |> ggmap()
get_googlemap("Región Metropolitana", zoom = 11, maptype = "roadmap", scale=4) |> ggmap()
get_googlemap("Región Metropolitana", zoom = 11, maptype = "hybrid", scale=4) |> ggmap()

bbox <- c(left = -70.8, bottom = -33.65, right = -70.5, top = -33.30)
get_stadiamap(bbox, zoom = 12, maptype = "stamen_terrain") |> ggmap()
get_stadiamap(bbox, zoom = 12, maptype = "stamen_toner") |> ggmap()
get_stadiamap(bbox, zoom = 12, maptype = "stamen_toner_lite") |> ggmap()
get_stadiamap(bbox, zoom = 12, maptype = "stamen_watercolor") |> ggmap()
get_stadiamap(bbox, zoom = 12, maptype = "stamen_terrain_lines") |> ggmap()
get_stadiamap(bbox, zoom = 12, maptype = "stamen_terrain_labels") |> ggmap()


## Pruebas de Mapas ----

us <- c(left = -70.8, bottom = -33.65, right = -70.5, top = -33.30)

# Stadia Maps
get_stadiamap(us, zoom = 12, maptype = "stamen_terrain_lines") |> ggmap() + geom_sf(data=map_ndv, aes(geometry = geometry, fill=q5), lwd = 0.5, color = "white") 
get_stadiamap(us, zoom = 12, maptype = "stamen_toner_lite") |> ggmap()
get_stadiamap(us, zoom = 12, maptype = "stamen_watercolor") |> ggmap()

# Google Maps
get_googlemap("Región Metropolitana", source = "stamen", zoom = 11, maptype = "terrain", scale=4) |> ggmap()
get_googlemap("Región Metropolitana", zoom = 11, maptype = "satellite", scale=4) |> ggmap()
get_googlemap("Región Metropolitana", zoom = 11, maptype = "roadmap", scale=4) |> ggmap()
get_googlemap("Región Metropolitana", zoom = 11, maptype = "hybrid", scale=4) |> ggmap()


test <- get_stadiamap(us, zoom = 12, maptype = "stamen_terrain_lines") 
ggmap(test) + geom_sf(data=map_ndv, aes(geometry = geometry, fill=q5), lwd = 0.5, color = "white") 
