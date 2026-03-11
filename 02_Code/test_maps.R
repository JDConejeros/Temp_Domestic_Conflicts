library(terra)  # o raster
basemap <- rast("scl_basemap.tif")
glimpse(basemap)
basemap
plot(basemap)


library(sf)
library(terra)
library(ggplot2)
library(ggspatial)

# Cargar raster
#basemap <- rast("data/presidenciales-2025-1ra/scl_basemap.tif")
# Convertir a objeto para ggplot si es necesario

library(maptiles)
library(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyterra)  # para geom_spatraster_rgb (install.packages("tidyterra") si falta)

# Bounds de Santiago (ejemplo) - con CRS WGS84 explícito
bounds <- st_bbox(
  c(xmin = -70.9, ymin = -33.6, xmax = -70.4, ymax = -33.3),
  crs = st_crs(4326)
)

# Descargar tiles CartoDB Positron
scl_basemap <- get_tiles(bounds, provider = "CartoDB.PositronNoLabels", zoom = 12)


# Mapa con tiles + comunas + etiquetas
ggplot() +
  geom_spatraster_rgb(data = scl_basemap) +
  #geom_sf(data = comunas_scl, fill = NA, color = "gray40", linewidth = 0.5) +
  #geom_text_repel(
  #  data = comunas_centroids,
  #  aes(x = X, y = Y, label = nombre_comuna),
  #  size = 3,
  #  fontface = "bold",
  #  color = "black",
  #  max.overlaps = Inf,
  #  box.padding = 0.3,
  #  min.segment.length = 0
  #) +
  #coord_sf(expand = FALSE) +
  theme_void()
