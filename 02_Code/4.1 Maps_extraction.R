# Code 4.1: Map extraction ----

rm(list = ls())

## Settings ----
source("02_Code/0.1 Functions.R")
source("02_Code/0.2 Settings.R")

library(terra)
library(sf)
library(maptiles)

## Extracción ----
bounds <- st_bbox(
  c(xmin = -70.9, ymin = -33.6, xmax = -70.4, ymax = -33.3),
  crs = st_crs(4326)
)


# OpenStreetMap
scl_basemap_01 <- get_tiles(bounds, provider = "OpenStreetMap", zoom = 12)
scl_basemap_02 <- get_tiles(bounds, provider = "OpenStreetMap.DE", zoom = 12)
scl_basemap_03 <- get_tiles(bounds, provider = "OpenStreetMap.France", zoom = 12)
scl_basemap_04 <- get_tiles(bounds, provider = "OpenStreetMap.HOT", zoom = 12)
scl_basemap_05 <- get_tiles(bounds, provider = "OpenTopoMap", zoom = 12)

# Stadia (requiere API key: Sys.setenv("STADIA_MAPS" = "tu_key"))
#ggmap::register_google(key="AIzaSyBM1GkWmFypAVdIO7aHNRB0spuuQtqDVNw")
ggmap::register_stadiamaps(key="b8908535-0fbc-44be-a3f0-5bb5ee5c2890")
Sys.setenv("STADIA_MAPS" = "b8908535-0fbc-44be-a3f0-5bb5ee5c2890")
scl_basemap_06 <- get_tiles(bounds, provider = "Stadia.AlidadeSmooth", zoom = 12)
scl_basemap_07 <- get_tiles(bounds, provider = "Stadia.AlidadeSmoothDark", zoom = 12)
scl_basemap_08 <- get_tiles(bounds, provider = "Stadia.OSMBright", zoom = 12)
scl_basemap_09 <- get_tiles(bounds, provider = "Stadia.Outdoors", zoom = 12)
scl_basemap_10 <- get_tiles(bounds, provider = "Stadia.StamenToner", zoom = 12)
scl_basemap_11 <- get_tiles(bounds, provider = "Stadia.StamenTonerBackground", zoom = 12)
scl_basemap_12 <- get_tiles(bounds, provider = "Stadia.StamenTonerLines", zoom = 12)
scl_basemap_13 <- get_tiles(bounds, provider = "Stadia.StamenTonerLabels", zoom = 12)
scl_basemap_14 <- get_tiles(bounds, provider = "Stadia.StamenTonerLite", zoom = 12)
scl_basemap_15 <- get_tiles(bounds, provider = "Stadia.StamenWatercolor", zoom = 12)
scl_basemap_16 <- get_tiles(bounds, provider = "Stadia.StamenTerrain", zoom = 12)
scl_basemap_17 <- get_tiles(bounds, provider = "Stadia.StamenTerrainBackground", zoom = 12)
scl_basemap_18 <- get_tiles(bounds, provider = "Stadia.StamenTerrainLabels", zoom = 12)

# Esri
scl_basemap_19 <- get_tiles(bounds, provider = "Esri.WorldStreetMap", zoom = 12)
scl_basemap_20 <- get_tiles(bounds, provider = "Esri.WorldTopoMap", zoom = 12)
scl_basemap_21 <- get_tiles(bounds, provider = "Esri.WorldImagery", zoom = 12)
scl_basemap_22 <- get_tiles(bounds, provider = "Esri.WorldTerrain", zoom = 12)
scl_basemap_23 <- get_tiles(bounds, provider = "Esri.WorldShadedRelief", zoom = 12)
scl_basemap_24 <- get_tiles(bounds, provider = "Esri.OceanBasemap", zoom = 12)
scl_basemap_25 <- get_tiles(bounds, provider = "Esri.NatGeoWorldMap", zoom = 12)
scl_basemap_26 <- get_tiles(bounds, provider = "Esri.WorldGrayCanvas", zoom = 12)

# CartoDB
scl_basemap_27 <- get_tiles(bounds, provider = "CartoDB.Positron", zoom = 12)
scl_basemap_28 <- get_tiles(bounds, provider = "CartoDB.PositronNoLabels", zoom = 12)
scl_basemap_29 <- get_tiles(bounds, provider = "CartoDB.PositronOnlyLabels", zoom = 12)
scl_basemap_30 <- get_tiles(bounds, provider = "CartoDB.DarkMatter", zoom = 12)
scl_basemap_31 <- get_tiles(bounds, provider = "CartoDB.DarkMatterNoLabels", zoom = 12)
scl_basemap_32 <- get_tiles(bounds, provider = "CartoDB.DarkMatterOnlyLabels", zoom = 12)
scl_basemap_33 <- get_tiles(bounds, provider = "CartoDB.Voyager", zoom = 12)
scl_basemap_34 <- get_tiles(bounds, provider = "CartoDB.VoyagerNoLabels", zoom = 12)
scl_basemap_35 <- get_tiles(bounds, provider = "CartoDB.VoyagerOnlyLabels", zoom = 12)

# Thunderforest (requiere API key: Sys.setenv("THUNDERFOREST_MAPS" = "tu_key"))
Sys.setenv("THUNDERFOREST_MAPS" = "ef4c58165ecc4131a87ae68952b84e02")
scl_basemap_36 <- get_tiles(bounds, provider = "Thunderforest.OpenCycleMap", zoom = 12)
scl_basemap_37 <- get_tiles(bounds, provider = "Thunderforest.Transport", zoom = 12)
scl_basemap_38 <- get_tiles(bounds, provider = "Thunderforest.TransportDark", zoom = 12)
scl_basemap_39 <- get_tiles(bounds, provider = "Thunderforest.SpinalMap", zoom = 12)
scl_basemap_40 <- get_tiles(bounds, provider = "Thunderforest.Landscape", zoom = 12)
scl_basemap_41 <- get_tiles(bounds, provider = "Thunderforest.Outdoors", zoom = 12)
scl_basemap_42 <- get_tiles(bounds, provider = "Thunderforest.Pioneer", zoom = 12)
scl_basemap_43 <- get_tiles(bounds, provider = "Thunderforest.MobileAtlas", zoom = 12)
scl_basemap_44 <- get_tiles(bounds, provider = "Thunderforest.Neighbourhood", zoom = 12)

## Plot de todos los mapas ----
providers <- c(
  "OpenStreetMap", "OpenStreetMap.DE", "OpenStreetMap.France", "OpenStreetMap.HOT", "OpenTopoMap",
  "Stadia.AlidadeSmooth", "Stadia.AlidadeSmoothDark", "Stadia.OSMBright", "Stadia.Outdoors",
  "Stadia.StamenToner", "Stadia.StamenTonerBackground", "Stadia.StamenTonerLines", "Stadia.StamenTonerLabels",
  "Stadia.StamenTonerLite", "Stadia.StamenWatercolor", "Stadia.StamenTerrain", "Stadia.StamenTerrainBackground", "Stadia.StamenTerrainLabels",
  "Esri.WorldStreetMap", "Esri.WorldTopoMap", "Esri.WorldImagery", "Esri.WorldTerrain", "Esri.WorldShadedRelief",
  "Esri.OceanBasemap", "Esri.NatGeoWorldMap", "Esri.WorldGrayCanvas",
  "CartoDB.Positron", "CartoDB.PositronNoLabels", "CartoDB.PositronOnlyLabels", "CartoDB.DarkMatter",
  "CartoDB.DarkMatterNoLabels", "CartoDB.DarkMatterOnlyLabels", "CartoDB.Voyager", "CartoDB.VoyagerNoLabels", "CartoDB.VoyagerOnlyLabels",
  "Thunderforest.OpenCycleMap", "Thunderforest.Transport", "Thunderforest.TransportDark", "Thunderforest.SpinalMap",
  "Thunderforest.Landscape", "Thunderforest.Outdoors", "Thunderforest.Pioneer", "Thunderforest.MobileAtlas", "Thunderforest.Neighbourhood"
)

maps_list <- mget(ls(pattern = "^scl_basemap_[0-9]+$"))

for (i in seq_along(maps_list)) {
  plotRGB(maps_list[[i]])
  title(main = paste0(i, ". ", providers[i]))
}

# 26
# 28 
# 

plot(scl_basemap_16)
plot(scl_basemap_20)
plot(scl_basemap_26)
plot(scl_basemap_28)
plot(scl_basemap_31)
plot(scl_basemap_33)
plot(scl_basemap_34)
plot(scl_basemap_40)
plot(scl_basemap_40)


scl_basemap_28 <- get_tiles(bounds, provider = "CartoDB.PositronNoLabels", zoom = 12, retina = TRUE, crop = TRUE, forceDownload=TRUE)
plot(scl_basemap_28)

# zoom 14: ~60 m por tile (más detalle)
# zoom 15: ~30 m por tile (muy detallado)
scl_basemap_28 <- get_tiles(bounds, provider = "CartoDB.PositronNoLabels", zoom = 15, retina = TRUE, crop = TRUE)
plot(scl_basemap_28)