# Code 4.0: Censo 2017 (chilemapas) + CPV 2024 (cartografia INE R13) ----
#
# Solo generacion de datos (sf + poblacion + densidad). Mapas: 5.0 Descriptives_analysis.R
#
# Densidad: siempre superficie desde geometria en EPSG:31979 (m^2 -> km^2).
#   No usar SHAPE_Area del parquet INE para densidad: viene en unidades de grado^2 (WGS84).
#
# Salidas:
#   - censo_RM_zonas_2017_resultados.RData  -> resultados_censo_rm_2017
#   - censo_RM_zonas_2024_resultados.RData  -> resultados_censo_rm_2024
#
# CPV 2024: insumos en 01_Data/CENSO (si existe) o 01_Data/Input/CENSO/
#   Parquet: .../*_R13/*Zonal.parquet

rm(list = ls())

## Settings ----
source("02_Code/0.1 Functions.R")
source("02_Code/0.2 Settings.R")

output <- "03_Output/Descriptives/"
dir.create(output, showWarnings = FALSE, recursive = TRUE)
crs_metrica <- "EPSG:31979"

## Superficie real (km2) desde geometria en CRS metrico ----
area_km2_desde_geom_31979 <- function(geo_sf) {
  geo_sf |>
    sf::st_transform(crs_metrica) |>
    dplyr::mutate(
      area_km2_zona_completa_31979 = as.numeric(sf::st_area(.data$geometry)) / 1e6
    ) |>
    sf::st_transform(4326)
}

## Area urbana (misma logica que 5.0 Descriptives_analysis.R) ----
urban_island <- c(
  "13124071004", "13124071005", "13124081001", "13124071001", "13124071002", "13124071003",
  "13401121001",
  "13119131001",
  "13203031000", "13203031001", "13203031002", "13203011001", "13203011002"
)

stgo_urb <- chilemapas::mapa_zonas |>
  dplyr::filter(as.numeric(codigo_region) == 13) |>
  left_join(
    chilemapas::codigos_territoriales |>
      dplyr::select(dplyr::matches("comuna"))
  ) |>
  dplyr::filter(
    codigo_provincia %in% c(131, 132) | nombre_comuna == "San Bernardo",
    nombre_comuna != "Pirque"
  ) |>
  dplyr::filter(!geocodigo %in% urban_island) |>
  group_by(nombre_comuna, codigo_comuna) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  dplyr::mutate(codigo_comuna = as.numeric(codigo_comuna)) |>
  st_as_sf() |>
  st_transform(4326)

stgo_urb_union <- st_union(stgo_urb)

## Recorte urbano + densidad con area en 31979 (post-recorte), nunca SHAPE_Area del archivo ----
clip_y_densidad <- function(geo_sf) {
  sf::st_intersection(
    sf::st_make_valid(geo_sf),
    sf::st_make_valid(stgo_urb_union)
  ) |>
    sf::st_transform(crs_metrica) |>
    dplyr::mutate(
      # st_area en EPSG:31979 -> metros cuadrados; /1e6 -> km2 (fragmento tras recorte urbano)
      area_km2 = as.numeric(sf::st_area(.data$geometry)) / 1e6
    ) |>
    sf::st_transform(4326) |>
    dplyr::mutate(
      # Umbral 1e-3 km2 (~1e3 m2); evita fragmentos del recorte con area_km2 muy pequena y densidad extrema
      densidad_hab_km2 = dplyr::if_else(
        !is.na(.data$poblacion) & .data$area_km2 >= 1e-3,
        .data$poblacion / .data$area_km2,
        NA_real_
      )
    )
}

## ----- Censo 2017 (chilemapas + censo_2017_zonas) -----
pop_zona_2017 <- censo_2017_zonas |>
  dplyr::filter(substr(.data$geocodigo, 1, 2) == "13") |>
  dplyr::group_by(.data$geocodigo) |>
  dplyr::summarise(poblacion = sum(.data$poblacion, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(geocodigo = as.character(.data$geocodigo))

geo_rm_2017 <- chilemapas::mapa_zonas |>
  dplyr::filter(as.numeric(.data$codigo_region) == 13) |>
  sf::st_as_sf() |>
  dplyr::left_join(pop_zona_2017, by = "geocodigo") |>
  dplyr::mutate(
    poblacion = .data$poblacion,
    geocodigo = as.character(.data$geocodigo)
  ) |>
  sf::st_transform(4326)

geo_rm_2017 <- area_km2_desde_geom_31979(geo_rm_2017)
geo_rm_2017 <- clip_y_densidad(geo_rm_2017)

resultados_censo_rm_2017 <- list(
  sf_zonas = geo_rm_2017,
  poblacion_por_zona = pop_zona_2017,
  stgo_urb = stgo_urb,
  crs_area = crs_metrica,
  nota_unidad = paste0(
    "Zona censal Censo 2017 (chilemapas + censo_2017_zonas). ",
    "area_km2 y densidad_hab_km2 usan st_area en ",
    crs_metrica,
    " (no grados geograficos)."
  )
)

save(
  resultados_censo_rm_2017,
  file = paste0(output, "censo_RM_zonas_2017_resultados.RData")
)

## ----- CPV 2024 (parquet zonal R13) -----
censo_root <- if (dir.exists("01_Data/CENSO")) {
  "01_Data/CENSO"
} else {
  "01_Data/Input/CENSO"
}
if (!dir.exists(censo_root)) {
  stop("No se encuentra carpeta CENSO. Crear ", censo_root, " o enlazar datos CPV 2024.")
}
cart_dir <- Sys.glob(file.path(censo_root, "*_R13"))
if (length(cart_dir) == 0L) {
  stop("No hay subcarpeta *_R13 en ", censo_root, " (cartografia regional CENSO 2024).")
}
cart_dir <- cart_dir[1L]
zonal_path <- Sys.glob(file.path(cart_dir, "*Zonal.parquet"))
if (length(zonal_path) == 0L) {
  stop("No se encontro *Zonal.parquet en ", cart_dir)
}
zonal_path <- zonal_path[1L]

crs_wkb <- "EPSG:4326"

cols_zonal <- c(
  "CUT", "COD_REGION", "COMUNA", "COD_ZONA", "ID_ZONA",
  "n_per", "n_mujeres", "SHAPE", "SHAPE_Area"
)
d_zonal <- arrow::read_parquet(zonal_path, col_select = dplyr::all_of(cols_zonal))
missing_cols <- setdiff(cols_zonal, names(d_zonal))
if (length(missing_cols)) {
  stop("Faltan columnas en Zonal.parquet: ", paste(missing_cols, collapse = ", "))
}
d_zonal <- d_zonal |>
  dplyr::filter(.data$COD_REGION == 13L) |>
  dplyr::mutate(
    geocodigo = as.character(.data$ID_ZONA),
    poblacion = as.numeric(.data$n_per),
    n_mujeres = as.numeric(.data$n_mujeres),
    codigo_comuna = as.numeric(.data$CUT)
  )

geom_zonal <- sf::st_as_sfc(
  structure(d_zonal$SHAPE, class = "WKB"),
  crs = crs_wkb
)
d_zonal$SHAPE <- NULL

geo_rm_2024 <- sf::st_sf(d_zonal, geometry = geom_zonal, sf_column_name = "geometry") |>
  dplyr::rename(
    # Valor del insumo INE: area en "grado^2" bajo WGS84; no usar para densidad ni como km2
    ine_shape_area_cuadrado_grados2 = "SHAPE_Area"
  )

pop_zona_2024 <- geo_rm_2024 |>
  sf::st_drop_geometry() |>
  dplyr::distinct(.data$geocodigo, .keep_all = TRUE) |>
  dplyr::select("geocodigo", "poblacion", "n_mujeres")

geo_rm_2024 <- geo_rm_2024 |>
  sf::st_transform(4326)

geo_rm_2024 <- area_km2_desde_geom_31979(geo_rm_2024)
geo_rm_2024 <- clip_y_densidad(geo_rm_2024) |>
  dplyr::mutate(
    densidad_mujeres_hab_km2 = dplyr::if_else(
      !is.na(.data$n_mujeres) & .data$area_km2 >= 1e-3,
      .data$n_mujeres / .data$area_km2,
      NA_real_
    )
  )

resultados_censo_rm_2024 <- list(
  sf_zonas = geo_rm_2024,
  poblacion_por_zona = pop_zona_2024,
  stgo_urb = stgo_urb,
  crs_area = crs_metrica,
  nota_unidad = paste0(
    "CPV 2024, cartografia zonal INE R13 (",
    basename(zonal_path),
    "). ID_ZONA = geocodigo; poblacion = n_per; n_mujeres en poblacion_por_zona. ",
    "ine_shape_area_cuadrado_grados2 = SHAPE_Area del archivo (unidades grado^2; solo referencia). ",
    "area_km2_zona_completa_31979 y area_km2 (post-recorte); densidad_hab_km2 y densidad_mujeres_hab_km2 usan st_area en ",
    crs_metrica,
    "."
  )
)

save(
  resultados_censo_rm_2024,
  file = paste0(output, "censo_RM_zonas_2024_resultados.RData")
)
