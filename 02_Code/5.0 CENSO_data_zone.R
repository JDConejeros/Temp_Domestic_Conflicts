# Code 1.0: Census 2017 (chilemapas) + CPV 2024 (INE cartography R13) ----
#
# Census zone characteristics and geometries
#
# Outputs:
#   - censo_RM_zonas_2017_resultados.RData  -> census_rm_2017_results
#   - censo_RM_zonas_2024_resultados.RData  -> census_rm_2024_results
#

## Settings ----
source("02_Code/0.1 Functions.R")
source("02_Code/0.2 Settings.R")

## Data path ----
data_inp <- "01_Data/Input/CENSO/"
output <- "01_Data/Output/"
# INE WKB geometries are geographic (WGS84); use with st_area only after projecting.
crs_wkb <- sf::st_crs(4326)
metric_crs <- "EPSG:31979"

## Area (km2) from projected geometry (full zone, before metro clip) ----
add_area_km2_zone_full <- function(geo_sf) {
  geo_sf |>
    sf::st_transform(metric_crs) |>
    dplyr::mutate(
      area_km2_zone_full = as.numeric(sf::st_area(.data$geometry)) / 1e6
    ) |>
    sf::st_transform(4326)
}

## Remove urban islands: metropolitan study area ----
urban_island <- c(
  "13124071004", "13124071005", "13124081001", "13124071001", "13124071002", "13124071003",
  "13401121001",
  "13119131001",
  "13203031000", "13203031001", "13203031002", "13203011001", "13203011002"
)

stgo_urb <- chilemapas::mapa_zonas |>
  dplyr::filter(as.numeric(codigo_region) == 13) |>
  dplyr::left_join(
    chilemapas::codigos_territoriales |>
      dplyr::select(dplyr::matches("comuna"))
  ) |>
  dplyr::filter(
    codigo_provincia %in% c(131, 132) | nombre_comuna == "San Bernardo",
    nombre_comuna != "Pirque"
  ) |>
  dplyr::filter(!geocodigo %in% urban_island) |>
  dplyr::group_by(nombre_comuna, codigo_comuna) |>
  dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
  dplyr::mutate(codigo_comuna = as.numeric(codigo_comuna)) |>
  sf::st_as_sf() |>
  sf::st_transform(4326)

stgo_urb_union <- sf::st_union(stgo_urb)

## Clip to metro union; km2 and densities use st_area in metric_crs (meters), not square degrees ----
clip_metro_area_and_density <- function(geo_sf) {
  sf::st_intersection(
    sf::st_make_valid(geo_sf),
    sf::st_make_valid(stgo_urb_union)
  ) |>
    sf::st_transform(metric_crs) |>
    dplyr::mutate(
      # Clipped polygon area (km2); replaces interpretation of full-zone area for local density
      area_km2 = as.numeric(sf::st_area(.data$geometry)) / 1e6,
      # Threshold 1e-3 km2 (~1e3 m2): avoids extreme densities on sliver fragments
      population_density_per_km2 = dplyr::if_else(
        !is.na(.data$population) & .data$area_km2 >= 1e-3,
        .data$population / .data$area_km2,
        NA_real_
      )
    ) |>
    sf::st_transform(4326)
}

## ----- Census 2017 (chilemapas + censo_2017_zonas): reference ----
pop_zona_2017 <- censo_2017_zonas |>
  dplyr::filter(substr(.data$geocodigo, 1, 2) == "13") |>
  dplyr::group_by(.data$geocodigo) |>
  dplyr::summarise(population = sum(.data$poblacion, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(geocodigo = as.character(.data$geocodigo))

geo_rm_2017 <- chilemapas::mapa_zonas |>
  dplyr::filter(as.numeric(.data$codigo_region) == 13) |>
  sf::st_as_sf() |>
  dplyr::left_join(pop_zona_2017, by = "geocodigo") |>
  dplyr::mutate(
    geocodigo = as.character(.data$geocodigo)
  ) |>
  sf::st_transform(4326)

geo_rm_2017 <- add_area_km2_zone_full(geo_rm_2017)
geo_rm_2017 <- clip_metro_area_and_density(geo_rm_2017)

summary(geo_rm_2017)
plot(geo_rm_2017)

census_rm_2017_results <- list(
  sf_zones = geo_rm_2017,
  population_by_zone = pop_zona_2017,
  metro_sample_frame = stgo_urb,
  projected_crs_for_area = metric_crs,
  notes = paste0(
    "2017 census zones (chilemapas + censo_2017_zonas). ",
    "area_km2_zone_full = full zone area (km2) from st_area in ",
    metric_crs,
    "; area_km2 = clipped area after metro intersection. ",
    "population_density_per_km2 uses population / area_km2 (not square degrees)."
  )
)

save(
  census_rm_2017_results,
  file = paste0(output, "censo_RM_zonas_censales_2017_resultados.RData")
)

## ----- CPV 2024 (zonal parquet, R13) ----
zonal_path <- Sys.glob(file.path(paste0(data_inp, "Cartografía_censo2024_R13"), "*Zonal.parquet"))

cols_zonal <- c(
  "CUT", "COD_REGION", "REGION", "COMUNA", "COD_ZONA", "ID_ZONA", "AREA_C",
  "n_per", "n_mujeres", "n_hombres",
  "n_edad_0_5", "n_edad_6_13", "n_edad_14_17", "n_edad_18_24", "n_edad_25_44", "n_edad_45_59", "n_edad_60_mas", "prom_edad",
  "n_jefatura_mujer",
  "n_inmigrantes",
  "prom_escolaridad18",
  "n_cine_primaria",
  "n_cine_secundaria",
  "n_cine_terciaria_maestria_doctorado",
  "n_ocupado", "n_desocupado",
  "n_hog",
  "n_vp",
  "n_viv_hacinadas",
  "n_vp_ocupada",
  "n_deficit_cuantitativo",
  "n_mat_paredes_hormigon",
  "n_mat_paredes_albanileria",
  "n_mat_paredes_tabique_forrado",
  "n_mat_paredes_tabique_sin_forro",
  "n_mat_paredes_artesanal",
  "n_mat_paredes_precarios",
  "n_mat_techo_tejas",
  "n_mat_techo_hormigon",
  "n_mat_techo_zinc",
  "n_mat_techo_fibrocemento",
  "n_mat_techo_fonolita",
  "n_mat_techo_paja",
  "n_mat_techo_precarios",
  "n_mat_techo_sin_cubierta",
  "n_mat_piso_radier_con_revestimiento",
  "n_mat_piso_radier_sin_revestimiento",
  "n_mat_piso_baldosa_cemento",
  "n_mat_piso_capa_cemento",
  "n_mat_piso_tierra",
  "SHAPE_Length", "SHAPE_Area",
  "SHAPE",
  "SHAPE_bbox"
)

d_zonal <- arrow::read_parquet(zonal_path, col_select = dplyr::all_of(cols_zonal)) |>
  dplyr::rename(zone_area_class = "AREA_C")

d_zonal <- d_zonal |>
  dplyr::filter(COD_REGION == 13L) |>
  dplyr::filter(n_per > 0) |>
  dplyr::mutate(
    geo_code = as.character(ID_ZONA),
    commune_code = as.numeric(CUT),
    population = n_per,
    female_population = n_mujeres
  ) |>
  dplyr::group_by(geo_code) |>
  dplyr::mutate(
    ## Demography
    sex_ratio_index = (n_hombres / n_mujeres) * 100,
    aging_index = (n_edad_60_mas / (n_edad_0_5 + n_edad_6_13)) * 100,
    pct_children = (n_edad_0_5 + n_edad_6_13) / n_per,
    pct_age_60_plus = n_edad_60_mas / n_per,
    pct_female_headed_hh = n_jefatura_mujer / n_hog,
    pct_immigrants = n_inmigrantes / n_per,
    ## Education
    pct_edu_primary = n_cine_primaria / n_per,
    pct_edu_secondary = n_cine_secundaria / n_per,
    pct_edu_tertiary = n_cine_terciaria_maestria_doctorado / n_per,
    ## Housing (INE denominator: occupied private dwellings)
    pct_housing_quantitative_deficit = n_deficit_cuantitativo / n_vp_ocupada,
    pct_overcrowded_units = n_viv_hacinadas / n_vp_ocupada,
    ## Dwelling materials (heat / vulnerability literature)
    pct_wall_concrete = n_mat_paredes_hormigon / n_vp_ocupada,
    pct_wall_masonry = n_mat_paredes_albanileria / n_vp_ocupada,
    pct_wall_studding_lined = n_mat_paredes_tabique_forrado / n_vp_ocupada,
    pct_wall_studding_unlined = n_mat_paredes_tabique_sin_forro / n_vp_ocupada,
    pct_wall_artisanal = n_mat_paredes_artesanal / n_vp_ocupada,
    pct_wall_precarious = n_mat_paredes_precarios / n_vp_ocupada,
    pct_roof_tiles = n_mat_techo_tejas / n_vp_ocupada,
    pct_roof_concrete = n_mat_techo_hormigon / n_vp_ocupada,
    pct_roof_zinc = n_mat_techo_zinc / n_vp_ocupada,
    pct_roof_fiber_cement = n_mat_techo_fibrocemento / n_vp_ocupada,
    pct_roof_precarious = n_mat_techo_precarios / n_vp_ocupada,
    pct_floor_slab_finished = n_mat_piso_radier_con_revestimiento / n_vp_ocupada,
    pct_floor_slab_unfinished = n_mat_piso_radier_sin_revestimiento / n_vp_ocupada,
    pct_floor_earth = n_mat_piso_tierra / n_vp_ocupada,
    ## Rates per 10,000 people
    rate_females_per_10k = (n_mujeres / n_per) * 10000,
    rate_children_per_10k = ((n_edad_0_5 + n_edad_6_13) / n_per) * 10000,
    rate_age_60_plus_per_10k = (n_edad_60_mas / n_per) * 10000,
    rate_immigrants_per_10k = (n_inmigrantes / n_per) * 10000,
    ## Housing material scores (literature weights on dwelling counts; higher = more “solid” stock)
    score_wall =
      2 * n_mat_paredes_hormigon +
      2 * n_mat_paredes_albanileria +
      1 * n_mat_paredes_tabique_forrado +
      0 * n_mat_paredes_tabique_sin_forro +
      -1 * n_mat_paredes_artesanal +
      -2 * n_mat_paredes_precarios,
    score_roof =
      2 * n_mat_techo_tejas +
      2 * n_mat_techo_hormigon +
      0 * n_mat_techo_zinc +
      0 * n_mat_techo_fibrocemento +
      -2 * n_mat_techo_precarios +
      -3 * n_mat_techo_sin_cubierta,
    score_floor =
      2 * n_mat_piso_radier_con_revestimiento +
      1 * n_mat_piso_radier_sin_revestimiento +
      0 * n_mat_piso_baldosa_cemento +
      -1 * n_mat_piso_capa_cemento +
      -2 * n_mat_piso_tierra,
    score_overcrowding = -3 * n_viv_hacinadas,
    ## Vulnerability index: negate the composite so LOW = lower vulnerability, HIGH = higher
    ## vulnerability (more precarious materials, worse roofs/floors, overcrowding raise the index).
    housing_heat_vulnerability_index =
      -(score_wall + score_roof + score_floor + score_overcrowding) / n_vp_ocupada
  ) |>
  dplyr::ungroup()

geom_zonal <- sf::st_as_sfc(
  structure(d_zonal$SHAPE, class = "WKB"),
  crs = crs_wkb
)
d_zonal$SHAPE <- NULL

geo_rm_2024 <- sf::st_sf(d_zonal, geometry = geom_zonal, sf_column_name = "geometry") |>
  dplyr::rename(
    # INE SHAPE_Area: plane metric under geographic CRS → square degrees (not km2 or m2)
    ine_shape_area_square_degrees = "SHAPE_Area"
  )

pop_zona_2024 <- geo_rm_2024 |>
  sf::st_drop_geometry() |>
  dplyr::distinct(.data$geo_code, .keep_all = TRUE)

geo_rm_2024 <- geo_rm_2024 |>
  sf::st_transform(4326)

geo_rm_2024 <- add_area_km2_zone_full(geo_rm_2024)
geo_rm_2024 <- clip_metro_area_and_density(geo_rm_2024)

census_rm_2024_results <- list(
  sf_zones = geo_rm_2024,
  population_by_zone = pop_zona_2024,
  metro_sample_frame = stgo_urb,
  projected_crs_for_area = metric_crs,
  notes = paste0(
    "CPV 2024, INE zonal cartography R13 (",
    basename(zonal_path),
    "). ID_ZONA maps to geo_code; population equals n_per. ",
    "zone_area_class is the INE AREA_C field (urban/rural label in this extract, not geometric area). ",
    "ine_shape_area_square_degrees is SHAPE_Area from the file (square degrees in WGS84; reference only). ",
    "area_km2_zone_full and area_km2 (clipped) come from st_area in ",
    metric_crs,
    " (meters → km2). ",
    "population_density_per_km2 uses clipped area_km2. ",
    "housing_heat_vulnerability_index increases with material/structural heat risk (negated weighted material-quality sum per occupied dwelling)."
  )
)

save(
  census_rm_2024_results,
  file = paste0(output, "censo_RM_zonas_censales_2024_resultados.RData")
)

## Choropleth maps: all numeric zone variables (shared layout; rotating viridis palettes) ----
maps_dir <- "03_Output/Descriptives/Maps/"
map_vars_2024 <- geo_rm_2024 |>
  sf::st_drop_geometry() |>
  dplyr::select(tidyselect::where(is.numeric)) |>
  names()

map_theme_census <- function() {
  ggplot2::theme_light() +
    ggplot2::theme(
      legend.position = "top",
      legend.justification = "center",
      legend.box.just = "center",
      legend.direction = "horizontal",
      legend.key.width = grid::unit(1.6, "cm"),
      legend.key.height = grid::unit(0.28, "cm"),
      legend.spacing.x = grid::unit(0, "cm"),
      legend.text = ggplot2::element_text(size = 9),
      legend.title = ggplot2::element_blank(),
      legend.margin = ggplot2::margin(b = 4),
      plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5),
      plot.margin = ggplot2::margin(t = 6, r = 10, b = 5, l = 10),
      panel.grid = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_text(angle = 0),
      strip.background = ggplot2::element_rect(fill = NA, color = "gray70"),
      strip.text = ggplot2::element_text(color = "black"),
      strip.text.y.left = ggplot2::element_text(angle = 0)
    )
}

viridis_opts <- c("plasma", "viridis", "cividis", "inferno", "magma", "turbo")

for (i in seq_along(map_vars_2024)) {
  v <- map_vars_2024[[i]]
  pal_opt <- viridis_opts[[((i - 1L) %% length(viridis_opts)) + 1L]]
  title_chr <- tools::toTitleCase(chartr("_", " ", v))
  p_i <- ggplot2::ggplot(geo_rm_2024) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = !!rlang::sym(v)),
      color = grDevices::gray(0.85),
      linewidth = 0.1
    ) +
    ggplot2::scale_fill_viridis_c(
      option = pal_opt,
      na.value = "grey90",
      direction = -1
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = title_chr) +
    map_theme_census()

  out_png <- file.path(maps_dir, paste0("map_census_2024_", v, ".png"))
  ggplot2::ggsave(
    filename = out_png,
    plot = p_i,
    width = 7,
    height = 8,
    dpi = 150,
    create.dir = TRUE
  )
}
