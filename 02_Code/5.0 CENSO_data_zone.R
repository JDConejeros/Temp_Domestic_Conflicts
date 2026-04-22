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
maps_dir <- "03_Output/Descriptives/Maps_censo/"
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

## Generate data with quadrant geometries for merging with crime data (after metro clip) ----
quad_geo <- rio::import(paste0(output, "Quadrant_data_geo_RM.RData"))
glimpse(quad_geo)
glimpse(geo_rm_2024)

## Area-weighted apportionment of metro-clipped census zones (geo_rm_2024) to quadrant polygons ----
## Each intersection piece gets fraction = area(piece) / area(full clipped zone); n_* counts are
## scaled by that fraction and summed by quadrant. prom_* are population-weighted means using n_per * fraction.
census_n_cols <- c(
  "n_per", "n_mujeres", "n_hombres",
  "n_edad_0_5", "n_edad_6_13", "n_edad_14_17", "n_edad_18_24", "n_edad_25_44", "n_edad_45_59", "n_edad_60_mas",
  "n_jefatura_mujer", "n_inmigrantes",
  "n_cine_primaria", "n_cine_secundaria", "n_cine_terciaria_maestria_doctorado",
  "n_ocupado", "n_desocupado", "n_hog", "n_vp", "n_viv_hacinadas", "n_vp_ocupada",
  "n_deficit_cuantitativo",
  "n_mat_paredes_hormigon", "n_mat_paredes_albanileria", "n_mat_paredes_tabique_forrado",
  "n_mat_paredes_tabique_sin_forro", "n_mat_paredes_artesanal", "n_mat_paredes_precarios",
  "n_mat_techo_tejas", "n_mat_techo_hormigon", "n_mat_techo_zinc", "n_mat_techo_fibrocemento",
  "n_mat_techo_fonolita", "n_mat_techo_paja", "n_mat_techo_precarios", "n_mat_techo_sin_cubierta",
  "n_mat_piso_radier_con_revestimiento", "n_mat_piso_radier_sin_revestimiento",
  "n_mat_piso_baldosa_cemento", "n_mat_piso_capa_cemento", "n_mat_piso_tierra"
)
census_prom_cols <- c("prom_edad", "prom_escolaridad18")

zones_for_apportion <- geo_rm_2024 |>
  dplyr::select(
    "geo_code",
    tidyselect::all_of(intersect(census_n_cols, names(geo_rm_2024))),
    tidyselect::all_of(intersect(census_prom_cols, names(geo_rm_2024)))
  )

quads_for_apportion <- quad_geo |>
  sf::st_as_sf(crs = 4326) |>
  dplyr::select("quad_code", "quadrant", "geometry")

## Pairwise intersection: attributes from both zone and quadrant; geometry is the overlap polygon
zone_quad_inters <- sf::st_intersection(
  sf::st_make_valid(zones_for_apportion),
  sf::st_make_valid(quads_for_apportion)
)

zone_quad_inters <- zone_quad_inters |>
  sf::st_transform(metric_crs) |>
  dplyr::mutate(
    piece_area_m2 = as.numeric(sf::st_area(.data$geometry))
  )

zone_areas_clipped <- geo_rm_2024 |>
  sf::st_transform(metric_crs) |>
  dplyr::mutate(zone_area_m2 = as.numeric(sf::st_area(.data$geometry))) |>
  sf::st_drop_geometry() |>
  dplyr::select("geo_code", "zone_area_m2")

inters_tab <- zone_quad_inters |>
  sf::st_drop_geometry() |>
  dplyr::left_join(zone_areas_clipped, by = "geo_code") |>
  dplyr::mutate(
    area_fraction = dplyr::if_else(
      !is.na(.data$zone_area_m2) & .data$zone_area_m2 > 0,
      .data$piece_area_m2 / .data$zone_area_m2,
      0
    )
  )

n_present <- intersect(census_n_cols, names(inters_tab))

## Scale count columns by area fraction; sum to quadrant level
inters_scaled <- inters_tab |>
  dplyr::mutate(
    dplyr::across(
      tidyselect::all_of(n_present),
      ~ .x * .data$area_fraction
    ),
    w_pop = .data$n_per
  )

sum_prom_edad <- "prom_edad" %in% names(inters_scaled)
sum_prom_esc <- "prom_escolaridad18" %in% names(inters_scaled)

quad_census_agg <- inters_scaled |>
  dplyr::group_by(.data$quad_code, .data$quadrant) |>
  dplyr::summarise(
    dplyr::across(tidyselect::all_of(n_present), ~ sum(.x, na.rm = TRUE)),
    prom_edad_wsum = if (sum_prom_edad) {
      sum(.data$prom_edad * .data$w_pop, na.rm = TRUE)
    } else {
      NA_real_
    },
    prom_esc_wsum = if (sum_prom_esc) {
      sum(.data$prom_escolaridad18 * .data$w_pop, na.rm = TRUE)
    } else {
      NA_real_
    },
    w_pop_sum = sum(.data$w_pop, na.rm = TRUE),
    .groups = "drop"
  )

quad_census_agg <- quad_census_agg |>
  dplyr::mutate(
    prom_edad = dplyr::if_else(
      sum_prom_edad & .data$w_pop_sum > 0,
      .data$prom_edad_wsum / .data$w_pop_sum,
      NA_real_
    ),
    prom_escolaridad18 = dplyr::if_else(
      sum_prom_esc & .data$w_pop_sum > 0,
      .data$prom_esc_wsum / .data$w_pop_sum,
      NA_real_
    )
  ) |>
  dplyr::select(-tidyselect::any_of(c("prom_edad_wsum", "prom_esc_wsum", "w_pop_sum"))) |>
  dplyr::mutate(
    dplyr::across(
      tidyselect::all_of(n_present),
      ~ as.integer(round(.x))
    )
  )

## Attach aggregated census fields to full quadrant table (same row order / geometry as quad_geo)
quad_geo_sf <- quad_geo |>
  sf::st_as_sf(crs = 4326)

quad_census_agg <- quad_census_agg |>
  dplyr::mutate(quad_code = as.character(.data$quad_code))

quad_geo_sf <- quad_geo_sf |>
  dplyr::mutate(quad_code = as.character(.data$quad_code))

quad_geo <- quad_geo_sf |>
  dplyr::left_join(
    quad_census_agg,
    by = c("quad_code", "quadrant")
  )

## Quadrant-level CPV 2024 derived variables (same definitions as zonal d_zonal) + km2 / density ----
quad_geo <- quad_geo |>
  dplyr::mutate(
    population = as.numeric(.data$n_per),
    female_population = as.numeric(.data$n_mujeres),
    ## Demography
    sex_ratio_index = (.data$n_hombres / .data$n_mujeres) * 100,
    aging_index = (.data$n_edad_60_mas / (.data$n_edad_0_5 + .data$n_edad_6_13)) * 100,
    pct_children = (.data$n_edad_0_5 + .data$n_edad_6_13) / .data$n_per,
    pct_age_60_plus = .data$n_edad_60_mas / .data$n_per,
    pct_female_headed_hh = .data$n_jefatura_mujer / .data$n_hog,
    pct_immigrants = .data$n_inmigrantes / .data$n_per,
    ## Education
    pct_edu_primary = .data$n_cine_primaria / .data$n_per,
    pct_edu_secondary = .data$n_cine_secundaria / .data$n_per,
    pct_edu_tertiary = .data$n_cine_terciaria_maestria_doctorado / .data$n_per,
    ## Housing (INE denominator: occupied private dwellings)
    pct_housing_quantitative_deficit = .data$n_deficit_cuantitativo / .data$n_vp_ocupada,
    pct_overcrowded_units = .data$n_viv_hacinadas / .data$n_vp_ocupada,
    ## Dwelling materials (heat / vulnerability literature)
    pct_wall_concrete = .data$n_mat_paredes_hormigon / .data$n_vp_ocupada,
    pct_wall_masonry = .data$n_mat_paredes_albanileria / .data$n_vp_ocupada,
    pct_wall_studding_lined = .data$n_mat_paredes_tabique_forrado / .data$n_vp_ocupada,
    pct_wall_studding_unlined = .data$n_mat_paredes_tabique_sin_forro / .data$n_vp_ocupada,
    pct_wall_artisanal = .data$n_mat_paredes_artesanal / .data$n_vp_ocupada,
    pct_wall_precarious = .data$n_mat_paredes_precarios / .data$n_vp_ocupada,
    pct_roof_tiles = .data$n_mat_techo_tejas / .data$n_vp_ocupada,
    pct_roof_concrete = .data$n_mat_techo_hormigon / .data$n_vp_ocupada,
    pct_roof_zinc = .data$n_mat_techo_zinc / .data$n_vp_ocupada,
    pct_roof_fiber_cement = .data$n_mat_techo_fibrocemento / .data$n_vp_ocupada,
    pct_roof_precarious = .data$n_mat_techo_precarios / .data$n_vp_ocupada,
    pct_floor_slab_finished = .data$n_mat_piso_radier_con_revestimiento / .data$n_vp_ocupada,
    pct_floor_slab_unfinished = .data$n_mat_piso_radier_sin_revestimiento / .data$n_vp_ocupada,
    pct_floor_earth = .data$n_mat_piso_tierra / .data$n_vp_ocupada,
    ## Rates per 10,000 people
    rate_females_per_10k = (.data$n_mujeres / .data$n_per) * 10000,
    rate_children_per_10k = ((.data$n_edad_0_5 + .data$n_edad_6_13) / .data$n_per) * 10000,
    rate_age_60_plus_per_10k = (.data$n_edad_60_mas / .data$n_per) * 10000,
    rate_immigrants_per_10k = (.data$n_inmigrantes / .data$n_per) * 10000,
    ## Housing material scores (literature weights on dwelling counts; higher = more “solid” stock)
    score_wall =
      2 * .data$n_mat_paredes_hormigon +
      2 * .data$n_mat_paredes_albanileria +
      1 * .data$n_mat_paredes_tabique_forrado +
      0 * .data$n_mat_paredes_tabique_sin_forro +
      -1 * .data$n_mat_paredes_artesanal +
      -2 * .data$n_mat_paredes_precarios,
    score_roof =
      2 * .data$n_mat_techo_tejas +
      2 * .data$n_mat_techo_hormigon +
      0 * .data$n_mat_techo_zinc +
      0 * .data$n_mat_techo_fibrocemento +
      -2 * .data$n_mat_techo_precarios +
      -3 * .data$n_mat_techo_sin_cubierta,
    score_floor =
      2 * .data$n_mat_piso_radier_con_revestimiento +
      1 * .data$n_mat_piso_radier_sin_revestimiento +
      0 * .data$n_mat_piso_baldosa_cemento +
      -1 * .data$n_mat_piso_capa_cemento +
      -2 * .data$n_mat_piso_tierra,
    score_overcrowding = -3 * .data$n_viv_hacinadas,
    ## Vulnerability index: negate the composite so LOW = lower vulnerability, HIGH = higher
    ## vulnerability (more precarious materials, worse roofs/floors, overcrowding raise the index).
    housing_heat_vulnerability_index =
      -(score_wall + score_roof + score_floor + score_overcrowding) / .data$n_vp_ocupada
  )

quad_geo <- add_area_km2_zone_full(quad_geo)
quad_geo <- clip_metro_area_and_density(quad_geo)

## Choropleth maps: all numeric quadrant variables (same layout as zonal maps) ----
maps_dir_q <- "03_Output/Descriptives/Maps_censo_q/"
map_vars_quad <- quad_geo |>
  sf::st_drop_geometry() |>
  dplyr::select(tidyselect::where(is.numeric)) |>
  names()

for (i in seq_along(map_vars_quad)) {
  v <- map_vars_quad[[i]]
  pal_opt <- viridis_opts[[((i - 1L) %% length(viridis_opts)) + 1L]]
  title_chr <- tools::toTitleCase(chartr("_", " ", v))
  p_i <- ggplot2::ggplot(quad_geo) +
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

  out_png <- file.path(maps_dir_q, paste0("map_census_2024_", v, ".png"))
  ggplot2::ggsave(
    filename = out_png,
    plot = p_i,
    width = 7,
    height = 8,
    dpi = 150,
    create.dir = TRUE
  )
}

glimpse(quad_geo)
summary(quad_geo)

save(quad_geo, file = paste0(output, "Quadrant_data_geo_CENSO_urban_RM.RData"))
