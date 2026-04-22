# Code 2: Temp data preparation ----

## Settings ----
source("02_Code/0.1 Functions.R")
source("02_Code/0.2 Settings.R")

# Data path 
data_inp <- "01_Data/Input/"
data_out <- "01_Data/Output/"

## Open data ---- 

# ID file load
temp_qua_f1 <- "temperature_daily_quadrant.csv"
temp_dis_f1 <- "temperature_daily_district.csv"
temp_qua_f5 <- "temperature_daily_missing_quadrants_fill.csv"

# NDVI 
ndvi_qua_f2 <- "ndvi_daily_quadrant.csv"
ndvi_dis_f2 <- "ndvi_daily_district.csv"

# Shape data
data_qua <- rio::import(paste0(data_out, "Quadrant_data_geo_RM.RData"))
data_dis <- rio::import(paste0(data_out, "District_data_geo_RM.RData"))

# Temperature
temp_qua <- rio::import(paste0(data_out, "clim_data/", temp_qua_f1)) %>% janitor::clean_names() |> rename(geometry=geo)
temp_dis <- rio::import(paste0(data_out, "clim_data/", temp_dis_f1)) %>% janitor::clean_names()
temp_qua_fill <- rio::import(paste0(data_out, "clim_data/", temp_qua_f5)) %>% janitor::clean_names()

# NDVI
ndvi_qua <- rio::import(paste0(data_out, "clim_data/", ndvi_qua_f2)) %>% janitor::clean_names()
ndvi_dis <- rio::import(paste0(data_out, "clim_data/", ndvi_dis_f2)) %>% janitor::clean_names()

## Filter date range ----
# Filter temperature and NDVI data to period 2017-01-01 to 2025-08-31
start_date <- as.Date("2017-01-01")
end_date <- as.Date("2025-08-31")

temp_qua <- temp_qua |> 
  mutate(date = as.Date(date)) |> 
  filter(date >= start_date & date <= end_date) |> 
  dplyr::select(geometry_id, date, temperature_2m, temperature_2m_min, temperature_2m_max)

temp_qua5 <- temp_qua_fill |> 
  mutate(date = as.Date(date)) |> 
  filter(date >= start_date & date <= end_date) |> 
  dplyr::select(geometry_id, date, temperature_2m, temperature_2m_min, temperature_2m_max)

temp_qua <- temp_qua |>
  dplyr::left_join(temp_qua5, by = c("geometry_id", "date"), suffix = c("", "_fill")) |>
  dplyr::mutate(
    temperature_2m = dplyr::coalesce(.data$temperature_2m, .data$temperature_2m_fill),
    temperature_2m_min = dplyr::coalesce(.data$temperature_2m_min, .data$temperature_2m_min_fill),
    temperature_2m_max = dplyr::coalesce(.data$temperature_2m_max, .data$temperature_2m_max_fill)
  ) |>
  dplyr::select("geometry_id", "date", "temperature_2m", "temperature_2m_min", "temperature_2m_max")

temp_dis <- temp_dis |> 
  mutate(date = as.Date(date)) |> 
  filter(date >= start_date & date <= end_date) |> 
  dplyr::select(geometry_id, date, temperature_2m, temperature_2m_min, temperature_2m_max)

ndvi_qua <- ndvi_qua |> 
  mutate(date = as.Date(date)) |> 
  filter(date >= start_date & date <= end_date) |> 
  dplyr::select(geometry_id, date, ndvi)

ndvi_dis <- ndvi_dis |> 
  mutate(date = as.Date(date)) |> 
  filter(date >= start_date & date <= end_date) |> 
  dplyr::select(geometry_id, date, ndvi)

## Create geometry mapping for districts ----
# The geometry_id in temp_dis is correlative (0, 1, 2...) and doesn't match codigo_comuna
# Since temp_dis doesn't contain geometry information, we match by the order used during export
# The order in Earth Engine export corresponds to the order geometries were processed
# This matches the order in data_dis when converted to FeatureCollection (sorted by codigo_comuna)

# Create mapping table: geometry_id -> codigo_comuna
# Match assumes geometries were processed in order sorted by codigo_comuna
# This is the order used in the Python script when creating the FeatureCollection
data_dis <- data_dis |> 
  mutate(geometry_id = row_number() - 1) 

data_qua <- data_qua |> 
  mutate(geometry_id = row_number() - 1) 

# Join with climate data 
temp_dis_join <- data_dis |> 
  left_join(temp_dis, by = "geometry_id")

temp_qua_join <- data_qua |> 
  left_join(temp_qua, by = "geometry_id")

ndvi_dis_join <- data_dis |> 
  left_join(ndvi_dis, by = "geometry_id")

ndvi_qua_join <- data_qua |> 
  left_join(ndvi_qua, by = "geometry_id")

## Imputation temp district ----
# 5 distritcts have some missing temperature values (mostly in 2024-2025). We impute these using the mean of the 4 nearest districts with available data for that date. This is done separately for mean, min and max temperature.
# District small size 
# Cerro navia: 249 
# Estación central: 193
# Lo Prado: 245
# Pedro Aguirre Cerda: 64
# Pudahuel: 231

## Save processed data ----
save(temp_dis_join, file = paste0(data_out, "Temp_district_data_RM_2017_2025.RData"))
save(temp_qua_join, file = paste0(data_out, "Temp_quadrant_data_RM_2017_2025.RData"))

save(ndvi_dis_join, file = paste0(data_out, "NDVI_district_data_RM_2017_2025.RData"))
save(ndvi_qua_join, file = paste0(data_out, "NDVI_quadrant_data_RM_2017_2025.RData"))

## Plots ----
maps_temp_dir <- "03_Output/Descriptives/Maps_temp/"

## Metro clip (same as 2.0 Crime_geo_data.R / 5.0 CENSO_data_zone.R) ----
urban_island <- c(
  "13124071004", "13124071005", "13124081001", "13124071001", "13124071002", "13124071003",
  "13401121001",
  "13119131001",
  "13203031000", "13203031001", "13203031002", "13203011001", "13203011002"
)

stgo_urb <- chilemapas::mapa_zonas |>
  dplyr::filter(as.numeric(.data$codigo_region) == 13) |>
  dplyr::left_join(
    chilemapas::codigos_territoriales |>
      dplyr::select(dplyr::matches("comuna"))
  ) |>
  dplyr::filter(
    .data$codigo_provincia %in% c(131, 132) | .data$nombre_comuna == "San Bernardo",
    .data$nombre_comuna != "Pirque"
  ) |>
  dplyr::filter(!.data$geocodigo %in% urban_island) |>
  dplyr::group_by(.data$nombre_comuna, .data$codigo_comuna) |>
  dplyr::summarise(geometry = sf::st_union(.data$geometry), .groups = "drop") |>
  dplyr::mutate(codigo_comuna = as.numeric(.data$codigo_comuna)) |>
  sf::st_as_sf() |>
  sf::st_transform(4326)

stgo_urb_union <- sf::st_union(stgo_urb)

quad_unique <- data_qua |>
  dplyr::distinct(.data$quadrant, .keep_all = TRUE) |>
  dplyr::select("quadrant", "geometry") |>
  sf::st_as_sf(crs = 4326)

quad_unique_metro <- sf::st_intersection(
  sf::st_make_valid(quad_unique),
  sf::st_make_valid(stgo_urb_union)
) |>
  dplyr::group_by(.data$quadrant) |>
  dplyr::summarise(geometry = sf::st_union(.data$geometry), .groups = "drop") |>
  sf::st_as_sf()

temp_quad_summary <- temp_qua_join |>
  sf::st_drop_geometry() |>
  dplyr::group_by(.data$quadrant) |>
  dplyr::summarise(
    temperature_2m_mean = mean(.data$temperature_2m, na.rm = TRUE),
    temperature_2m_min_mean = mean(.data$temperature_2m_min, na.rm = TRUE),
    temperature_2m_max_mean = mean(.data$temperature_2m_max, na.rm = TRUE),
    .groups = "drop"
  )

## Monthly climatology: mean of all January days, all February days, … (full date range) ----
temp_quad_monthly <- temp_qua_join |>
  sf::st_drop_geometry() |>
  dplyr::mutate(month = lubridate::month(.data$date)) |>
  dplyr::group_by(.data$quadrant, .data$month) |>
  dplyr::summarise(
    temperature_2m_mean = mean(.data$temperature_2m, na.rm = TRUE),
    temperature_2m_min_mean = mean(.data$temperature_2m_min, na.rm = TRUE),
    temperature_2m_max_mean = mean(.data$temperature_2m_max, na.rm = TRUE),
    .groups = "drop"
  )

ndvi_quad_summary <- ndvi_qua_join |>
  sf::st_drop_geometry() |>
  dplyr::group_by(.data$quadrant) |>
  dplyr::summarise(ndvi_mean = mean(.data$ndvi, na.rm = TRUE), .groups = "drop")

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

ndvi_green_palette <- c(
  "#ffffe5", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#004529"
)

temp_layers <- tibble::tibble(
  var = c("temperature_2m_mean", "temperature_2m_min_mean", "temperature_2m_max_mean"),
  title = c(
    "Mean 2 m temperature (daily average, °C)",
    "Mean daily minimum 2 m temperature (°C)",
    "Mean daily maximum 2 m temperature (°C)"
  ),
  fname = c("map_temp_quadrant_mean", "map_temp_quadrant_min_mean", "map_temp_quadrant_max_mean"),
  panel_fname = c(
    "panel_temp_monthly_mean_2m",
    "panel_temp_monthly_min_2m",
    "panel_temp_monthly_max_2m"
  ),
  panel_title = c(
    "Monthly climatology: mean 2 m temperature (°C)",
    "Monthly climatology: mean daily minimum 2 m temperature (°C)",
    "Monthly climatology: mean daily maximum 2 m temperature (°C)"
  )
)

## Single colour scale (°C) for mean, min and max — full study period + monthly aggregates ----
temp_fill_limits <- range(
  c(
    temp_quad_summary$temperature_2m_mean,
    temp_quad_summary$temperature_2m_min_mean,
    temp_quad_summary$temperature_2m_max_mean,
    temp_quad_monthly$temperature_2m_mean,
    temp_quad_monthly$temperature_2m_min_mean,
    temp_quad_monthly$temperature_2m_max_mean
  ),
  na.rm = TRUE
)

theme_temp_panel_cell <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 9, face = "bold", hjust = 0.5),
      plot.margin = ggplot2::margin(4, 4, 4, 4),
      legend.position = "none",
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
}

plot_quad_temp_fill <- function(geo_sf, var, subtitle, fill_limits) {
  ggplot2::ggplot(geo_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = !!rlang::sym(var)),
      color = grDevices::gray(0.85),
      linewidth = 0.08
    ) +
    ggplot2::scale_fill_gradientn(
      colours = temp_max_10,
      limits = fill_limits,
      na.value = "grey90",
      oob = scales::squish
    ) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(title = subtitle) +
    theme_temp_panel_cell()
}

for (i in seq_len(nrow(temp_layers))) {
  v <- temp_layers$var[[i]]
  geo_i <- quad_unique_metro |>
    dplyr::left_join(
      temp_quad_summary |> dplyr::select("quadrant", dplyr::all_of(v)),
      by = "quadrant"
    )

  p_i <- ggplot2::ggplot(geo_i) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = !!rlang::sym(v)),
      color = grDevices::gray(0.85),
      linewidth = 0.1
    ) +
    ggplot2::scale_fill_gradientn(
      colours = temp_max_10,
      limits = temp_fill_limits,
      na.value = "grey90",
      oob = scales::squish
    ) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = temp_layers$title[[i]]) +
    map_theme_census()

  ggplot2::ggsave(
    filename = paste0(maps_temp_dir, temp_layers$fname[[i]], ".png"),
    plot = p_i,
    width = 7,
    height = 8,
    dpi = 150,
    create.dir = TRUE
  )
}

for (i in seq_len(nrow(temp_layers))) {
  v <- temp_layers$var[[i]]
  month_plots <- vector("list", 12L)
  for (mo in 1:12) {
    temp_mo <- temp_quad_monthly |>
      dplyr::filter(.data$month == mo) |>
      dplyr::select(-"month")
    geo_m <- quad_unique_metro |>
      dplyr::left_join(
        temp_mo |> dplyr::select("quadrant", dplyr::all_of(v)),
        by = "quadrant"
      )
    month_plots[[mo]] <- plot_quad_temp_fill(
      geo_m,
      v,
      month.name[mo],
      temp_fill_limits
    )
  }

  panel_i <- patchwork::wrap_plots(month_plots, ncol = 4, nrow = 3) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.justification = "center",
      legend.key.width = grid::unit(1.6, "cm"),
      legend.key.height = grid::unit(0.35, "cm")
    )

  panel_i <- panel_i +
    patchwork::plot_annotation(
      title = temp_layers$panel_title[[i]],
      subtitle = "Each map: mean of all days in that calendar month (study period). Same colour scale (°C) for mean, min and max panels.",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5, margin = ggplot2::margin(b = 8))
      )
    )

  ggplot2::ggsave(
    filename = paste0(maps_temp_dir, temp_layers$panel_fname[[i]], ".png"),
    plot = panel_i,
    width = 14,
    height = 11,
    dpi = 150,
    create.dir = TRUE
  )
}

geo_ndvi <- quad_unique_metro |>
  dplyr::left_join(ndvi_quad_summary, by = "quadrant")

p_ndvi <- ggplot2::ggplot(geo_ndvi) +
  ggplot2::geom_sf(
    ggplot2::aes(fill = .data$ndvi_mean),
    color = grDevices::gray(0.85),
    linewidth = 0.1
  ) +
  ggplot2::scale_fill_gradientn(
    colours = ndvi_green_palette,
    na.value = "grey90"
  ) +
  ggplot2::coord_sf(expand = FALSE) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Mean NDVI (daily composite)") +
  map_theme_census()

ggplot2::ggsave(
  filename = paste0(maps_temp_dir, "map_ndvi_quadrant_mean.png"),
  plot = p_ndvi,
  width = 7,
  height = 8,
  dpi = 150,
  create.dir = TRUE
)
