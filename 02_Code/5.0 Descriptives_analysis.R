# Code 5.0: Descriptive analysis ----

rm(list=ls())
## Settings ----
source("02_Code/0.1 Functions.R")
source("02_Code/0.2 Settings.R")

# Data path 
data_out <- "01_Data/Output/"
quad <- "analytical_data/quadrant/"
output <- "03_Output/Descriptives/"

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

## Decile labels (VIF, temperature maps)
labs_dec <- c(
  "Lower", "2nd", "3rd", "4th", "5th",
  "6th", "7th", "8th", "9th", "Upper"
)

## Spatial distribution of crimes -----
## Map only urban municipalities
## Crime counts by quadrant ----

vif <- crime_count |> 
  group_by(zone, code_district, district, quadrant) |> 
  summarise(n_crimes = sum(domestic_violence)) |> 
  mutate(dec = ntile(n_crimes, 10)) |> 
  mutate(d10 = factor(dec, levels = 1:10, labels = labs_dec))

summary(vif)
write.xlsx(vif, paste0(output, "VIF_crimes_RM.xlsx"))

#vif <- vif |> 
#  filter(zone == "Urban")

## Urban map santiago ----

urban_island <- c("13124071004", "13124071005", "13124081001", "13124071001", "13124071002", "13124071003", #Pudahuel
                  "13401121001", #San Bernardo
                  "13119131001", #Maipú
                  "13203031000", "13203031001", "13203031002", "13203011001", "13203011002" #San José de Maipo
)

stgo_urb <- chilemapas::mapa_zonas |> 
  filter(codigo_region == 13) |> 
  left_join(chilemapas::codigos_territoriales |> 
              select(matches("comuna"))) |> 
  filter(codigo_provincia %in% c(131, 132) | nombre_comuna == "San Bernardo", nombre_comuna != "Pirque") |>
  filter(!geocodigo %in% urban_island) |>
  group_by(nombre_comuna, codigo_comuna) %>%
  summarise(geometry = st_union(geometry)) |>
  ungroup() |> 
  mutate(codigo_comuna = as.numeric(codigo_comuna))

glimpse(stgo_urb)
stgo_urb <- st_as_sf(stgo_urb)
stgo_urb <- st_transform(stgo_urb, 4326)
plot(stgo_urb)

# Extent del área urbana (stgo_urb) + margen para tiles
bb <- st_bbox(stgo_urb)
pad <- 0.02
bounds_rm <- st_bbox(
  c(
    bb$xmin - pad,
    bb$ymin - pad,
    bb$xmax + pad,
    bb$ymax + pad
  ),
  crs = st_crs(4326)
)

# Basemap: CartoDB Positron sin etiquetas; zoom alto para detalle urbano
map_base_rm <- get_tiles(
  bounds_rm,
  provider = "CartoDB.PositronNoLabels",
  zoom = 14,
  crop = TRUE,
  retina = TRUE
) # Generate API to extract the map: https://docs.stadiamaps.com/tutorials/getting-started-in-r-with-ggmap/

plot(map_base_rm)

## Map VIF with RM basemap (maptiles) + quadrant polygons ----

# 35 Mun 
com_suburb <- unique(stgo_urb$codigo_comuna)

# Límites comunales (interior del área urbana)
stgo_comunas_lim <- chilemapas::mapa_comunas |>
  dplyr::mutate(codigo_comuna = as.numeric(codigo_comuna)) |>
  dplyr::filter(codigo_comuna %in% com_suburb) |>
  st_as_sf() |>
  st_transform(4326)

glimpse(vif)
glimpse(quad_geo)

map_vif <- quad_geo |> 
  left_join(vif, by = c("com_code"="code_district", "quadrant")) |> 
  filter(com_code %in% com_suburb)

# Paleta deciles: rampa violeta (10 pasos, claro → oscuro, sin blanco)
theme_map_ndv <- theme_light() +
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.box.just = "center",
    legend.direction = "horizontal",
    legend.key.width = unit(1.6, "cm"),
    legend.key.height = unit(0.28, "cm"),
    legend.spacing.x = unit(0, "cm"), #0.35
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10, face = "bold"),
    legend.margin = margin(b = 4),
    plot.margin = margin(t = 6, r = 10, b = 5, l = 10),
    panel.grid = element_blank(),
    strip.text.y = element_text(angle = 0),
    strip.background = element_rect(fill = NA, color = "gray70"),
    strip.text = element_text(color = "black"),
    strip.text.y.left = element_text(angle = 0),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

## Leyenda continua en 10 cortes (poblacion, temperatura, densidades)
guide_censo_10steps <- guide_coloursteps(
  direction = "horizontal",
  title.position = "top",
  label.position = "bottom",
  barwidth = unit(10, "cm"),
  barheight = unit(0.3, "cm"),
  label.theme = element_text(size = 8),
  show.limits = TRUE
)

map_vif <- st_as_sf(map_vif)
map_vif <- st_transform(map_vif, 4326)

# Recorte de cuadrantes al límite urbano (evita geometrías que sobresalen)
stgo_urb_union <- st_union(stgo_urb)
map_vif_clipped <- st_intersection(
  st_make_valid(map_vif),
  st_make_valid(stgo_urb_union)
)

## Daily maximum temperature: media por cuadrante (valor continuo), mismos cortes que poblacion ----
temp_cov_df <- if (inherits(data_cov, "sf")) {
  sf::st_drop_geometry(data_cov)
} else {
  data_cov
}

temp_by_quad <- temp_cov_df |>
  dplyr::group_by(.data$com_code, .data$quadrant) |>
  dplyr::summarise(
    temperature_2m_max_mean = mean(.data$temperature_2m_max, na.rm = TRUE),
    .groups = "drop"
  ) #|> 
  #mutate(temperature_2m_max_mean = if_else(is.infinite(temperature_2m_max_mean), 22, temperature_2m_max_mean)) 
  #filter(!is.infinite(temperature_2m_max_mean))

map_temp <- quad_geo |>
  dplyr::left_join(temp_by_quad, by = c("com_code", "quadrant")) |>
  dplyr::filter(.data$com_code %in% com_suburb)

map_temp <- sf::st_as_sf(map_temp)
map_temp <- sf::st_transform(map_temp, 4326)
map_temp_clipped <- sf::st_intersection(
  sf::st_make_valid(map_temp),
  sf::st_make_valid(stgo_urb_union)
)

fig_temp_basemap <- ggplot() +
  geom_spatraster_rgb(data = map_base_rm) +
  geom_sf(
    data = stgo_comunas_lim,
    fill = NA,
    color = "gray40",
    linewidth = 0.35,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = st_union(stgo_urb) |> st_boundary(),
    color = "gray15",
    linewidth = 2,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = map_temp_clipped,
    aes(fill = temperature_2m_max_mean),
    color = "white",
    linewidth = 0.5
  ) +
  scale_fill_stepsn(
    colours = temp_max_10,
    n.breaks = 10,
    nice.breaks = FALSE,
    name = "Daily maximum temperature, quadrant mean (°C, 2017-2025)",
    na.value = "gray90",
    oob = scales::squish,
    labels = scales::label_number(accuracy = 0.1),
    guide = guide_censo_10steps
  ) +
  coord_sf(
    crs = st_crs(4326),
    expand = FALSE,
    xlim = c(bb$xmin, bb$xmax),
    ylim = c(bb$ymin, bb$ymax)
  ) +
  theme_map_ndv +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    height = unit(1, "cm"),
    width = unit(0.75, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering()
  )

fig_temp_basemap 

ggsave(
  filename = paste0(output, "map_temperature_2m_max_RM.png"),
  plot = fig_temp_basemap,
  width = 24,
  height = 20,
  units = "cm",
  dpi = 300
)

fig_ndv_basemap <- ggplot() +
  geom_spatraster_rgb(data = map_base_rm) +
  geom_sf(
    data = stgo_comunas_lim,
    fill = NA,
    color = "gray40",
    linewidth = 0.35,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = st_union(stgo_urb) |> st_boundary(),
    color = "gray15",
    linewidth = 2,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = map_vif_clipped,
    aes(fill = d10),
    color = "white",
    linewidth = 0.5
  ) +
  scale_fill_manual(
    name = "Reporting domestic violence deciles (2017-2025)",
    values = terra_pal,
    na.value = "gray90",
    na.translate = TRUE,
    guide = guide_legend(
      direction = "horizontal",
      title.position = "top",
      label.position = "bottom",
      nrow = 1,
      keywidth = unit(1, "cm"),
      keyheight = unit(0.3, "cm"),
      label.theme = element_text(size = 8)
    )
  ) +
  coord_sf(
    crs = st_crs(4326),
    expand = FALSE,
    xlim = c(bb$xmin, bb$xmax),
    ylim = c(bb$ymin, bb$ymax)
  ) +
  theme_map_ndv +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    height = unit(1, "cm"),
    width = unit(0.75, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering()
  )

fig_ndv_basemap

ggsave(
  filename = paste0(output, "map_ndv_RM_basemap.png"),
  plot = fig_ndv_basemap,
  width = 24,
  height = 20,
  units = "cm",
  dpi = 300
)


## Maps: VIF + Census 2024 (population, female population, densities) ----
## Data: 02_Code/4.2_Descriptives_censo_zonas.R -> censo_RM_zonas_2024_resultados.RData

load(paste0(output, "censo_RM_zonas_2024_resultados.RData"))
geo_rm_2024 <- resultados_censo_rm_2024$sf_zonas

## Same layers and theme as fig_ndv_basemap; legend titles in English
fig_censo_choropleth <- function(geo_rm, fill_var, colours_vec, legend_name, density_scale = FALSE) {
  lbl <- if (density_scale) {
    scales::label_number(big.mark = " ", accuracy = 1)
  } else {
    scales::label_number(big.mark = " ")
  }
  ggplot() +
    geom_spatraster_rgb(data = map_base_rm) +
    geom_sf(
      data = stgo_comunas_lim,
      fill = NA,
      color = "gray40",
      linewidth = 0.35,
      inherit.aes = FALSE
    ) +
    geom_sf(
      data = st_union(stgo_urb) |> st_boundary(),
      color = "gray15",
      linewidth = 2,
      inherit.aes = FALSE
    ) +
    geom_sf(
      data = geo_rm,
      aes(fill = !!rlang::sym(fill_var)),
      color = "white",
      linewidth = 0.5
    ) +
    scale_fill_stepsn(
      colours = colours_vec,
      n.breaks = 10,
      nice.breaks = FALSE,
      name = legend_name,
      na.value = "gray90",
      oob = scales::squish,
      labels = lbl,
      guide = guide_censo_10steps
    ) +
    coord_sf(
      crs = st_crs(4326),
      expand = FALSE,
      xlim = c(bb$xmin, bb$xmax),
      ylim = c(bb$ymin, bb$ymax)
    ) +
    theme_map_ndv +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.2) +
    ggspatial::annotation_north_arrow(
      location = "tr",
      height = unit(1, "cm"),
      width = unit(0.75, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering()
    )
}

fig_pop_total <- fig_censo_choropleth(
  geo_rm_2024,
  "poblacion",
  blues_pal,
  "Population (Census 2024)",
  density_scale = FALSE
)
fig_pop_female <- fig_censo_choropleth(
  geo_rm_2024,
  "n_mujeres",
  vi_pal,
  "Female population (Census 2024)",
  density_scale = FALSE
)

fig_panel_population <-
  fig_ndv_basemap + fig_temp_basemap + fig_pop_total + fig_pop_female +
  patchwork::plot_layout(ncol = 4)

ggsave(
  filename = paste0(output, "map_descriptives_RM_panel_population_2024.png"),
  plot = fig_panel_population,
  width = 68,
  height = 20,
  units = "cm",
  dpi = 300,
  create.dir = TRUE
)
fig_panel_population



