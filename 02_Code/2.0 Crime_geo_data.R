# Code 1.1: Crime classification ----
rm(list=ls())
## Settings ----
source("02_Code/0.1 Functions.R")
source("02_Code/0.2 Settings.R")

# Data path 
data_inp <- "01_Data/Input/"
data_out <- "01_Data/Output/"

# Crime Inputs ----
crime_data <- rio::import(paste0(data_out, "Crime_process_RM_2017_2025.RData")); glimpse(crime_data)
crimes_groups <- rio::import(paste0(data_out, "Crime_types_groups.xlsx")); glimpse(crimes_groups)  
crimes_plances <- rio::import(paste0(data_out, "Crime_places_groups.xlsx")); glimpse(crimes_plances)  
quad_geo <- rio::import(paste0(data_out, "Info_geo_quadrant.RData")); glimpse(quad_geo)
pq <- rio::import(paste0(data_inp, "Webscraping_quadrant_plan/Data/pq/mdsf_plan_cuadrante_carabineros.csv")); glimpse(pq)

# Joint data ----
crime_data <- crime_data |> 
  left_join(crimes_groups |> dplyr::select(-n), by = "crime") |> 
  left_join(crimes_plances |> dplyr::select(-n), by = "place")

unique(crime_data$family)
#[5] "Domestic violence"               
#[4] "Crimes against life or personal integrity"
#[2] "Violent robberies"                        
#[3] "Non-violent property crimes"              
#[6] "Weapon-related crimes"                    
#[7] "Incivilities"                             
#[8] "Drug-related crimes" 
#[1] "Other"                                    

unique(crime_data$place_category)
# "Public space"    "Residence"       
# "Transportation"  "Commercial space" "Work/study place"  "Other"   
unique(crime_data$place_type)
# [1] "Public"   "Private""Other"  

crime_data <- crime_data |> 
  mutate(
    family = factor(family, levels = c("Domestic violence", "Crimes against life or personal integrity", 
                                       "Violent robberies", "Non-violent property crimes", "Weapon-related crimes",
                                       "Incivilities", "Drug-related crimes", "Other")),
    place_category = factor(place_category, levels = c("Public space", "Residence", 
                                                       "Transportation", "Commercial space", 
                                                       "Work/study place", "Other")),
    place_type = factor(place_type, levels = c("Public", "Private", "Other"))
  )

glimpse(crime_data)
summary(crime_data)
glimpse(quad_geo)

quad_geo <- quad_geo |> 
  mutate(com_codigo=as.numeric(com_codigo)) |> 
  dplyr::select(
    uni_codigo,
    cua_codigo, 
    cua_descri, 
    cua_tipo,
    comuna, 
    com_codigo,
    unidad, 
    zona, 
    num_cuad, 
    uni_codigo,
    cod_aupol,
    cua_ano, 
    shape_area, 
    geometry      
        ) |>
  rename(
    quadrant = cua_descri,
    quadrant_type = cua_tipo,
    com = comuna,
    com_code = com_codigo,
    unit = unidad,
    zone = zona,
    quad_num = num_cuad,
    quad_code = cua_codigo
  )

glimpse(quad_geo)

# Join quad data 
crime_data <- crime_data |> 
  mutate(quadrant = stringr::str_replace(quadrant, "Sec.Rural", "Sector Rural")) |> 
  mutate(quadrant = stringr::str_replace(quadrant, "(?<=\\bCuadrante\\s)0([1-9])\\b", "\\1"))

pq <- pq |> 
  filter(cod_reg == 13) |>
  dplyr::select(cod_com, cua_codigo, cua_sup, cua_descri, cua_tipo, uni_codigo)

unique(crime_data$quadrant)[!unique(crime_data$quadrant) %in% unique(quad_geo$quadrant)]
unique(crime_data$quadrant)[!unique(crime_data$quadrant) %in% unique(pq$cua_descri)]

units <- unique(crime_data$quadrant)[!unique(crime_data$quadrant) %in% unique(quad_geo$quadrant)]
table(unique(crime_data$quadrant) %in% unique(quad_geo$quadrant))
table(unique(quad_geo$quadrant) %in% unique(crime_data$quadrant))

# Manual quadrant harmonization (crime labels not in quad_geo; match district + plan MDSF)
crime_data <- crime_data |>
  mutate(
    quadrant = case_when(
      quadrant == "Cuadrante 122A" & district == "Providencia" ~ "Cuadrante 122",
      quadrant == "Cuadrante 14" & district == "Santiago" ~ "Cuadrante 7",
      quadrant == "Cuadrante 141A" & district == "Providencia" ~ "Cuadrante 124",
      quadrant == "Cuadrante 178" & district == "La Pintana" ~ "Cuadrante 177",
      quadrant == "Cuadrante 237B" & district == "Pudahuel" ~ "Cuadrante 237A",
      quadrant == "Cuadrante 238" & district == "Pudahuel" ~ "Cuadrante 237A",
      quadrant == "Cuadrante 25C" & district == "Providencia" ~ "Cuadrante 125",
      quadrant == "Cuadrante 25C" & district == "Recoleta" ~ "Cuadrante 25A",
      quadrant == "Cuadrante 26A" & district == "Providencia" ~ "Cuadrante 126",
      quadrant == "Cuadrante 36B" & district == "Colina" ~ "Cuadrante 36",
      quadrant == "Cuadrante 36C" & district == "Colina" ~ "Cuadrante 36",
      quadrant == "Cuadrante 37B" & district == "Colina" ~ "Cuadrante 37",
      quadrant == "Cuadrante 38B" & district == "Colina" ~ "Cuadrante 38",
      quadrant == "Cuadrante 39B" & district == "Colina" ~ "Cuadrante 39",
      quadrant == "Cuadrante 59" & district == "La Cisterna" ~ "Cuadrante 59A",
      TRUE ~ quadrant
    )
  )

# Join geo data
crime_data <- crime_data |> 
  left_join(quad_geo |> dplyr::select(quadrant, com_code, geometry), by = "quadrant")

glimpse(crime_data)

# District maps 
com_geo <- chilemapas::codigos_territoriales |> 
  mutate(codigo_comuna=as.numeric(codigo_comuna)) |> 
  filter(codigo_region==13) |> 
  dplyr::select(codigo_comuna, nombre_comuna) |> 
  left_join(chilemapas::mapa_comunas |> 
    dplyr::select(codigo_comuna, geometry) |> 
    mutate(codigo_comuna=as.numeric(codigo_comuna))
)

# Shape file
quad_geo_shp <- st_as_sf(quad_geo)
st_crs(quad_geo_shp)

com_geo_shp <- st_as_sf(com_geo)
st_crs(com_geo_shp)

## Save all data -----
save(crime_data, file = paste0(data_out, "Crime_data_RM_2017_2025.RData"))
save(quad_geo, file = paste0(data_out, "Quadrant_data_geo_RM.RData"))
save(com_geo, file = paste0(data_out, "District_data_geo_RM.RData"))

# Shape file export
st_write(quad_geo_shp, paste0(data_out, "quad_geo/", "quad_geo.shp"), delete_layer = TRUE)
st_write(com_geo_shp, paste0(data_out, "district_geo/", "district_geo.shp"), delete_layer = TRUE)

# Plot crime distribution by quadrant (count per family × quadrant) ----
## Metro study area (same clip as 5.0 CENSO_data_zone.R) ----
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

maps_crime_dir <- "03_Output/Descriptives/Maps_crime/"

crime_by_quad_family <- crime_data |>
  sf::st_drop_geometry() |>
  dplyr::count(.data$family, .data$quadrant, name = "n_crimes")

quad_unique <- quad_geo_shp |>
  sf::st_transform(4326) |>
  dplyr::distinct(.data$quadrant, .keep_all = TRUE)

quad_unique_metro <- sf::st_intersection(
  sf::st_make_valid(quad_unique),
  sf::st_make_valid(stgo_urb_union)
) |>
  dplyr::group_by(.data$quadrant) |>
  dplyr::summarise(geometry = sf::st_union(.data$geometry), .groups = "drop") |>
  sf::st_as_sf()

crime_family_filename <- function(fam) {
  gsub("_+", "_", gsub("^_|_$", "", gsub("[^A-Za-z0-9]+", "_", as.character(fam))))
}

## Same theme / choropleth style as 5.0 CENSO_data_zone.R ----
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

crime_family_levels <- levels(crime_data$family)

for (i in seq_along(crime_family_levels)) {
  fam <- crime_family_levels[[i]]
  pal_opt <- viridis_opts[[((i - 1L) %% length(viridis_opts)) + 1L]]
  title_chr <- paste0(fam)

  counts_f <- crime_by_quad_family |>
    dplyr::filter(.data$family == fam) |>
    dplyr::select("quadrant", "n_crimes")

  geo_f <- quad_unique_metro |>
    dplyr::select("quadrant", "geometry") |>
    dplyr::left_join(counts_f, by = "quadrant") |>
    dplyr::mutate(n_crimes = tidyr::replace_na(.data$n_crimes, 0L))

  p_f <- ggplot2::ggplot(geo_f) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data$n_crimes),
      color = grDevices::gray(0.85),
      linewidth = 0.1
    ) +
    ggplot2::scale_fill_viridis_c(
      option = pal_opt,
      na.value = "grey90",
      direction = -1
    ) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = title_chr) +
    map_theme_census()

  ggplot2::ggsave(
    filename = paste0(maps_crime_dir, "map_crime_count_", crime_family_filename(fam), ".png"),
    plot = p_f,
    width = 7,
    height = 8,
    dpi = 150,
    create.dir = TRUE
  )
}

