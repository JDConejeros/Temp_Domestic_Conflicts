# Code 5.0: Descriptive analysis ----

rm(list=ls())
## Settings ----
source("03_Code/0.1 Functions.R")
source("03_Code/0.2 Settings.R")

# Data path 
data_inp <- "02_Data/Input/"
data_out <- "02_Data/Output/"
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

glimpse(crime_count)
glimpse(data_cov)
glimpse(quad_geo)

data_cov <- data_cov |> 
  left_join(quad_geo, by = "quad_code")

glimpse(data_cov)

## Spatial distribution of crimes -----

america <- ne_countries(scale = "medium", continent = "South America", returnclass = "sf")
chile <- ne_states(country = "Chile", returnclass = "sf")
santiago <- chile[chile$name == "Región Metropolitana de Santiago", ]

chile$color <- "gray50"
santiago$color <- "white"

america_centroids <- st_centroid(america) %>% 
  mutate(long = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) |> 
  filter(!iso_a3 %in% c("FLK", "CHL"))

base_map <- ggplot() +
  geom_sf(data = america, fill = "white", color = "black") +
  geom_sf(data = chile, aes(fill = color), color = "black") +
  geom_sf(data = santiago, fill = "white", color = "black") +  
  geom_text(data = america_centroids, aes(x = long, y = lat, label = iso_a3), size = 4, fontface="bold") + 
  scale_fill_manual(values = c("gray80", "gray50", "white")) +
  #annotation_scale(location = "bl", width_hint = 0.4, text_cex = 0.6, tick_height = 0.3) +
  labs(x=NULL, y=NULL, title = "A.") +
  theme_light() +
  theme(
      plot.title = element_text(size = 16),
      legend.position = "none", 
      #axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1), 
      plot.margin = margin(0, 0, 0, 0),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      strip.text.y = element_text(angle = 0),
      strip.background = element_rect(fill=NA, color="gray70"), 
      strip.text=element_text(color="black"),
      strip.text.y.left = element_text(angle = 0),
      
    )

zoom_map <- ggplot() +
  geom_sf(data = chile, fill = "gray70", color = "black") +
  geom_sf(data = santiago, fill = "white", color = "black") +
  coord_sf(xlim = c(-72, -69.5), ylim = c(-34.5, -32.5)) + 
  geom_sf_text(data=santiago, aes(label = name_en, geometry = geometry), size = 5, fontface="bold", color = "black", stat = "sf_coordinates") +
  labs(x=NULL, y=NULL) +
  theme_light() + 
  theme(
      legend.position = "none", 
      plot.title = element_text(size=11, hjust = 0.5),
      plot.margin = margin(0, 0, 0, 0),
      panel.grid = element_blank(),
      axis.text.y = element_text(size=9),
      axis.text.x = element_text(size=9, angle=45, hjust = 1),
      #axis.ticks = element_blank(),
      strip.text.y = element_text(angle = 0),
      strip.background = element_rect(fill=NA, color="gray70"), 
      strip.text=element_text(color="black"),
      strip.text.y.left = element_text(angle = 0)
    )

zoom_map

map_plot <- base_map + inset_element(zoom_map, 
                                        left = -0.1, 
                                        bottom = 0.2, 
                                        right = 0.5, 
                                        top = 0.5)

map_plot

ggsave(
  filename = paste0("04_Output/Figures/Maps.png"), 
  res = 300,
  width = 20,
  height = 25,
  units = 'cm',
  scaling = 1,
  device = ragg::agg_png) 

## Spatial distribution of crimes -----


## Descriptive stats crimes -----

