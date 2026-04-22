# Settings ---- 

options(scipen=999)
options(max.print = 99999999)
options(knitr.kable.NA = '', OutDec = ".") 
knitr::opts_chunk$set("ft.shadow" = FALSE)
rm(list=(ls()))

# Local figures text
#Sys.setlocale(category = "LC_ALL", "es_ES.UTF-8") #LAT
Sys.setlocale(category = "LC_ALL", "en_US.UTF-8") #USA

# Function install/load packages
install_load <- function(packages){
  for (i in packages) {
    if (i %in% rownames(installed.packages())) {
      library(i, character.only=TRUE)
    } else {
      install.packages(i)
      library(i, character.only = TRUE)
    }
  }
}

# Apply function common packages 
install_load(c("rio", 
               "janitor", 
               "tidyverse", 
               "openxlsx",
               "chilemapas", 
               "patchwork",
               "sf", 
               "ggpubr", 
               "data.table",
               "vtable",
               "naniar", 
               "visdat", 
               "VIM",
               "rpart", 
               "rpart.plot", 
               "parallel", 
               "profvis", 
               "htmlwidgets",
               "future", 
               "purrr", 
               "furrr",
               "future.apply", 
               "zoo",
               "splines",      
               "dlnm",
               "mgcv",
               "magrittr",
               "r2symbols",
               "plotly",      
               "nlme",
               "ggstatsplot",
               "tidymodels",
               "knitr", 
               "kableExtra",
               "Epi",
               "metR", 
               "mvmeta",
               "ncdf4",
               "tidync", 
               "rix", # Reproducible environments and package
               "tictoc",
               "paletteer",
               "texreg",
               "tidymodels", 
               "broom",
               "RColorBrewer",  
               "fixest", 
               "sandwich", 
               "lmtest", 
               "modelsummary", 
               "MASS", 
               "rnaturalearth", 
               "rnaturalearthdata",
               "ggforce",
               "ggspatial", 
               "ggmap",
               "ggmapinset",
               "gridExtra",
               "ggrepel",
               "survey", 
               "imputeTS",
               "maptiles",
               "tidyterra",
               "arrow"
               ))

# Extra 
#devtools::install_github("ropensci/rnaturalearthhires")

# Palettes

# Opción 1 — azul pizarra
blues_pal <- c(
  "#E8EDF5",
  "#CDD6EA",
  "#AEBEDD",
  "#8EA5CF",
  "#6D8CBF",
  "#4F73AD",
  "#375C92",
  "#244776",
  "#14325A",
  "#071E3D"
)

# Opción 2 — ciruela
vi_pal <- c(
  "#F2EAF5",
  "#E0CCEA",
  "#CAAEDD",
  "#B390CE",
  "#9972BC",
  "#7E56A8",
  "#643E90",
  "#4C2A76",
  "#35195C",
  "#1E0A40"
)

# Opción 3 — tierra tostada
terra_pal <- c(
  "#F5EDE6",
  "#EAD6C8",
  "#DCBDA8",
  "#CCA086",
  "#B88265",
  "#A06448",
  "#854B31",
  "#6A351E",
  "#4F2210",
  "#331206"
)


temp_max_10 <- c(
  "#313695",  # muy frío
  "#4575B4",
  "#74ADD1",
  "#ABD9E9",
  "#E0F3F8",
  "#FFFFBF",  # neutro / transición
  "#FEE090",
  "#FDAE61",
  "#F46D43",
  "#A50026"   # extremo calor
)