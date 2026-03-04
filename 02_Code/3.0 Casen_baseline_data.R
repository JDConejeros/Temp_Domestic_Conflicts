# Code 3: CASEN 2017 (BASELINE) ----

rm(list=ls())
## Settings ----
source("02_Code/0.1 Functions.R")
source("02_Code/0.2 Settings.R")

# Data path 
data_inp <- "01_Data/Input/"
data_out <- "01_Data/Output/"

# CASEN DATA 2017 ----

# Open data and adjust variables 
casen17 <- "Casen 2017.dta"

casen <- rio::import(paste0(data_inp, "CASEN/", casen17)) |> 
  clean_names() |> 
  filter(region==13) |> 
  dplyr::select(comuna, expc, varstrat, varunit, qautr, pobreza, pobreza_multi_4d) |> 
  drop_na()

# Create quintile dummy variables from qautr (quintil autodeclarado)
quintiles_dummy <- as.data.frame(make_dummies(casen$qautr, prefix = "q_"))
colnames(quintiles_dummy) <- paste0("q", 1:5)
casen <- bind_cols(casen, quintiles_dummy)

pobreza_multi_dummy <- as.data.frame(make_dummies(casen$pobreza_multi_4d, prefix = "p_"))
colnames(pobreza_multi_dummy) <- c("no_pob", "pob_multi")
casen <- bind_cols(casen, pobreza_multi_dummy)

pobreza_ing_dummy <- as.data.frame(make_dummies(casen$pobreza, prefix = "p_"))
colnames(pobreza_ing_dummy) <- c("pob_extreme", "pob", "no_pob")
casen <- bind_cols(casen, pobreza_ing_dummy)
casen <- casen |> dplyr::select(!starts_with("no_pob"))

glimpse(casen)

# Complex survey design for CASEN 2017
# Use varstrat as strata (not estrato)
# Use varunit as id
design <- svydesign(id = ~varunit, strata = ~varstrat, weights = ~expc, data = casen)

# Quintile Income: % of persons in each income quintile by district
quintil <- svyby(~q1 + q2 + q3 + q4 + q5 + pob_multi + pob + pob_extreme,
                 ~comuna, design, svymean, na.rm = TRUE) |> 
  mutate(across(starts_with("q"), ~ . * 100)) |> 
  mutate(across(starts_with("pob"), ~ . * 100)) |> 
  dplyr::select(comuna, q1, q2, q3, q4, q5, pob_multi, pob, pob_extreme) %>%
  mutate(quintil = apply(dplyr::select(., q1, q2, q3, q4, q5), 1, function(x) names(which.max(x))))

# Add year and save CASEN 2017 data
casen_mun <- quintil 
glimpse(casen_mun)

# Save data 
save(casen_mun, file=paste0(data_out, "Poverty_casen_RM_2017", ".RData"))

