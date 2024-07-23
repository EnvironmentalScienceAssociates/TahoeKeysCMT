library(dplyr)
library(usethis)
library(sf)

yrs = as.character(2022:2024)
usethis::use_data(yrs, overwrite = TRUE)

# used to create weekly/biweekly periods starting from these dates
# first monday in the week when sampling started
first_monday = c("2022-05-16", "2023-05-15", "2024-05-06") |>
  setNames(yrs)
use_data(first_monday, overwrite = TRUE)

biweek_start = lapply(yrs, function(x){
  data.frame(period_start = seq(from = lubridate::ymd(first_monday[[x]]),
                                to = lubridate::ymd(paste0(x, "-12-20")),
                                by = "2 weeks")) |>
    mutate(biweek = TahoeKeysCMT::isobiweek(period_start, first_monday[[x]]))
}) |>
  setNames(yrs)
use_data(biweek_start, overwrite = TRUE)

# rake fullness lookup table for popup display
rake_lu = data.frame(rake_biomass_fullness = c(0, 1, 6, 26, 51, 76),
                     rake_biomass_fullness_per = c("0%", "1-5%", "6-25%", "26-50%",
                                                   "51-75%", "76+%"))
use_data(rake_lu, overwrite = TRUE)

species_df = read.csv(file.path("data-raw", "species_df.csv")) |>
  # bumping the focal species to the front of the list
  mutate(species = factor(species, levels = c("coontail", "curlyleaf_pondweed", "eurasian_watermilfoil", "elodea",
                                              "richardsons_pondweed", "water_smartweed", "watershield", "bladderwort",
                                              "chara", "leafy_pondweed", "najas", "nitella", "northern_watermilfoil",
                                              "quillwort", "sagittaria", "sago_pondweed", "spirogyra", "variable_leaf_pondweed",
                                              "white_water_buttercup", "yellow_pond_lily", "filamentous_algae"))) |>
  arrange(species)
use_data(species_df, overwrite = TRUE)

analyte_limits = c("Orthophosphate, as P" = NA_real_, "Total Phosphorous as P" = 0.008,
                   "Nitrate + Nitrite Nitrogen" = NA_real_, "Total Kjeldahl Nitrogen" = NA_real_,
                   "Total Nitrogen" = 0.15)
use_data(analyte_limits, overwrite = TRUE)

analyte_labels <- c("Orthophosphate, as P" = "Orthophosphate, as P (mg/L)",
                    "Total Phosphorous as P" = "Total Phosphorous (mg/L)",
                    "Nitrate + Nitrite Nitrogen" = "Nitrate + Nitrite Nitrogen (mg/L)",
                    "Total Kjeldahl Nitrogen" = "Total Kjeldahl Nitrogen (mg/L)",
                    "Total Nitrogen" = "Total Nitrogen (mg/L)")
use_data(analyte_labels, overwrite = TRUE)

group_b_colors = c("grey", "#9B0000", "#26F7FD", "#FFFF00") |>
  setNames(c("N/A", "BB", "DASH", "UVC Spot"))
use_data(group_b_colors, overwrite = TRUE)

group_b_sf = list("2023" = st_read(dsn = file.path("data-raw", "group_b_sf.geojson")),
                  "2024" = st_read(dsn = file.path("data-raw", "group_b_yr3.geojson")))
use_data(group_b_sf, overwrite = TRUE)

trt_colors = c("#0070C0", "#FF4500", "#FFBD31", "#006600", "#93C572", "#F83AEF", "#8A3CC4") |>
  setNames(c("Control", "Endothall", "Endothall + UV", "Triclopyr", "Triclopyr + UV", "UV", "LFA"))
use_data(trt_colors, overwrite = TRUE)

sites_sf = st_read(dsn = file.path("data-raw", "sites_sf.geojson"))
use_data(sites_sf, overwrite = TRUE)
