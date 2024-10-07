library(dplyr)
library(usethis)
library(sf)
library(lubridate)

# Dates -------------------------------------------------------------------

yrs = as.character(2022:2024)
use_data(yrs, overwrite = TRUE)

# used to create weekly/biweekly periods starting from these dates
# first monday in the week when sampling started
first_monday = c("2022-05-16", "2023-05-15", "2024-05-06") |>
  setNames(yrs)
use_data(first_monday, overwrite = TRUE)

biweek_start = lapply(yrs, function(x){
  data.frame(period_start = seq(from = ymd(first_monday[[x]]),
                                to = ymd(paste0(x, "-12-20")),
                                by = "2 weeks")) |>
    mutate(biweek = TahoeKeysCMT::isobiweek(period_start, first_monday[[x]]))
}) |>
  setNames(yrs)
use_data(biweek_start, overwrite = TRUE)

week_start = lapply(yrs, function(x){
  data.frame(period_start = seq(from = ymd(first_monday[[x]]),
                                to = ymd(paste0(x, "-12-20")),
                                by = "weeks")) |>
    mutate(week = isoweek(period_start))
}) |>
  setNames(yrs)
use_data(week_start, overwrite = TRUE)

# Locations ---------------------------------------------------------------

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

# Macrophytes -------------------------------------------------------------

# rake fullness lookup table for popup display
rake_lu = data.frame(rake_biomass_fullness = c(0, 1, 6, 26, 51, 76),
                     rake_biomass_fullness_per = c("0%", "1-5%", "6-25%", "26-50%",
                                                   "51-75%", "76+%"))
use_data(rake_lu, overwrite = TRUE)

species_df = read.csv(file.path("data-raw", "species_df.csv")) |>
  # bumping the focal species to the front of the list
  mutate(species = factor(species, levels = c("coontail", "curlyleaf_pondweed", "eurasian_watermilfoil", "elodea",
                                              "richardsons_pondweed", "water_smartweed", "watershield", "andean_watermilfoil",
                                              "bladderwort", "chara", "elodea_nuttallii", "leafy_pondweed", "najas", "nitella",
                                              "northern_watermilfoil", "quillwort", "sagittaria", "sago_pondweed", "spirogyra",
                                              "variable_leaf_pondweed", "white_water_buttercup", "white_water_crowfoot",
                                              "yellow_pond_lily", "filamentous_algae"))) |>
  arrange(species)
use_data(species_df, overwrite = TRUE)

# Nutrients ---------------------------------------------------------------

nut_site_lu = read.csv(file.path("data-raw", "Nutrient_SiteLookup.csv")) |>
  mutate(site_num = as.character(site_num))
use_data(nut_site_lu, overwrite = TRUE)

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

analytes = names(analyte_limits)
use_data(analytes, overwrite = TRUE)

nut_lab_raw = readxl::read_excel(file.path("data-raw", "Year2_Nutrients_Master20231113.xlsx"),
                                 sheet = "CMT_Nutrients_Year1") |>
  mutate(year = "2022") |>
  bind_rows(read.csv(file.path("data-raw", "Year2_nutrients_master20231113_for_QA_r.csv")) |>
              mutate(year = "2023",
                     Date_Collected = mdy(Date_Collected),
                     Date_Received = mdy(Date_Received),
                     Date_Analyzed = mdy(Date_Analyzed))) |>
  bind_rows(readxl::read_xlsx(file.path("data-raw", "Year3_nutrients_master2024105.xlsx"), sheet = "RawData") |>
              mutate(year = "2024"))
use_data(nut_lab_raw, overwrite = TRUE)

nut_lab = nut_lab_raw |>
  mutate(ESA_omit_data = ifelse(ESA_omit_data == "", NA_character_, ESA_omit_data)) |>
  filter(qa_type == "field_primary" & WC_Sample_Site_Name != "Site 11 Duplicate" & is.na(ESA_omit_data)) |>
  left_join(nut_site_lu) |>
  # need to compare unique Site_Number to what is in nut_site_lu to make sure that this next step is dropping correct records
  filter(!is.na(site_num)) |>
  select(year, site_num, group_b_name, sample_num = Lab_Samp_No,
         date = Date_Collected, analyte = Analyte, value = Result) |>
  # ended with with rows that have same Result value to go with same grouping variables
  # dropping them here (with unique) even though not sure how those duplicates entered the dataset
  unique() |>
  mutate(date = as.Date(date), # convert from POSIXct to Date
         week = isoweek(date),
         group_b_method = TahoeKeysCMT::get_gbm(group_b_name),
         group_b_loc = TahoeKeysCMT::get_gbl(group_b_name))

total_n <- nut_lab |>
  filter(analyte %in% c("Nitrate + Nitrite Nitrogen", "Total Kjeldahl Nitrogen"))|>
  tidyr::pivot_wider(names_from = analyte, values_from = value)|>
  mutate(analyte = "Total Nitrogen",
         value = round(`Nitrate + Nitrite Nitrogen` + `Total Kjeldahl Nitrogen`, 4)) |>
  select(-`Nitrate + Nitrite Nitrogen`, -`Total Kjeldahl Nitrogen`)

nut_all = bind_rows(nut_lab, total_n) |>
  left_join(bind_rows(TahoeKeysCMT::week_start, .id = "year")) |>
  filter(analyte %in% analytes)
use_data(nut_all, overwrite = TRUE)

nut_all_list = nut_all |>
  tidyr::pivot_wider(names_from = "analyte", values_from = "value") |>
  split(~ year)
use_data(nut_all_list, overwrite = TRUE)

# BMI ---------------------------------------------------------------------

traits_raw = read.csv(file.path("data-raw", "BMI_Traits.csv"))
use_data(traits_raw, overwrite = TRUE)

traits = traits_raw |>
  TahoeKeysCMT::fix_names() |>
  rename(insect = `insect_`) |>
  # WY HBI index index of 11 indicates a taxa that is discarded from the calculation
  mutate(wy_hbi = ifelse(wy_hbi == 11, NA, wy_hbi))
use_data(traits, overwrite = TRUE)

bmi_raw = read.csv(file.path("data-raw", "BMI_2021-2023.csv"))
use_data(bmi_raw, overwrite = TRUE)

bmi = bmi_raw |>
  TahoeKeysCMT::fix_names() |>
  unique() |>
  mutate(date = ymd(date),
         year = year(date),
         month_abb = factor(month.abb[month(date)], levels = month.abb[1:12]),
         replicate = as.character(replicate),
         habitat = ifelse(habitat == "mid-channel", "Mid-Channel", "Shoreline"),
         order = ifelse(order %in% c("x", "miscellaneous non-insect"), "N/A", order)) |>
  rename(site_num = replicate, relative_location = habitat) |>
  left_join(traits) |>
  left_join(data.frame(feeding_group = c("CF", "CG", "MH", "PA", "PH", "PR", "SC"),
                       fg = c("Collector-filterer", "Collector-gatherer", "Other",
                              "Parasite", "Other", "Predator", "Scraper"))) |>
  select(year, month_abb, date, site_num, relative_location, order, taxon, feeding_group = fg, abundance, wy_hbi) |>
  mutate(feeding_group = ifelse(is.na(feeding_group), "N/A", feeding_group))
use_data(bmi, overwrite = TRUE)

bmi_ta = bmi |>
  group_by(year, month_abb, date, site_num, relative_location) |>
  summarise(total_abundance = sum(abundance, na.rm = TRUE))
use_data(bmi_ta, overwrite = TRUE)
