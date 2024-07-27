#' Get main nutrient table
#'
#'
#' @md
#' @param yr             Four-digit year as a character
#'
#' @export
#'

get_nut <- function(yr = c("2022", "2023", "2024")){
  fulcrum_tables = c("2022" = "Tahoe Keys - Nutrients",
                     "2023" = "CMT_Nutrients_Year2",
                     "2024" = "CMT_Nutrients_Year 3")
  fulcrum_table_try(fulcrum_tables[[yr]])
}

#' Prepare main nutrient table
#'
#'
#' @md
#' @param yr             Four-digit year as a character
#'
#' @export
#'

prep_nut <- function(yr = c("2022", "2023", "2024")){
  get_nut(yr) |>
    dplyr::rename(nut_id = `_record_id`) |>
    dplyr::select(nut_id, date) |>
    dplyr::mutate(week = lubridate::isoweek(date)) |>
    dplyr::left_join(week_start[[yr]])
}

#' Get nutrient site visit table
#'
#'
#' @md
#' @param yr             Four-digit year as a character
#'
#' @export
#'

get_nut_site <- function(yr){
  fulcrum_tables = c("2022" = "Tahoe Keys - Nutrients/site",
                     "2023" = "CMT_Nutrients_Year2/site",
                     "2024" = "CMT_Nutrients_Year 3/site")
  fulcrum_table_try(fulcrum_tables[[yr]])
}

#' Prepare nutrient site visit table
#'
#'
#' @md
#' @param yr             Four-digit year as a character
#' @param nut            Nutrient dataframe returned from get_nut
#'
#' @export
#'

prep_nut_site <- function(yr, nut){
  sel_cols = c("site_id", "nut_id", "site_name", "sample_type", "cyanobacteria_present", "hab_observed")
  nut_site = get_nut_site(yr) |>
    dplyr::rename(nut_id = `_parent_id`,
                  site_id = `_child_record_id`)
  if (yr == "2022"){
    nut_site = nut_site |>
      dplyr::rename(hab_observed = harmful_algal_blooms_observed) |>
      dplyr::mutate(site_name = dplyr::case_when(
        site_name %in% c("Site 25 control", "Site 25 (HABs)") ~ "25",
        site_name == "Site 9 (HABs)" ~ "9",
        site_name == "Site 13 (HABs)" ~ "13",
        site_name == "Site 12 (HABS)" ~ "12",
        TRUE ~ site_name),
        site_num = site_name) |>
      dplyr::filter(!is.na(as.numeric(site_num)))  # lots of different site names in 2022
  }
  if (yr != "2022"){
    sel_cols = c(sel_cols, "group_b_method")
    nut_site = dplyr::rename(nut_site, hab_observed = habs_observed)
  }

  nut_site = nut_site |>
    dplyr::select(dplyr::all_of(sel_cols)) |>
    dplyr::mutate(Site_Number = ifelse(site_name %in% c("Rinsate Blank", "Porta Potty Calibration"),
                                       NA_integer_, gsub("Site ", "", site_name)),
                  cyanobacteria_present = ifelse(is.na(cyanobacteria_present) | cyanobacteria_present == "no", "No", "Yes"),
                  hab_observed = ifelse(is.na(hab_observed) | hab_observed == "no", "No", "Yes")) |>
    dplyr::left_join(nut_site_lu) |>
    dplyr::right_join(nut)

  if (yr == "2023"){
    nut_site = get_gbm(nut_site)
  }
  nut_site
}

#' Get nutrient samples table
#'
#'
#' @md
#' @param yr             Four-digit year as a character
#'
#' @export
#'

get_nut_samples <- function(yr){
  fulcrum_tables = c("2022" = "Tahoe Keys - Nutrients/nutrient_sample",
                     "2023" = "CMT_Nutrients_Year2/nutrient_sample",
                     "2024" = "CMT_Nutrients_Year 3/nutrient_sample")
  fulcrum_table_try(fulcrum_tables[[yr]])
}

#' Prepare nutrient samples table
#'
#'
#' @md
#' @param yr             Four-digit year as a character
#' @param nut_site       Nutrient site visit dataframe returned from get_nut_site
#'
#' @export
#'

prep_nut_samples <- function(yr, nut_site){
  get_nut_samples(yr) |>
    dplyr::select(nut_id = `_record_id`,
                  site_id = `_parent_id`,
                  samples_id = `_child_record_id`,
                  lat = `_latitude`,
                  lon = `_longitude`,
                  subsite,
                  total_depth_ft) |>
    dplyr::mutate(subsite = toupper(subsite)) |>
    dplyr::left_join(nut_site) |>
    dplyr::filter(!is.na(Site_Number) & sample_type == "Primary")
}

#' Prepare nutrient spatial table
#'
#' @md
#' @param nut_samples    Nutrient samples dataframe returned from prep_nut_samples
#'
#' @export
#'

prep_nut_spatial <- function(nut_samples){
  nut_samples |>
    dplyr::mutate(id = samples_id, # unused; just need a column with this name
                  popup = paste0("Subsite: ", subsite, "<br>",
                                 "Sample Date: ", date, "<br>",
                                 "Total Depth: ", total_depth_ft, " ft")) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
}


