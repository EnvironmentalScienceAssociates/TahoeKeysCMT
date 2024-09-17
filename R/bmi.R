
#' Get main BMI tables
#'
#'
#' @md
#' @param yrs             Vector of four-digit years as a character
#'
#' @export
#'

get_bmi <- function(yrs = c("2022", "2023", "2024")){
  fulcrum_tables = c("2022" = "Tahoe Keys - Benthic Macro Invertebrates",
                     "2023" = "CMT_BMI_Year2",
                     "2024" = "CMT_BMI_Year3")
  lapply(yrs, function(yr) fulcrumr::fulcrum_table(fulcrum_tables[[yr]])) |>
    dplyr::bind_rows()
}

#' Prepare main BMI tables
#'
#'
#' @md
#' @param yrs             Vector of four-digit years as a character
#'
#' @export
#'

prep_bmi <- function(yrs = c("2022", "2023", "2024")){
  get_bmi(yrs) |>
    dplyr::rename(bmi_id = `_record_id`) |>
    dplyr::select(bmi_id, date) |>
    dplyr::mutate(year = year(date),
                  month_abb = factor(month.abb[month(date)], levels = month.abb[1:12]))
}

#' Get BMI site visit table
#'
#'
#' @md
#' @param yrs             Vector of four-digit years as a character
#'
#' @export
#'

get_bmi_site <- function(yrs = c("2023", "2024")){
  fulcrum_tables = c("2023" = "CMT_BMI_Year2/site_information",
                     "2024" = "CMT_BMI_Year3/site_information")
  lapply(yrs, function(yr) fulcrumr::fulcrum_table(fulcrum_tables[[yr]])) |>
    dplyr::bind_rows()
}

#' Prepare BMI site visit table
#'
#'
#' @md
#' @param yrs             Vector of four-digit years as a character
#'
#' @export
#'

prep_bmi_site <- function(yrs = c("2023", "2024")){
  get_bmi_site(yrs) |>
    dplyr::rename(bmi_id = `_parent_id`,
                  site_id = `_child_record_id`) |>
    dplyr::select(site_id, bmi_id, site_num = site_number)
}

#' Get BMI samples table
#'
#'
#' @md
#' @param yrs             Vector of four-digit years as a character
#'
#' @export
#'

get_bmi_samples <- function(yrs = c("2022", "2023", "2024")){
  fulcrum_tables = c("2022" = "Tahoe Keys - Benthic Macro Invertebrates/samples",
                     "2023" = "CMT_BMI_Year2/subsite_samples",
                     "2024" = "CMT_BMI_Year3/subsite_samples")
  lapply(yrs, function(yr) fulcrumr::fulcrum_table(fulcrum_tables[[yr]])) |>
    dplyr::bind_rows()
}

#' Prepare BMI samples table
#'
#' @md
#' @param yrs             Vector of four-digit years as a character
#'
#' @export
#'

prep_bmi_samples <- function(yrs = c("2022", "2023", "2024")){
  bmi_2022 = NULL
  if ("2022" %in% yrs){
    bmi_2022 = get_bmi_samples("2022") |>
      dplyr::select(bmi_id = `_parent_id`,
                    samples_id = `_child_record_id`,
                    lat = `_latitude`,
                    lon = `_longitude`,
                    site_num = site,
                    subsite,
                    relative_location,
                    depth_ft = depth) |>
      dplyr::mutate(relative_location = ifelse(relative_location == "Near Shore", "Shoreline",
                                               ifelse(relative_location == "Mid Channel", "Mid-Channel", relative_location))) |>
      dplyr::left_join(prep_bmi("2022"))
  }

  other_yrs = yrs[yrs != "2022"]
  bmi = NULL
  if (length(other_yrs) > 0){
    bmi = get_bmi_samples(other_yrs)|>
      dplyr::select(bmi_id = `_record_id`,
                    site_id = `_parent_id`,
                    samples_id = `_child_record_id`,
                    lat = `_latitude`,
                    lon = `_longitude`,
                    subsite,
                    relative_location,
                    depth_ft) |>
      dplyr::left_join(prep_bmi(other_yrs)) |>
      dplyr::left_join(prep_bmi_site(other_yrs))
  }

  dplyr::bind_rows(bmi_2022, bmi) |>
    dplyr::mutate(site_num = sub("Site ", "", site_num))
}

#' Prepare BMI spatial table
#'
#' @md
#' @param bmi_samples    BMI samples dataframe returned from prep_bmi_samples
#'
#' @export
#'
#'

prep_bmi_spatial <- function(bmi_samples){
  bmi_samples |>
    dplyr::mutate(id = samples_id, # unused; just need a column with this name
                  popup = paste0("Site: ", site_num, "<br>",
                                 "Subsite: ", subsite, "<br>",
                                 "Sample Date: ", date, "<br>",
                                 "Depth: ", depth_ft, " ft")) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
}


#' Fix column names in BMI laboratory data
#'
#' @md
#' @param data           Dataframe
#'
#' @export
#'

fix_names <- function(data){
  new_names = gsub("-|\\.|\\s+", "_", tolower(names(data)))
  setNames(data, new_names)
}

