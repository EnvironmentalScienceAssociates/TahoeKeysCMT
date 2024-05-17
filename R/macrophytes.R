
#' Get main macrophyte table
#'
#'
#' @md
#' @param yr             Four-digit year as a character
#'
#' @export
#'

get_mac <- function(yr = c("2022", "2023", "2024")){
  fulcrum_tables = c("2022" = "Tahoe Keys - Macrophytes",
                     "2023" = "CMT_Macrophytes_Year2",
                     "2024" = "CMT_Macrophytes_Year3")
  out = fulcrum_table_try(fulcrum_tables[[yr]]) |>
    dplyr::select(mac_id = `_record_id`, date) |>
    dplyr::mutate(biweek = isobiweek(date, first_monday[yr])) |>
    dplyr::left_join(biweek_start[[yr]])
  # dropping mistakenly duplicate main record (rematching happens in samples below)
  if (yr == "2022") out = dplyr::filter(out, !(mac_id %in% c("802e7971-09dc-4bd5-b2b2-95d90abacae0",
                                                             "6d9bba20-fa89-48c3-8d52-9e74852c21b0")))
  out
}

#' Get macrophyte site visit table
#'
#'
#' @md
#' @param yr             Four-digit year as a character
#' @param mac_df         Macrophyte dataframe returned from get_mac
#'
#' @export
#'

get_mac_site <- function(yr, mac_df){
  fulcrum_tables = c("2022" = "Tahoe Keys - Macrophytes/site_visit",
                     "2023" = "CMT_Macrophytes_Year2/site_visit",
                     "2024" = "CMT_Macrophytes_Year3/site_visit")
  site_df = fulcrum_table_try(fulcrum_tables[[yr]]) |>
    dplyr::select(site_id = `_child_record_id`, mac_id = `_parent_id`, site_name)
  if (yr == "2022"){
    site_df = site_df |>
      dplyr::mutate(site_name = paste("Site", site_name)) |>
      # dropping mistakenly duplicate site record (rematching happens in samples below)
      dplyr::filter(!(site_id %in% c("439c39ea-ff06-49de-8dae-611b11bc4a07", "bda0a1f5-aa55-443a-9f9d-118db2236756")))
  }
  site_df |>
    dplyr::mutate(site_num = gsub("Site ", "", site_name)) |>
    dplyr::left_join(mac_df, by = dplyr::join_by(mac_id)) |>
    # site 25 was moved in 2022; dropping samples from earlier periods
    dplyr::filter(!(site_num == 25 & date < lubridate::ymd("2022-06-10")))
}

#' Get macrophyte samples table
#'
#'
#' @md
#' @param yr             Four-digit year as a character
#' @param site_df        Macrophyte site visit dataframe returned from get_mac_site
#'
#' @export
#'

get_mac_samples <- function(yr, site_df){
  fulcrum_tables = c("2022" = "Tahoe Keys - Macrophytes/macrophyte_samples",
                     "2023" = "CMT_Macrophytes_Year2/macrophyte_samples",
                     "2024" = "CMT_Macrophytes_Year3/macrophyte_samples")
  samples_df = fulcrum_table_try(fulcrum_tables[[yr]]) |>
    dplyr::rename(mac_id = `_record_id`,
                  site_id = `_parent_id`,
                  samples_id = `_child_record_id`,
                  lat = `_latitude`,
                  lon = `_longitude`,
                  subsite = rake_subsite,
                  depth_ft = macrophyte_depth,
                  nitella_relative_abundance = nitella_realtive_abundance) |>
    dplyr::select(!starts_with("_")) |>
    dplyr::mutate(rake_photos = sapply(rake_photos,
                                       # for simplicity, just keep first photo id
                                       function(x) if (grepl(",", x)) strsplit(x, ",")[[1]][1] else x,
                                       USE.NAMES = FALSE)) |>
    # adding dates for filtering in sample size counts
    dplyr::left_join(site_df, by = dplyr::join_by(mac_id, site_id))
  if (yr == "2022"){
    samples_df = samples_df |>
      # matching 3 rake samples to a different main record
      dplyr::mutate(mac_id = ifelse(mac_id %in% c("802e7971-09dc-4bd5-b2b2-95d90abacae0", "6d9bba20-fa89-48c3-8d52-9e74852c21b0"),
                                    "cb9951ed-84c7-4461-93ea-5f4f1ac65ab4", mac_id),
                    site_id = ifelse(site_id %in% c("439c39ea-ff06-49de-8dae-611b11bc4a07", "bda0a1f5-aa55-443a-9f9d-118db2236756"),
                                     "a7229e5e-c0e7-4754-851c-3975b0aeacd2", site_id))
  }
  if (yr != "2022"){
    samples_df = dplyr::mutate(samples_df, group_b_method = ifelse(group_b_method == "\" \"", "N/A", group_b_method))
  }
  samples_df
}
