
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
  fulcrum_table_try(fulcrum_tables[[yr]])
}

#' Prepare main macrophyte table
#'
#'
#' @md
#' @param yr             Four-digit year as a character
#'
#' @export
#'

prep_mac <- function(yr = c("2022", "2023", "2024")){
  out = get_mac(yr) |>
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
#'
#' @export
#'

get_mac_site <- function(yr){
  fulcrum_tables = c("2022" = "Tahoe Keys - Macrophytes/site_visit",
                     "2023" = "CMT_Macrophytes_Year2/site_visit",
                     "2024" = "CMT_Macrophytes_Year3/site_visit")
  fulcrum_table_try(fulcrum_tables[[yr]])
}

#' Prepare macrophyte site visit table
#'
#'
#' @md
#' @param yr             Four-digit year as a character
#' @param mac            Macrophyte dataframe returned from get_mac
#'
#' @export
#'

prep_mac_site <- function(yr, mac){
  sel_columns = c("site_id", "mac_id", "site_name")
  if (yr != "2022") sel_columns = c(sel_columns, "cyanobacteria_present", "hab_observed")
  mac_site = get_mac_site(yr) |>
    dplyr::rename(site_id = `_child_record_id`, mac_id = `_parent_id`) |>
    dplyr::select(dplyr::all_of(sel_columns))
  if (yr == "2022"){
    mac_site = mac_site |>
      dplyr::mutate(site_name = paste("Site", site_name)) |>
      # dropping mistakenly duplicate site record (rematching happens in samples below)
      dplyr::filter(!(site_id %in% c("439c39ea-ff06-49de-8dae-611b11bc4a07", "bda0a1f5-aa55-443a-9f9d-118db2236756")))
  }
  if (yr != "2022"){
    mac_site = mac_site |>
      dplyr::mutate(cyanobacteria_present = ifelse(is.na(cyanobacteria_present) | cyanobacteria_present == "no", "No", "Yes"),
                    hab_observed = ifelse(is.na(hab_observed) | hab_observed == "no", "No", "Yes"))
  }
  mac_site |>
    dplyr::mutate(site_num = gsub("Site ", "", site_name)) |>
    dplyr::left_join(mac, by = dplyr::join_by(mac_id)) |>
    # site 25 was moved in 2022; dropping samples from earlier periods
    dplyr::filter(!(site_num == 25 & date < lubridate::ymd("2022-06-10")))
}

#' Get macrophyte samples table
#'
#'
#' @md
#' @param yr             Four-digit year as a character
#'
#' @export
#'

get_mac_samples <- function(yr){
  fulcrum_tables = c("2022" = "Tahoe Keys - Macrophytes/macrophyte_samples",
                     "2023" = "CMT_Macrophytes_Year2/macrophyte_samples",
                     "2024" = "CMT_Macrophytes_Year3/macrophyte_samples")
  fulcrum_table_try(fulcrum_tables[[yr]])
}

#' Prepare macrophyte samples table
#'
#'
#' @md
#' @param yr             Four-digit year as a character
#' @param mac_site       Macrophyte site visit dataframe returned from get_mac_site
#'
#' @export
#'

prep_mac_samples <- function(yr, mac_site){
  mac_samples = get_mac_samples(yr) |>
    dplyr::rename(mac_id = `_record_id`,
                  site_id = `_parent_id`,
                  samples_id = `_child_record_id`,
                  lat = `_latitude`,
                  lon = `_longitude`,
                  subsite = rake_subsite,
                  depth_ft = macrophyte_depth,
                  nitella_relative_abundance = nitella_realtive_abundance) |>
    dplyr::select(!starts_with("_")) |>
    dplyr::mutate(rake_photos = sapply(rake_photos, process_photo_id, USE.NAMES = FALSE)) |>
    # adding dates for dplyr::filtering in sample size counts
    dplyr::left_join(mac_site, by = dplyr::join_by(mac_id, site_id))
  if (yr == "2022"){
    mac_samples = mac_samples |>
      # matching 3 rake samples to a different main record
      dplyr::mutate(mac_id = ifelse(mac_id %in% c("802e7971-09dc-4bd5-b2b2-95d90abacae0", "6d9bba20-fa89-48c3-8d52-9e74852c21b0"),
                                    "cb9951ed-84c7-4461-93ea-5f4f1ac65ab4", mac_id),
                    site_id = ifelse(site_id %in% c("439c39ea-ff06-49de-8dae-611b11bc4a07", "bda0a1f5-aa55-443a-9f9d-118db2236756"),
                                     "a7229e5e-c0e7-4754-851c-3975b0aeacd2", site_id))
  }
  if (yr != "2022"){
    mac_samples = mac_samples |>
      dplyr::mutate(group_b_method = ifelse(group_b_method == "\" \"", "N/A", group_b_method),
                    group_b_method = factor(group_b_method, levels = c("N/A", "BB", "DASH", "UVC Spot")))
  }
  if (yr == "2023"){
    turions = fulcrum_table_try("CMT_Macrophytes_Year2/turions_add_record_and_measurement_for_each_turion_observed") |>
      dplyr::select(samples_id = `_parent_id`,
                    turions_id = `_child_record_id`,
                    turions_length = length_cm) |>
      dplyr::group_by(samples_id) |>
      dplyr::summarise(turions_length = max(turions_length, na.rm = TRUE))

    mac_samples = mac_samples |>
      dplyr::rename(turions_observed = turions_oberseved) |>
      dplyr::mutate(turions_observed = ifelse(is.na(turions_observed), "N/A",
                                              ifelse(turions_observed == "no", "No", "Yes"))) |>
      dplyr::left_join(turions) |>
      # 0 indicates that none of the turions have growth
      dplyr::mutate(turions_length = ifelse(turions_observed == "Yes" &
                                              (is.na(turions_length) | is.infinite(turions_length)),
                                            0, turions_length))
  }
  if (yr == "2024") {
    mac_samples = mac_samples |>
      dplyr::rename(turions_length = turion_length_range_cm_starting_0) |>
      dplyr::mutate(turions_observed = ifelse(is.na(turions_observed), "N/A",
                                              ifelse(turions_observed == "no", "No", "Yes")),
                    turions_length = ifelse(turions_length == -999, NA_real_, turions_length),
                    turions_length = ifelse(turions_observed == "Yes" & is.na(turions_length), 0, turions_length),
                    # turions_nogrowth and turions_growth are only currently used in popup
                    turions_nogrowth = dplyr::case_when(
                      turions_observed == "Yes" & is.na(number_of_turions_no_growth) ~ "0",
                      is.na(number_of_turions_no_growth) | number_of_turions_no_growth == -999 ~ "N/A",
                      .default = as.character(number_of_turions_no_growth)),
                    turions_growth = dplyr::case_when(
                      turions_observed == "Yes" & is.na(number_of_turions_with_growth) ~ "0",
                      is.na(number_of_turions_with_growth) | number_of_turions_no_growth == -999 ~ "N/A",
                      .default = as.character(number_of_turions_with_growth)))
  }
  mac_samples
}

#' Prepare macrophyte rakes table
#'
#'
#' @md
#' @param yr             Four-digit year as a character
#' @param mac_site       Macrophyte site visit dataframe returned from get_mac_site
#' @param mac_samples    Macrophyte samples dataframe returned from get_mac_samples
#'
#' @export
#'

prep_rakes <- function(yr, mac_site, mac_samples){
  cols = c("samples_id", "mac_id", "site_id", "lat", "lon", "subsite", "relative_location",
           "depth_ft", "rake_biomass_fullness", "rake_photos")
  cols_yr = list("2023" = c(cols, "group_b_method", "turions_observed", "turions_length"),
                 "2024" = c(cols, "group_b_method", "turions_observed", "turions_length",
                            "turions_nogrowth", "turions_growth"))
  rakes = NULL
  if (yr == "2022"){
    rakes = mac_site |>
      dplyr::left_join(select(mac_samples, all_of(cols)),
                       by = dplyr::join_by(mac_id, site_id)) |>
      dplyr::mutate(rake_biomass_fullness = dplyr::case_when(
        rake_biomass_fullness == "0%" ~ "0",
        rake_biomass_fullness == "1-5%" ~ "1",
        rake_biomass_fullness == "5" ~ "1",
        rake_biomass_fullness == "25" ~ "6",
        rake_biomass_fullness == "26-50%" ~ "26",
        rake_biomass_fullness == "50" ~ "26",
        rake_biomass_fullness == "51-75%" ~ "51",
        rake_biomass_fullness == "75" ~ "51",
        rake_biomass_fullness == "76%+" ~ "76",
        rake_biomass_fullness == "100" ~ "76",
        rake_biomass_fullness == "Needs Further Review" ~ NA_character_,
        TRUE ~ rake_biomass_fullness),
        rake_biomass_fullness = as.numeric(rake_biomass_fullness)) |>
      dplyr::filter(subsite != "R31" & site_num != 7)
  }
  if (yr != "2022"){
    rakes = dplyr::left_join(mac_site, dplyr::select(mac_samples, all_of(cols_yr[[yr]])),
                             by = dplyr::join_by(mac_id, site_id))
  }
  rakes |>
    dplyr::select(-mac_id, -site_id) |>
    dplyr::filter(!(is.na(lat) | is.na(lon)))
}

#' Prepare macrophyte species table
#'
#'
#' @md
#' @param mac_samples    Macrophyte samples dataframe returned from get_mac_samples
#' @param rakes          Macrophyte rakes dataframe returned from prep_rakes
#' @param species_df     Species dataframe included with this package
#'
#' @export
#'
#'

prep_mac_species <- function(mac_samples, rakes, species_df = TahoeKeysCMT::species_df){
  ra = process_samples(mac_samples, "relative_abundance") |>
    dplyr::mutate(relative_abundance = ifelse(is.na(relative_abundance), 0, relative_abundance))
  rs = process_samples(mac_samples, "rating_score")

  dplyr::left_join(ra, rs, by = dplyr::join_by(samples_id, species)) |>
    dplyr::left_join(species_df, by = dplyr::join_by(species)) |>
    # zeroing out species for abundance when health rating < 3
    # zeroing out instead of dplyr::filtering because retaining true zeros for summaries and plotting purposes
    dplyr::mutate(relative_abundance = ifelse(is.na(rating_score) | rating_score < 3, 0, relative_abundance)) |>
    dplyr::left_join(select(rakes, samples_id, rake_biomass_fullness), by = dplyr::join_by(samples_id)) |>
    dplyr::mutate(species_fullness = relative_abundance/100 * rake_biomass_fullness) |>
    dplyr::select(-rake_biomass_fullness) # drop this column so that it doesn't create messiness in future joins
}

#' Prepare macrophyte frequency table
#'
#'
#' @md
#' @param mac_species    Macrophyte species dataframe returned from prep_mac_species
#'
#' @export
#'
#'

prep_mac_freq <- function(mac_species){
  mac_species |>
    dplyr::group_by(species, species_label, samples_id) |>
    dplyr::summarise(species_count = length(species[relative_abundance > 0]),
                     samples = n()) |>
    dplyr::ungroup()
}

#' Prepare rakes spatial table
#'
#'
#' @md
#' @param yr             Four-digit year as a character
#' @param mac_species    Macrophyte species dataframe returned from prep_mac_species
#' @param rakes          Macrophyte rakes dataframe returned from prep_rakes
#'
#' @export
#'
#'

prep_rakes_spatial <- function(yr, mac_species, rakes){
  species_sub = mac_species |>
    dplyr::select(samples_id, species, relative_abundance, rating_score) |>
    dplyr::mutate(rating_score = ifelse(is.na(rating_score), "n/a", rating_score)) |>
    dplyr::filter(species %in% c("eurasian_watermilfoil", "curlyleaf_pondweed", "coontail", "elodea")) |>
    tidyr::pivot_wider(names_from = "species", values_from = c("relative_abundance", "rating_score"))

  out = dplyr::left_join(rakes, species_sub, by = dplyr::join_by(samples_id)) |>
    dplyr::left_join(rake_lu, by = dplyr::join_by(rake_biomass_fullness)) |>
    dplyr::mutate(id = rake_photos,
                  popup = paste0("Site: ", site_num, "<br>",
                                 "Subsite: ", subsite, "<br>",
                                 "Sample Date: ", date, "<br>",
                                 "Depth: ", depth_ft, " ft<br>",
                                 "Rake Fullness: ", rake_biomass_fullness_per, "<br>",
                                 '<a href="rake_photo.jpg" target="blank_">View Rake Photo</a><br>',
                                 "<b>Relative Abundance</b> <br>",
                                 # this name order is flipped from what is in Fulcrum; created in pivot_wider above
                                 "Eurasian Watermilfoil: ", relative_abundance_eurasian_watermilfoil, "%<br>",
                                 "Curlyleaf Pondweed: ", relative_abundance_curlyleaf_pondweed, "%<br>",
                                 "Coontail: ", relative_abundance_coontail, "%<br>",
                                 "Elodea: ", relative_abundance_elodea, "%<br>",
                                 "<b>Plant Health Condition</b> <br>",
                                 "Eurasian Watermilfoil: ", rating_score_eurasian_watermilfoil, "<br>",
                                 "Curlyleaf Pondweed: ", rating_score_curlyleaf_pondweed, "<br>",
                                 "Coontail: ", rating_score_coontail, "<br>",
                                 "Elodea: ", rating_score_elodea)) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  if (yr == "2023"){
    out = dplyr::mutate(out, popup = paste0(popup,
                                            "<br> <b>Turions</b> <br>",
                                            "Observed: ", turions_observed, "<br>",
                                            "Max Length: ", ifelse(is.na(turions_length), "N/A", paste(turions_length, "cm"))))
  }
  if (yr == "2024"){
    out = dplyr::mutate(out, popup = paste0(popup,
                                            "<br> <b>Turions</b> <br>",
                                            "Observed: ", turions_observed, "<br>",
                                            "No Growth: ", turions_nogrowth, "<br>",
                                            "Growth: ", turions_growth, "<br>",
                                            "Max Length: ", ifelse(is.na(turions_length), "N/A", paste(turions_length, "cm"))))
  }
  out
}


#' Process macrophyte samples for relative abundance and plant health
#'
#' @md
#' @param mac_samples    Macrophyte samples dataframe returned from get_mac_samples
#' @param type           Type of response: relative_abundance or rating_score
#'
#' @export
#'

process_samples <- function(mac_samples, type = c("relative_abundance", "rating_score")) {
  mac_samples |>
    dplyr::select(samples_id, dplyr::contains(type)) |>
    tidyr::pivot_longer(cols = dplyr::contains(type),
                        names_to = "species", values_to = type) |>
    dplyr::mutate(species = gsub(paste0("_", type), "", species))
}


#' Modified version of lubridate::isoweek
#'
#' @description
#' The goal is to assign a date to a biweek such that the first biweek in a period
#' has the same date as the Monday of the week when sampling started.
#'
#' @md
#' @param x              Date-time object
#' @param start-date     Date of Monday in first sampling week
#'
#' @export
#'

isobiweek <- function(x, start_date){
  wk = lubridate::isoweek(x)
  start_date = if (length(start_date) == 1) rep(start_date, length(x)) else start_date
  start_week = lubridate::isoweek(start_date)

  adj_week = ifelse(start_week %% 2 == 0,
                    ifelse(wk %% 2 == 0, wk, wk - 1),
                    ifelse(wk %% 2 == 0, wk, wk + 1))
  adj_week/2
}

