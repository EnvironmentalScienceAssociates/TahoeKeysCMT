
#' Process Fulcrum photo ID string
#'
#'
#' @md
#' @param x     Vector of strings potentially comprised of more than one photo ID
#'
#' @export
#'

process_photo_id <- function(x){
  # for simplicity, just keep first photo id
  if (grepl(",", x)) strsplit(x, ",")[[1]][1] else x
}

#' Calculate mean and standard error
#'
#'
#' @md
#' @param grp_df        Grouped dataframe
#' @param column_name   Name of column in grp_df to calculate mean and SE
#'
#' @export
#'

mean_se <- function(grp_df, column_name){
  grp_df |>
    dplyr::summarise(avg = mean(.data[[column_name]], na.rm = TRUE),
                     se = plotrix::std.error(.data[[column_name]], na.rm = TRUE)) |>
    dplyr::mutate(lwr = avg - se,
                  upr = avg + se)
}

#' Calculate species frequency
#'
#'
#' @md
#' @param grp_df        Grouped dataframe
#'
#' @export
#'

freq_summ <- function(grp_df){
  grp_df |>
    dplyr::summarise(species_count = sum(species_count, na.rm = TRUE),
                     samples = sum(samples, na.rm = TRUE)) |>
    dplyr::mutate(freq = species_count/samples * 100)
}

#' Check if an object is a number
#'
#'
#' @md
#' @param x        Object to be tested
#'
#' @export
#'

is_number <- function(x) is.na(suppressWarnings(as.numeric(x)))


#' Get group B method from group B name
#'
#' @md
#' @param group_b_name          Vector of group B names, e.g., 10DASHa2023
#'
#' @export
#'

get_gbm <- function(group_b_name){
  dplyr::case_when(
    grepl("UV", group_b_name) ~ "UVC Spot",
    grepl("BB", group_b_name) ~ "BB",
    grepl("DASH", group_b_name) ~ "DASH",
    .default = "N/A")
}

#' Get group B location (A, B, C, N/A) from group B name
#'
#' @md
#' @param group_b_name          Vector of group B names, e.g., 10DASHa2023
#'
#' @export
#'

get_gbl <- function(group_b_name){
  pos = nchar(group_b_name) - 4
  out = toupper(substr(group_b_name, pos, pos))
  ifelse(is.na(out), "N/A", out)
}
