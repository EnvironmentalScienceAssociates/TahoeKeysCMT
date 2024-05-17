

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


#' Modified version of fulcrumr::fulcrum_table
#'
#' @description
#' Catch connection errors and return NULL
#'
#' @md
#' @param table_name     Name of Fulcrum table
#'
#' @export
#'

fulcrum_table_try <- function(table_name){
  tryCatch(fulcrumr::fulcrum_table(table_name),
           error = function(err){
             # if fulcrum_table returns an error, then display message and return NULL
             message("Fulcrum connection error. Please try again.")
             NULL})
}
