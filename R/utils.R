#' Utility Functions


#' Utility function to check for internet
#' @importFrom attempt stop_if_not
#' @importFrom curl has_internet
#'
#' @name check_internet
#'
#'
check_internet <- function() {
  attempt::stop_if_not(
    .x = curl::has_internet(),
    "Please check your internet connection"
  )
}


#' Utility function to check API status
#'
#' @importFrom httr status_code
#'
#' @name check_status
#' @param res response from httr verb function
#'
check_status <- function(res) {
  attempt::stop_if_not(
    .x = httr::status_code(res),
    .p = ~ .x == 200,
    msg = "The API returned an error"
  )
}


base_url <- "https://banks.data.fdic.gov/api"
