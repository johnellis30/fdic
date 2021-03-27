
#' FDIC Query Call
#'
#' @name fdicQuery
#'
#' @importFrom glue glue, glue_collapse
#' @importFrom httr content, GET
#' @importFrom tibble tibble
#' @importFrom dplyr rename_at, vars
#' @importFrom jsonlite fromJSON
#'
#'
#' @param filters a list of filters which will be transformed by fdicFilter
#' @param fields a vector of fields to pull in a query
#' @param dataset which dataset you want to pull from default: institutions
#' @param limit limit on records to pull from query
#' @param descriptive logical on whether to rename columns with labels
#'
#'
#' @return
#' @export
#'
#' @examples
#'
fdicQuery <- function(filters = NULL,
                      fields = NULL,
                      dataset = "institutions",
                      limit = 10L,
                      descriptive = FALSE) {
  base_url <- "https://banks.data.fdic.gov/api"

  url <- file.path(base_url, dataset)

  if (is.list(filters)) {
    filterstring <- fdicFilter(filters = filters)
  } else {
    filterstring <- filters
  }

  data("data_dictionary", envir = environment())

  dataset_fields <- data_dictionary$Field[data_dictionary$Dataset == dataset]

  testfields <- fields %in% dataset_fields

  if (!all(testfields)) {
    warning(glue::glue(
      glue::glue_collapse(fields[!testfields], sep = " and "),
      " field(s) are NOT in chosen dataset"
    ))
  }

  if (is.null(fields)) {
    fields <- dataset_fields
  } else {
    fields <- fields[testfields]
  }



  fieldstring <- glue::glue_collapse(fields, sep = ",")

  response <- httr::GET(
    url = url,
    query = list(
      filters = filterstring,
      fields = fieldstring,
      limit = limit
    )
  )

  data <- jsonlite::fromJSON(httr::content(response, "text"))$data$data %>%
    tibble::tibble()


  if (descriptive) {
    descriptive <- data_dictionary[data_dictionary$Field %in% colnames(data) & data_dictionary$Dataset == dataset, 2:3]

    data <- dplyr::rename_at(data, dplyr::vars(descriptive$Field), ~ descriptive$Label)
  }


  return(data)
}
