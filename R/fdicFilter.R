
#' FDIC Filter Query String
#'
#'
#' @name fdicFilter
#'
#' @importFrom glue glue, glue_collapse
#'
#' @param filters a list in the format field = value,value,etc...
#' @param inclusive Logic to determine the inclusive or exclusive of the bounded parameter for date and numeric fields
#' @param exact if an exact value for string fields
#'
#'
#'
#' @return
#' @export
#'
#' @examples
fdicFilter <- function(filters = NULL, inclusive = TRUE, exact = TRUE) {
  field <- names(filters)
  value <- c()


  for (i in 1:length(field)) {

    # check to see if date type filter
    if (is.numeric(filters[[i]][1])) {
      v <- gsub("Inf", "*", as.character(filters[[i]])) %>%
        glue::glue_collapse(sep = " TO ")

      if (length(filters[[i]]) > 1) {
        if (inclusive) {
          v <- glue::glue("[{v}]")
        } else {
          v <- glue::glue("{<v>}", .open = "<", .close = ">")
        }
      }
    } else if (class(try(as.Date(filters[[i]][1], tryFormats = "%Y-%m-%d"), silent = TRUE)) != "try-error" |
               class(try(as.Date(filters[[i]][2], tryFormats = "%Y-%m-%d"), silent = TRUE)) != "try-error") {

      # Given a date, collapse with Date1 TO Date2
      v <- glue::glue_collapse(filters[[i]], sep = " TO ")

      # Given that more than 1 date was provided, add inclusive/exclusive
      if (length(filters[[i]]) > 1) {
        if (inclusive) {
          v <- glue::glue("[{v}]")
        } else {
          v <- glue::glue("{<v>}", .open = "<", .close = ">")
        }
      }
    } else {
      if (exact) {
        v <- glue::glue_collapse(glue::glue("\"{filters[[i]]}\""), sep = ",")
      } else {
        v <- glue::glue_collapse(filters[[i]], sep = ",")
      }



      if (length(filters[[i]]) > 1) {
        v <- glue::glue("({v})")
      }
    }
    value[i] <- glue::glue("{field[i]}:{v}")
  }


  field_values <- glue::glue_collapse(value, sep = " AND ")
  return(field_values)
}
