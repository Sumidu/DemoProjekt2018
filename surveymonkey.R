
#' Loads surveymonkey generated csv files that contain 2 line headers
#'
#' @param filename Character of the file location.
#'
#' @return raw data with a single header
#' @export
#'
#' @examples
load_surveymonkey_csv <- function(filename) {
  suppressWarnings(
    header <- read_csv(filename, n_max = 1))
  suppressWarnings(
    raw <- read_csv(filename, skip = 1)
  )
  col_names <- paste(names(header), names(raw))
  names(raw) <- col_names
  raw
}

name_variables <- function(data, columnvector) {
  oldnames <- names(data)
  l <- length(columnvector)
  names(data)[1:l] <- columnvector
  data
}
