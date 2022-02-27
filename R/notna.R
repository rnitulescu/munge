#' Count total values in field that are not NA's
#'
#' Returns the number of values in a vector (excluding NA's).
#' @param x vector of numeric values.
#' @keywords notna
#' @export
#' @examples
#' notna(c(1,2,3,NA,NA))
notna <- function(x) {
    length(x[!is.na(x)])
}

