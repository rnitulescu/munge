#' Count total values in field
#'
#' Returns the number of values in a vector (including NA's).
#' @param x vector of numeric values.
#' @keywords total
#' @export
#' @examples
#' total(c(1,2,3,NA,NA))
total <- function(x) {
    length(x)
}

