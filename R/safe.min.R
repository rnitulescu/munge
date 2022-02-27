#' Safe Minima
#'
#' Returns the minima of the input values, excluding NA's. Returns NA if all values are NA.
#' @param ... numeric arguments.
#' @keywords min
#' @export
#' @examples
#' safe.min(c(1, 2, 3, NA))
safe.min <- function(...) {
	tryCatch(min(..., na.rm=TRUE), warning=function(w) {return(NA)})
}

