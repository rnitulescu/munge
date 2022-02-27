#' Safe Maxima
#'
#' Returns the maxima of the input values, excluding NA's. Returns NA if all values are NA.
#' @param ... numeric arguments.
#' @keywords max
#' @export
#' @examples
#' safe.max(c(1, 2, 3, NA))
safe.max <- function(...) {
	tryCatch(max(..., na.rm=TRUE), warning=function(w) {return(NA)})
}

