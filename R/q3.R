#' Third quartile
#'
#' Returns the third quartile of the input values, excluding NA's. Returns NA if all values are NA.
#' @param x numeric vector.
#' @param NUMLST character vector of valid class types.
#' @keywords q3
#' @importFrom stats quantile
#' @export
#' @examples
#' q3(c(1, 2, 3, NA))
q3 <- function(x, NUMLST=get_NUMLST()) {
	if ( !(class(x) %in% NUMLST) ) {
		stop("Invalid variable type")
	} else {
		return(quantile(x, probs=0.75, na.rm=TRUE))
	}
}

