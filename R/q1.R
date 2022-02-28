# First quartile
#
# Returns the first quartile of the input values, excluding NA's. Returns NA if all values are NA.
# @param x numeric vector.
# @param NUMLST character vector of valid class types.
# @keywords q1
#' @importFrom stats quantile
# @examples
# q1(c(1, 2, 3, NA))
q1 <- function(x, NUMLST=get_NUMLST()) {
	if ( !(class(x) %in% NUMLST) ) {
		stop("Invalid variable type")
	} else {
		return(quantile(x, probs=0.25, na.rm=TRUE))
	}
}

