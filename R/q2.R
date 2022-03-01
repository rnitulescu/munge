# Second quartile (median)
#
# Returns the second quartile (i.e., median) of the input values, excluding NA's. Returns NA if all values are NA.
# @param x numeric vector.
# @param NUMLST character vector of valid class types.
# @keywords q2
#' @importFrom stats quantile
# @examples
# q2(c(1, 2, 3, NA))
q2 <- function(x, NUMLST=MYNUMLST) {
	if ( !(class(x) %in% NUMLST) ) {
		stop("Invalid variable type")
	} else {
		return(quantile(x, probs=0.50, na.rm=TRUE))
	}
}

