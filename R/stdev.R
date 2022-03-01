# Standard deviation
#
# Returns the standard deviation of the input values, excluding NA's. Returns NA if all values are NA.
# @param x numeric vector.
# @param NUMLST character vector of valid class types.
# @keywords stdev
#' @importFrom stats sd
# @examples
# stdev(c(1, 2, 3, NA))
stdev <- function(x, NUMLST=MYNUMLST) {
	if ( !(flatten(class(x)) %in% NUMLST) ) {
		stop("Invalid variable type")
	} else {
		return(sd(x, na.rm=TRUE))
	}
}

