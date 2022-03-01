# Arithmetic mean
#
# Returns the arithmetic mean of the input values, excluding NA's. Returns NA if all values are NA.
# @param x numeric vector.
# @param NUMLST character vector of valid class types.
# @keywords armean
# @examples
# armean(c(1, 2, 3, NA))
armean <- function(x, NUMLST=MYNUMLST) {
	if ( !(flatten(class(x)) %in% NUMLST) ) {
		stop("Invalid variable type")
	} else {
		return(mean(x, na.rm=TRUE))
	}
}

