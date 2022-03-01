# One-way tabulation
#
# Returns a one-way table counting the number (and percentage) of observations per category from a vector.
# @param x vector of factor or logical values.
# @param FACLST character vector of valid class types.
# @keywords One-way
#' @import data.table
# @examples
# tab1way(x=factor(c("A","B","B")))
# tab1way(x=c(TRUE,FALSE,FALSE))
tab1way <- function(x, FACLST=MYFACLST) {
	## Don't run if variables are not of logical or factor type
	if ( !(class(x) %in% FACLST) ) {
		stop("Invalid variable type")
	} else {
		## Compute frequency and proportion (but drop the FALSE entries as they are redundant for for logical types)
		if ( class(x) == "logical" ) {
			freq <- as.data.table(table(x, exclude="useNA"))[x != "FALSE"]
			perc <- as.data.table(prop.table(table(x, exclude="useNA")))[x != "FALSE"]
		} else {
			freq <- as.data.table(table(x, exclude="useNA"))
			perc <- as.data.table(prop.table(table(x, exclude="useNA")))
		}
		## Rename columns so we can merge freq and perc
		setnames(freq, old=c("x","N"), new=c("CAT","freq"))
		setnames(perc, old=c("x","N"), new=c("CAT","prop"))
		## Merge freq and perc on CAT
		y <- freq[perc, on="CAT"]
		## Convert proportion to percentage and drop prop column (forcing use of standard evaluation)
		set(y, j="perc", value = 100 * y[["prop"]])
		set(y, j="prop", value = NULL)
		## Return table
		return(y)
	}
}

