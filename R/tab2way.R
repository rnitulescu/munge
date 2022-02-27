#' Two-way tabulation
#'
#' Returns a two-way table counting the number (and percentage) of observations per
#' category combination across two vectors.
#' @param x vector of factor or logical values.
#' @param byvar vector of factor or logical values (stratification).
#' @param FACLST character vector of valid class types.
#' @keywords Two-way
#' @import data.table
#' @importFrom stats reshape
#' @export
#' @examples
#' tab2way(x=factor(c("A","B","B","A")), byvar=factor(c("C","D","C","D")))
tab2way <- function(x, byvar, FACLST=get_FACLST()) {
	## Don't run if variables are not of logical or factor type
	if ( !(class(x) %in% FACLST) | !(class(byvar) %in% FACLST) ) {
		stop("Invalid variable type")
	## Don't run if there are missing values for the byvar, since this leads to warning in reshape
	} else if ( length(byvar[is.na(byvar)]) > 0 ) {
		stop("<NA> values in byvar")
	} else {
		## Compute frequency and proportion
		freq <- as.data.table(table(x, byvar, exclude="useNA"))
		perc <- as.data.table(prop.table(table(x, byvar, exclude="useNA"), 2))
		## Rename columns so we can merge freq and perc
		setnames(freq, old=c("x","N"), new=c("CAT","freq"))
		setnames(perc, old=c("x","N"), new=c("CAT","prop"))
		## Merge freq and perc on CAT and byvar (changed from << on=.(CAT, byvar) >>)
        y <- freq[perc, on=c("CAT", "byvar")]
		## Convert proportion to percentage and drop prop column (forcing use of standard evaluation)
		set(y, j="perc", value = 100 * y[["prop"]])
		set(y, j="prop", value = NULL)
		## Restructure to have columns for each byvar level
		yt <- reshape(y, v.names=c("freq","perc"), timevar="byvar", idvar="CAT", direction="wide", sep="_")
		return(yt)
	}
}

