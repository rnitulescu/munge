# Summarize input data's selected columns using selected functions
#
# Returns a data table object with the summarized data in a standardized format.
# @param x data.table object containing columns to be summarized.
# @param byvar character variable representing name of variable to group statistics by (default: NULL).
# @param cols character vector representing names of variables to compute statistics for.
# @param FUN function to use for computing statistics.
# @param FUN.name character variable representing name of function to use for computing statistics
#                 (this function will only be called by another in a well controlled manner,
#                 so this will not be an issue).
# @keywords fn.summary
fn.summary <- function(x, byvar=NULL, cols, FUN, FUN.name)
{
	## Don't run if there are multiple byvars. Implementaton only works with 1 byvar at the moment.
	if ( length(byvar) > 1 ) {
		stop("Invalid arguments")
	} else if ( !(FUN.name %in% c("tab1way","tab2way")) ) {
	## Case 1: Numeric variables (i.e., not for 1-way or 2-way tables)
		if ( is.null(byvar) ) {
			## Summarize data
			y <- x[, lapply(.SD, FUN), .SDcols=cols]
			## Transpose results
			yt <- transpose(y)
			## Rename results column
			setnames(yt, FUN.name)
			## Merge in the names of the variables summarized
			yt <- cbind(data.table(REF=colnames(y)), yt)
		} else {
			## Summarize data
			y <- x[, lapply(.SD, FUN), by=byvar, .SDcols=cols]
			## Transpose results
			yt <- transpose(y)[-1]
			## Format header/column names
			header <- y[[byvar]]
			header <- paste(FUN.name, header, sep="_")
			setnames(yt, header)
			## Merge in the names of the variables summarized and the byvar
			yt <- cbind(data.table(REF=colnames(y)[-1]), byvar, yt)
		}
	} else {
	## Case 2: Deal with the case of categorical (i.e., factor) variables
		if ( is.null(byvar) ) {
		## One-way table
			## Summarize data
			y <- lapply(x[, cols, with=FALSE], FUN)
			## Merge in name of variable
			y <- Map(function(y, i) cbind(VAR=i, y), y, names(y))
		} else {
		## Two-way table
			## Summarize data
			y <- Map(function(z) FUN(z, x[[byvar]]), x[, cols, with=FALSE])
			## Merge in name of variable and byvar
			y <- Map(function(y, i) cbind(VAR=i, byvar, y), y, names(y))
		}
		## Stack tables
		yt <- do.call("rbind", y)
		## Create reference column, drop unneeded columns, and reorder columns
		set(yt, j="REF", value = paste(yt[["VAR"]], yt[["CAT"]], sep="_"))
		set(yt, j=c("VAR","CAT"), value = NULL)
		setcolorder(yt, c("REF", setdiff(names(yt), "REF")))
	}
	return(yt)
}

