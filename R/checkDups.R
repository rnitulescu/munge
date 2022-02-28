#' Check duplicates
#' 
#' Checks data.frame for rows with duplicated values.
#'
#' \code{checkDups} counts the number of times a group of variables
#' (\code{byvars}) is repeated in a data.frame. If the number is greater than 1,
#' the function returns all rows with such repetitions, ordered by \code{byvars}.
#' This tool facilitates the investigation of duplicates in data.
#'
#' @param x data.frame object containing columns to be checked for duplicates.
#' @param byvars character vector representing names of variables to check for duplicates.
#'
#' @keywords checkDups
#' @importFrom stats aggregate as.formula
#' @export
#' @examples
#' data(mtcars)
#' checkDups(mtcars, byvars=c("cyl","vs","am","gear","carb"))
checkDups <- function(x, byvars) {
	## Collapse data (i.e., count obs)
	x[["nrec"]] <- row.names(x)
	col <- aggregate(as.formula(paste0("nrec", "~", paste(byvars, collapse="+"))), FUN=length, data=x)
	## Retrieve cases where there are 2 or more records per byvars cluster
	col.sub <- col[col[["nrec"]] > 1, ]
	## Merge to main table and return
	x[["nrec"]] <- NULL
	fnl <- merge(x=col.sub[byvars], y=x, by=byvars, all.x=TRUE) ## Left join
	return(fnl)
}

