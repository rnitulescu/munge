#' Efficient summary statistics
#' 
#' Summarize ("munge") all the data in a data.table.
#'
#' \code{munge} automatically calculates one-way or two-way summary statistics for all
#' columns in a data.table object that are of the following types (it ignores all other types):
#' integer, numeric, double, logical, and factor.
#' The function returns a data.table object with the summarized data in a standardized format.
#' It calls the function \code{fn.summary} for figuring out which statistics to compute
#' and stacks all results into one data.table which the function returns.
#' \code{munge} allows for some extendibility by allowing the user to pass additional functions to
#' the \code{fun.list} argument, or overriding the defaults with different ones.
#'
#' @param x data.table object containing columns to be summarized.
#' @param byvar character variable representing name of variable to group statistics by (default: NULL, for one-way analysis).
#' @param sep character variable representing character to use between appended strings in REF column (default: "_").
#' @param NUMLST character vector of valid class types.
#' @param FACLST character vector of valid class types.
#' @param fun.list list of functions indexed by their names to be used in the summarization.
#'
#' @keywords munge
#' @export
#' @examples
#' data(ChickWeight)
#' munge(as.data.table(ChickWeight[c("weight","Time","Diet")]))
#' munge(as.data.table(ChickWeight)) ## handles ordered factors, too
#' munge(as.data.table(ChickWeight[c("weight","Time","Diet")]), byvar="Diet")
#' munge(as.data.table(ChickWeight), byvar="Diet")
munge <- function(x, byvar=NULL, sep="_",
                  NUMLST=MYNUMLST,
                  FACLST=MYFACLST,
                  fun.list=list(armean=armean, stdev=stdev,
                                q1=q1, q2=q2, q3=q3,
                                safe.min=safe.min, safe.max=safe.max,
                                total=total, na=na, notna=notna))
{
	## Don't run if there are multiple byvars. Implementaton only works with 1 byvar at the moment.
	if ( length(byvar) > 1 ) {
		stop("Invalid arguments")
	} else {
		## First, group variables by class (excluding the byvar)
		nm.lst <- setdiff(names(x), byvar)
		cl.lst <- unlist(lapply(nm.lst, getVarClass, x))
		cont.vars <- nm.lst[cl.lst %in% NUMLST]
		disc.vars <- nm.lst[cl.lst %in% FACLST]
		## Second, compute stats (1-way or 2-way, as the case may be)
		## Case 1: 1-way stats
		if ( is.null(byvar) ) {
			## A) compute 1-way frequency tables, if the list of such variables is non-empty
			if ( length(disc.vars) > 0 ) {
				disc.stat <- fn.summary(x, cols=disc.vars, glue=sep, FUN=tab1way, FUN.name="tab1way")
			}
			## B) compute statistics for continuous variables and merge them together, if the list of such variables is non-empty
			if ( length(cont.vars) > 0 ) {
				cont.stat <- Map(function(y, z) fn.summary(x, cols=cont.vars, glue=sep, FUN=y, FUN.name=z), fun.list, names(fun.list))
			}
			## C) Add a row for the number of obs in table
			tmp <- data.table(REF="NOBS", freq=nrow(x))
		} else {
		## Case 2: 2-way stats
			## A) compute 2-way frequency tables, if the list of such variables is non-empty
			if ( length(disc.vars) > 0 ) {
				disc.stat <- fn.summary(x, byvar=byvar, cols=disc.vars, glue=sep, FUN=tab2way, FUN.name="tab2way")
			}
			## B) compute statistics for continuous variables and merge them together, if the list of such variables is non-empty
			if ( length(cont.vars) > 0 ) {
				cont.stat <- Map(function(y, z) fn.summary(x, byvar=byvar, cols=cont.vars, glue=sep, FUN=y, FUN.name=z), fun.list, names(fun.list))
			}
			## C) Add a row for the number of obs in table
			## Tabulate number of observations per group (as per byvar)
			tmp <- transpose(x[, .N, by=byvar])
			## Define column names based on contens of first row of this table (which contains the names of the byvar groups)
			header <- paste0("freq_", as.matrix(tmp[1]))
			## Set the names of the columns
			setnames(tmp, header)
			## Keep on the second row (i.e., the row containing the counts)
			tmp <- tmp[2]
			## Add the structural columns (REF and byvar)
			tmp <- cbind(REF="NOBS", byvar=byvar, tmp)
			## Change values back to numeric (since they were forced to character when we transposed above)
			tmp <- tmp[, lapply(.SD, as.numeric), by=c("REF","byvar")]
		}
		## Third, merge together all tables in list of continuous variables, if the list of such variables in non-empty
		if ( length(cont.vars) > 0 ) {
			cont.stat.mrg <- Reduce(merge, cont.stat)
			## Change values back to numeric (since they were forced to character when we Mapped the summary functions above)
			if ( is.null(byvar) ) {
				cont.stat.mrg <- cont.stat.mrg[, lapply(.SD, as.numeric), by="REF"]
			} else {
				cont.stat.mrg <- cont.stat.mrg[, lapply(.SD, as.numeric), by=c("REF","byvar")]
			}
		}
		## Finally, return stacked stats table, depending on what was computed
		if ( length(disc.vars) == 0 & length(cont.vars) == 0 ) {
			return(tmp)
		} else if ( length(disc.vars) == 0 ) {
			return( rbindlist(list(tmp, cont.stat.mrg), fill=TRUE) )
		} else if ( length(cont.vars) == 0 ) {
			return( rbindlist(list(tmp, disc.stat), fill=TRUE) )
		} else {
			return( rbindlist(list(tmp, disc.stat, cont.stat.mrg), fill=TRUE) )
		}	
	}
}

