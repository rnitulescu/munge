# Count total NA values in field
#
# Returns the number of NA values in a vector.
# @param x vector of numeric values.
# @keywords na
# @examples
# na(c(1,2,3,NA,NA))
na <- function(x) {
    length(x[is.na(x)])
}

