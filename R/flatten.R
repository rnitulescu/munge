# Get rid of "ordered" class
#
# See example for the one use-case I designed this function for.
# Basically, without this, "munge" will crash if a factor is also ordered.
# @param x vector of character values (as returned by class(), say).
# @keywords flatten
# @examples
# x <- c(1,2,3)
# y <- factor(x, ordered=TRUE)
# z <- class(y)
# print(z)
# print(flatten(z))
flatten <- function(x) {
    x[ x != "ordered" ]
}

