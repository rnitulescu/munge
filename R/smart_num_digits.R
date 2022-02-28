#' Rounding digits
#' 
#' Guesses the number of digits to round to based on the number.
#'
#' \code{smart_num_digits} is an attempt at guessing a reasonable number
#' of digits to round to, based on the magnitude of the number in question.
#' The function returns a non-negative integer representing the number of digits
#' one might wish to use in rounding. It is best used within other functions.
#'
#' @param x numeric value.
#'
#' @keywords smart_num_digits
#' @export
#' @examples
#' smart_num_digits(100)
#' smart_num_digits(6.2)
#' smart_num_digits(0.78)
#' smart_num_digits(0.0125)
smart_num_digits <- function(x) {
    x <- abs(x)
    digits <- ifelse(x == 0, 0,
                ifelse(x < 1, ceiling(abs(log(x, 10))),
                  ifelse(x < 10, 2, 
                    ifelse(x < 100, 1, 0))))
    return(digits)
}

