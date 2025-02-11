#' @title Quick Number Formatting with Custom Defaults
#' @description A wrapper for the `format` function, designed to format numbers
#' with custom defaults for thousands separator, number of significant digits,
#' and scientific notation.
#'
#' @param x Numeric. The input values to format.
#' @param big.mark Character. The separator for thousands
#' (e.g., `" "` for "1 000" or `","` for "1,000"). Default is `" "`.
#' @param digits Integer. The number of significant digits to display.
#' Default is `1`.
#' @param scientific Logical. Should the numbers be displayed in scientific
#' notation? Default is `FALSE`.
#'
#' @return A character vector of formatted numbers.
#' @export
#'
#' @examples
#' # Format with a comma as a thousands separator and 3 significant digits
#' sepThsd(1234567.89, big.mark = ",", digits = 3)
#’
#' # Use scientific notation
#' sepThsd(1234567.89, scientific = TRUE)
#'
sepThsd <- function(x, big.mark = " ", digits = 1, scientific = FALSE){
	formatted <- format(
		x
		, big.mark = big.mark
		, digits = digits
		, scientific = scientific
	)
	return(formatted)
}
