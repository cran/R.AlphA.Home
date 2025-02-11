#' @title Generate Random Dates, with a similar usage as the r* functions
#' @description Generates a vector of random dates within a specified range.
#' This function tries to replicate the usage of the r* functions from stats
#' package, such as runif(), rpois(), ...
#'
#' @param x Integer. Length of the output vector
#' (number of random dates to generate).
#' @param min Date. Optional. The minimum date for the range.
#' Defaults to the 1st of January of the current year.
#' @param max Date. Optional. The maximum date for the range.
#' Defaults to the 31st of December of the current year.
#' @param sort Logical. Should the dates be sorted in ascending order?
#' Default is `FALSE`.
#' @param include_hours Logical. Should the generated dates include time?
#'  Default is `FALSE` (dates only). this will slow down the function
#'
#' @return A vector of random dates of length `x`.
#' @importFrom lubridate decimal_date date_decimal
#' @importFrom stats runif
#' @export
#'
#' @examples
#' # Generate 5 random dates between two specific dates, sorted
#' rdate(5, min = as.Date("2020-01-01"), max = as.Date("2020-12-31"), sort = TRUE)
#'
#' # Generate 7 random datetime values (with hours)
#' rdate(7, include_hours = TRUE)
#'
rdate <- function(
		x
		, min = paste0(format(Sys.Date(), '%Y'), '-01-01')
		, max = paste0(format(Sys.Date(), '%Y'), '-12-31')
		, sort = FALSE
		, include_hours = FALSE
){
	max <- as.Date(max)
	min <- as.Date(min)
	if (!include_hours) {
		daysDiff <- max-min
		dates <- min + runif(x, 0, daysDiff + 1) %>% floor
	}
	if (include_hours) {
		dec_min <- lubridate::decimal_date(as.Date(min))
		dec_max <- lubridate::decimal_date(as.Date(max))
		dec_date <- stats::runif(x, min = dec_min, max = dec_max)
		dates <- lubridate::date_decimal(dec_date)
	}

	dates <- if (sort) sort(dates) else dates

	return(dates)
}


