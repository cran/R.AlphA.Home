#' @title allow organized tracking of R code execution time
#' @description The `timer` function allows you to append timeStamps to a data.table,
#' and include additional metadata provided as arguments.
#' The last call calculates time differences between timeStamps.
#'
#' @param timer_table A data.table containing the timer log to continue from.
#' Defaults to an empty `data.table().
#' @param end A logical, inidicating the end of the timer, defaulted to FALSE.
#' 'timer()' calls must be placed at the beginning of each part :
#' therefore, this 'closing' step is necessary to compute time for the last part.
#' Time differences between timeStamps are calculated only when closing the timer.
#' @param ... Additional specifications. Use named arguments to provide documentation
#' on the code parts you are timing : naming the current step, the version of
#' the code you are trying, or any other useful specification
#'
#' @return A `data.table` containing the original data, plus one new timeStamp,
#' and optionally computed time differences :
#'   \itemize{
#'     \item `timeStamp`: The current timeStamp (`POSIXct`).
#'     \item `timeStamp_num`: timeStamp converted to numeric, useful for intermediary calculations.
#'     \item `dt_num`: The time difference in seconds between consecutive rows as a numeric value.
#'     \item `dt_text`: The formatted time difference in seconds with milliseconds as a character string.
#'     \item Additional columns for any information provided by the user via `...`. It allows documentation about the current step running, substeps, which version is being tested, ...
#'   }
#' @rawNamespace import(data.table, except =  c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek))
#' @export
#'
#' @examples
#' # compare code speed between using a loop, or the mean() function
#' library(data.table)
#' library(dplyr)
#' tmr <- data.table()  # Initialize timer
#' vec <- rnorm(1e6)    # Example vector
#'
#' tmr <- timer(tmr, method = "loop")   # timeStamp : 1st step =================
#' total <- 0
#' for (i in seq_along(vec)) total <- total + vec[i]
#' mean_loop <- total / length(vec)
#'
#' tmr <- timer(tmr, method = "mean()") # timeStamp : 1st step =================
#' mean_func <- mean(vec)
#'
#' tmr <- timer(tmr, end = TRUE)        # timeStamp : close timer ==============
#'
#' t_step1 <- tmr[method == "loop"]$dt_num
#' t_step2 <- tmr[method == "mean()"]$dt_num
#' diff_pc <- (t_step2/t_step1 - 1) * 100
#' diff_txt <- format(diff_pc, nsmall = 0, digits = 1)
#'
#' # view speed difference
#' print(tmr %>% select(-matches("_num$")))
#' paste0("speed difference : ", diff_txt, "%")
#'
timer <- function(timer_table = data.table(), end = FALSE, ...) {
	formatTime <- function(time_numeric) {
		ifelse(
			is.na(time_numeric)
			, NA
			, sprintf(
				'%0d.%03d'
				, trunc(time_numeric)
				, trunc((time_numeric %% 1) * 1000)
			)
		)
	} # a simple function to format times in readable text
	time_inter <- data.table(timeStamp = Sys.time())
	extra_args <- list(...)
	if (length(extra_args) > 0) {
		for (name in names(extra_args)) {
			time_inter[, (name) := extra_args[[name]]]
		} # for each additional arg, add it to the table
	} # if additional args provided, add corresponding columns
	{
		timer_table <- rbind(
			fill = TRUE
			, timer_table
			, time_inter
		)
	} # Add a new line of timeStamp to the timer table
	if (end == TRUE && nrow(timer_table) > 1) {
		timer_table[ , timeStamp_num   := as.numeric(timeStamp)]
		timer_table[ , dt_num          := c(diff(timeStamp_num), 0)]
		timer_table[ , dt_text         := formatTime(dt_num)]
	} # Compute time differences if `end = TRUE`
	return(timer_table)
}






