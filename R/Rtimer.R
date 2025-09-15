#' Timer Class for Performance Measurement
#'
#' An R6 class for measuring and tracking execution time of code segments.
#' Provides functionality to add timing checkpoints, calculate time differences,
#' and generate summary reports of performance metrics.
#'
#' @section Public Methods:
#' \describe{
#'   \item{\code{new()}}{Initialize a new Timer instance.}
#'   \item{\code{add(...)}}{Add a timing checkpoint with optional labels.}
#'   \item{\code{get(fill = TRUE)}}{Generate timing results as data.table.}
#' }
#'
#' @examples
#' \dontrun{
#' tmr <- Rtimer$new()
#' tmr$add("start")
#' # some code
#' tmr$add("end")
#' result <- tmr$get()
#' print(result)
#' }
#'
#' @import R6
#' @importFrom data.table data.table rbindlist
#' @export
Rtimer <- R6Class(
	"Timer",
	private = list(
		timer_list = list()
	),
	public = list(
		#' @description Create a new Timer instance
		#' @return A Rtimer object
		initialize = function() {
			private$timer_list <- list()
		},
		#' @description Add a timestamp
		#' @param ... Optional named labels attached to the timestamp.
		#' @return The object itself (invisible) for chaining
		add = function(...) {
			extra_args <- list(...)
			if (length(extra_args) > 0) {
				noName <- is.null(names(extra_args)) || names(extra_args)[1] == ""
				if (noName) names(extra_args)[1] <- "step"
				new_entry <- c(extra_args, list(currentTime = Sys.time(), ct_proc = proc.time()[3]))
			}
			else new_entry <- list(currentTime = Sys.time(), ct_proc = proc.time()[3])
			private$timer_list <- append(private$timer_list, list(new_entry))
			invisible(self)
		},
		#' @description Return the collected timings as a \code{data.table}
		#' @param fill Logical; if \code{TRUE}, fill missing columns when combining entries
		#' @return A \code{data.table} containing timestamps and time differences
		get = function(fill = TRUE) {
			if (length(private$timer_list) == 0) return(data.table())
			timer_table <- rbindlist(private$timer_list, fill = fill)
			timer_table[, ct_num := as.numeric(currentTime)]
			timer_table[, dt_num := c(diff(ct_num), 0)]
			timer_table[, dt_proc := c(diff(ct_proc), 0)]
			timer_table[, tot_num := cumsum(dt_num)]
			timer_table[, diffTime := round(dt_num, 3)]
			timer_table[, totalTime := round(tot_num, 3)]
			return(timer_table)
		}
	)
)
