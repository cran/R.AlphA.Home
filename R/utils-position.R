#' Internal position utilities for R.AlphA
#'
#' These functions handle conversions between different position formats
#' used in RStudio API for cursor and range management.
#'
#' @name position-utils
#' @keywords internal



#' Column factor constant for position calculations
#' @keywords internal
#' @noRd
.ralpha_col_factor <- function() {
	1E-3
}

#' Get current cursor position
#' @param context Source editor context from rstudioapi
#' @return Document range object
#' @keywords internal
#' @noRd
.ralpha_get_position <- function(context = NULL) {
	if (is.null(context)) {
		context <- rstudioapi::getSourceEditorContext()
	}
	context %>%
		rstudioapi::primary_selection() %>%
		getElement("range")
}

#' Convert position number to document position
#' @param pos_num Numeric position value
#' @param col_fact Column factor (default: 1E-3)
#' @return Document position object
#' @keywords internal
#' @noRd
.ralpha_pos_num_to_doc_pos <- function(pos_num, col_fact = .ralpha_col_factor()) {
	row <- floor(pos_num)
	col <- ((pos_num - row) / col_fact) %>% round(8)
	rstudioapi::document_position(row, col)
}

#' Convert document position to position number
#' @param doc_pos Document position object (row, col)
#' @param col_fact Column factor (default: 1E-3)
#' @return Numeric position value
#' @keywords internal
#' @noRd
.ralpha_doc_pos_to_pos_num <- function(doc_pos, col_fact = .ralpha_col_factor()) {
	pos_num <- (doc_pos[1] + doc_pos[2] * col_fact) %>% round(8)
	as.numeric(pos_num)
}

#' Convert document range to position number
#' @param doc_range Document range object with start position
#' @param col_fact Column factor (default: 1E-3)
#' @return Numeric position value
#' @keywords internal
#' @noRd
.ralpha_doc_range_to_pos_num <- function(doc_range, col_fact = .ralpha_col_factor()) {
	pos_num <- (doc_range$start[1] + doc_range$start[2] * col_fact) %>% round(8)
	as.numeric(pos_num)
}

#' Convert position numbers to document range
#' @param pos_num_start Starting position number
#' @param pos_num_end Ending position number (default: same as start)
#' @param col_fact Column factor (default: 1E-3)
#' @return Document range object
#' @keywords internal
#' @noRd
.ralpha_pos_num_to_doc_range <- function(pos_num_start, pos_num_end = pos_num_start,
										 col_fact = .ralpha_col_factor()) {
	# Start position
	start_row <- floor(pos_num_start)
	start_col <- ((pos_num_start - start_row) / col_fact) %>% round(8)

	# End position
	end_row <- floor(pos_num_end)
	end_col <- ((pos_num_end - end_row) / col_fact) %>% round(8)

	rstudioapi::document_range(
		start = rstudioapi::document_position(start_row, start_col),
		end = rstudioapi::document_position(end_row, end_col)
	)
}

#' Get end of line position for a position number
#' @param pos_num Numeric position value
#' @param col_fact Column factor (default: 1E-3)
#' @return Numeric position at end of line
#' @keywords internal
#' @noRd
.ralpha_end_line <- function(pos_num, col_fact = .ralpha_col_factor()) {
	ceiling(pos_num) - col_fact
}

#' Execute RStudio command with optional wait time
#' @param command Command to execute
#' @param wait_time Wait time in seconds (from option or default)
#' @keywords internal
#' @noRd
.ralpha_execute_command <- function(command, wait_time = NULL) {
	if (is.null(wait_time)) {
		wait_time <- getOption("fab_wait", default = 0)
	}
	rstudioapi::executeCommand(command)
	if (wait_time > 0) {
		Sys.sleep(wait_time)
	}
}

#' Set cursor position with optional wait
#' @param position Document position or position number
#' @param wait_time Wait time in seconds
#' @keywords internal
#' @noRd
.ralpha_set_cursor_position <- function(position, wait_time = NULL) {
	if (is.null(wait_time)) {
		wait_time <- getOption("fab_wait", default = 0)
	}

	# Convert numeric position to document position if needed
	if (is.numeric(position)) {
		position <- .ralpha_pos_num_to_doc_pos(position)
	}

	rstudioapi::setCursorPosition(position)
	if (wait_time > 0) {
		Sys.sleep(wait_time)
	}
}
