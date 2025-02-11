#' @title Add Variables to ease data usage in a Pivot Table
#' @description Adds dummy columns to reach the number specified by the user.
#' this is mostly useful to ensure straightforward and easy data updating when
#' using pivot tables in Excel. It allows replacement of the previous data sheet
#' by the new one, without having to take care about the number of columns, which
#' will always be the same.
#'
#' @param data The data frame to which dummy columns will be added.
#' @param nCols the total number of columns required : default is 100
#' @param colPrefix A string used as the prefix for the names of dummy columns.
#'
#' @return A data frame with the specified total number of columns.
#' @importFrom stringr str_replace
#' @export
#'
#' @examples
#' table <- data.frame(a = 1:5, b = letters[1:5])
#' extraTable <- cols_pad(table, nCols = 6, colPrefix = "extra_")
#' print(extraTable)
#'
#'
cols_pad <- function(data, nCols = 100, colPrefix = "x_"){
	nColsData <- ncol(data)
	colsToAdd <- nCols - nColsData
	if(colsToAdd < 0){
		stop("data already has ", nColsData, " cols, >", nCols)
	}
	dummyAdd <- matrix(ncol = colsToAdd, nrow = nrow(data)) %>%
		as.data.frame %>%
		rename_with(~str_replace(.,"V", colPrefix))

	data %>% cbind(dummyAdd)
}
