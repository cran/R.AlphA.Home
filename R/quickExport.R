#' @title Quick Export of Data to Excel with Column Padding
#' @description Exports a data frame to an Excel file with optional column padding to ensure a consistent number of columns. This function combines data export functionality with column padding, making it particularly useful for creating Excel files that maintain the same structure across different datasets, especially when used with pivot tables.
#' @param data A data frame to be exported to Excel.
#' @param sheetName A character string specifying the name of the Excel sheet. Default is "data_".
#' @param saveDir A character string specifying the directory path where the file will be saved. Default uses root() function.
#' @param saveName A character string specifying the filename for the Excel file. Default is "tmp_export.xlsx".
#' @param nCols An integer specifying the total number of columns required after padding. Default is 100.
#' @param colPrefix A character string used as the prefix for the names of dummy columns added during padding. Default is "x_".
#' @param overwrite A logical value indicating whether to overwrite existing files. Default is TRUE.
#' @return No explicit return value. The function writes an Excel file to the specified location and prints a message with the file path.
#' @examples
#' \dontrun{
#' # Basic usage with default parameters
#' df <- data.frame(name = c("Alice", "Bob"), age = c(25, 30))
#' quickExport(df, sheetName = "employees", saveName = "employee_data.xlsx")
#'
#' # Custom column padding and file location
#' sales_data <- data.frame(product = c("A", "B"), sales = c(100, 200))
#' quickExport(sales_data, nCols = 50, colPrefix = "col_",
#'            saveName = "sales_report.xlsx", overwrite = FALSE)
#' }
#' @importFrom openxlsx write.xlsx
#' @export
quickExport <- function(
		data, sheetName = "data_", saveDir = root(), saveName = "tmp_export.xlsx"
		, nCols = 100, colPrefix = "x_", overwrite = TRUE
){
	data_complete <- data %>% cols_pad(nCols, colPrefix)
	savePath <- saveDir %>% file.path(saveName)
	openxlsx::write.xlsx(
		data_complete
		, savePath
		, sheetName = sheetName
		, overwrite = overwrite
	)
	message("file written to : \n", savePath)
}
