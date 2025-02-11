#' @title Function to Import and Concatenate Multiple data files
#' @description Imports multiple files into a list, concatenates them into a single
#' table, and adds an `fName` variable.
#'
#' The files can be selected either by giving a file list (character vector), or
#' by specifying a pattern.
#'
#' @param path Path to the directory, passed to `list.files`.
#' @param pattern Pattern to match file names, passed to `list.files`.
#' @param ignore.case Logical. If `TRUE`, ignores case when matching file names.
#' Passed to `list.files`. Default behavior is case-sensitive (`FALSE`)
#' @param importFunction A custom function for importing files. If not set, the
#' function selects an import method based on the file extension.
#' @param fill Logical. Passed to `rbind` to allow filling missing columns.
#' @param fileList A character vector of file names to import
#' (used instead of `pattern`).
#'
#' @return A data frame containing the concatenated table with the fName column
#' @importFrom openxlsx read.xlsx
#' @export
#'
#' @examples
#' # Directory containing test files
#' test_path <- tempdir()
#'
#' # Create test files
#' write.csv( data.frame(a = 1:3, b = 4:6)    , file.path(test_path, "file1.csv"))
#' write.csv( data.frame(a = 7:9, b = 10:12)  , file.path(test_path, "file2.csv"))
#' write.csv( data.frame(a = 3:5, b = 8:10)   , file.path(test_path, "file3.csv"))
#' saveRDS(   data.frame(a = 1:5, b = 6:10)   , file.path(test_path, "file1.rds"))
#' saveRDS(   data.frame(a = 11:15, b = 16:20), file.path(test_path, "file2.rds"))
#'
#' # Example 1 : Import all csv files
#' result <- importAll(path = test_path, pattern = "\\.csv$")
#' print(result)
#'
#' # Example 2: Import only selected files
#' file_list <- c("file1.csv", "file2.csv")
#' result <- importAll(path = test_path, fileList = file_list)
#' print(result)
#'
#' # Example 3: Import all .rds files
#' result <- importAll(path = test_path, pattern = "\\.rds$")
#' print(result)
#'
#' # Example 4: Use a custom import function
#' custom_import <- function(file) {
#'   data <- read.csv(file, stringsAsFactors = FALSE)
#'   return(data)
#' }
#' result <- importAll(path = test_path, pattern = "\\.csv$", importFunction = custom_import)
#' print(result)
#'
importAll <- function(
		path = "."
		, pattern = ""
		, ignore.case = FALSE
		, importFunction = NULL
		, fill = FALSE
		, fileList = NULL
){

	if (missing(fileList)) {
		# with a pattern
		fullPaths <- list.files(
			path = path,
			pattern = pattern,
			ignore.case = ignore.case,
			full.names = TRUE
		)
		filePaths <- data.table(fulPath = fullPaths) %>%
			mutate(locPath = fulPath %>% basename) %>%
			as.data.table
	} else {
		# with a file list
		filePaths <- data.table(fulPath = file.path(path, fileList)) %>%
			mutate(locPath = fulPath %>% basename) %>%
			as.data.table
	} # list file paths : either with pattern, or with fileList

	# choosing import function depending on extensions
	if (missing(importFunction)) {
		filePaths[, ext := gsub(".*\\.", "", locPath)]
		importFunsList <- tribble(
			~ext     , ~fun
			, "xlsx" , function(x) as.data.table(openxlsx::read.xlsx(x))
			, "csv"  , fread
			, "rds"  , readRDS
		) %>%
			as.data.table

		filePaths <- merge(filePaths, importFunsList, by = "ext")
	} else {
		testnames <- names(filePaths)
		filePaths[, cst := TRUE]
		importFunsList <- tribble(
			~cst     , ~fun
			, TRUE      , importFunction
		) %>%
			as.data.table

		filePaths <- merge(filePaths, importFunsList, by = "cst")[, cst := NULL]
	} # choose import function, from extensions, or from a function provided

	if (length(unique(filePaths$fun)) > 1) {
		warning(
			"More than one type of file detected:"
			,"\nCurrently, this has high chances to cause issues with column types."
			,"\nThis topic will be adressed in the upcoming versions of "
			,"this function."
		)
	} # warning when multiple file types
	importsList <- mapply(
		FUN = function(ful_path, loc_path, importFunction){
			import <- importFunction(ful_path) %>% as.data.table
			import[, fName := loc_path]
		}
		, ful_path = filePaths$fulPath
		, loc_path = filePaths$locPath
		, importFunction = filePaths$fun
		, SIMPLIFY = FALSE
	)
	concatenation <- do.call(
		function(...) rbind(..., fill = fill)
		, importsList
	)
}
