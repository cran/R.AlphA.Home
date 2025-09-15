#' @title Function to Import and Concatenate Multiple Data Files
#' @description Imports multiple files into a list, concatenates them into a single
#' table, and adds an `fName` variable. The function automatically handles type
#' harmonization when different file types are mixed and supports various formats
#' including CSV, Excel, RDS, Parquet, Feather, and QS files.
#'
#' The files can be selected either by giving a file list (character vector), or
#' by specifying a pattern. The function also supports column renaming and file
#' exclusion patterns. When type conflicts are detected across files, the function
#' automatically harmonizes column types using a priority system (character > numeric > Date > integer).
#'
#' @param path Character. Path to the directory, passed to `list.files`. Default is current directory (".").
#' @param pattern Character. Pattern to match file names, passed to `list.files`. Default is empty string (all files).
#' @param ignore.case Logical. If `TRUE`, ignores case when matching file names.
#' Passed to `list.files`. Default behavior is case-sensitive (`FALSE`).
#' @param importFunction Function. A custom function for importing files. If not set, the
#' function selects an import method based on the file extension.
#' @param fill Logical. Passed to `rbind` to allow filling missing columns with NA values. Default is `FALSE`.
#' @param fileList Character vector. A vector of file names to import
#' (used instead of `pattern`). Can contain absolute or relative paths.
#' @param renameTable Data.frame. A data.frame with 2 columns (oldName/newName). importAll
#' will rename the columns of each file following this table. Default is empty data.frame.
#' @param excludePattern Character. Pattern to exclude files from import, applied after initial file selection.
#' Default is `NULL` (no exclusion).
#'
#' @return A data.table containing the concatenated table with the fName column indicating the source file for each row.
#' All imported data is converted to data.table format with automatic type harmonization when necessary.
#'
#' @examples
#' # Directory containing test files
#' test_path <- tempdir()
#'
#' # Create test files
#' write.csv(data.frame(a = 1:3, b = 4:6), file.path(test_path, "file1.csv"))
#' write.csv(data.frame(a = 7:9, b = 10:12), file.path(test_path, "file2.csv"))
#' write.csv(data.frame(a = 3:5, b = 8:10), file.path(test_path, "file3.csv"))
#' saveRDS(data.frame(a = 1:5, b = 6:10), file.path(test_path, "file1.rds"))
#' saveRDS(data.frame(a = 11:15, b = 16:20), file.path(test_path, "file2.rds"))
#'
#' # Example 1: Import all csv files
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
#' @importFrom openxlsx read.xlsx
#' @importFrom data.table fread setnames as.data.table
#' @importFrom arrow read_parquet read_feather
#' @importFrom dplyr mutate filter
#' @importFrom stringr str_detect str_extract
#' @importFrom tibble tribble
#' @importFrom qs qread
#' @export
importAll <- function(
		path = "."
		, pattern = ""
		, ignore.case = FALSE
		, importFunction = NULL
		, fill = FALSE
		, fileList = NULL
		, renameTable = data.frame(oldName = character(), newName = character())
		, excludePattern = NULL
){

	is_absolute_path <- function(path) {
		if (length(path) == 0) return(FALSE)
		# R.utils function
		if (requireNamespace("R.utils", quietly = TRUE)) {
			return(R.utils::isAbsolutePath(path))
		}
		# If R.utils not available : starting with / or ~ (Unix/Mac),
		# "letter:" (Windows), or \\ (UNC)
		grepl("^(/|~|[A-Za-z]:|\\\\)", path)
	} # check if a path is absolute
	{
		if (is.null(fileList)) {
			# with a pattern
			fullPaths_vec <- list.files(
				path = path,
				pattern = pattern,
				ignore.case = ignore.case,
				full.names = TRUE
			)
			filePaths <- data.table(fulPath = fullPaths_vec) %>%
				mutate(locPath = fulPath %>% basename) %>%
				as.data.table
		} else {
			filePaths <- data.table(locPath = fileList) %>%
				mutate(
					fulPath = ifelse(
						sapply(locPath, is_absolute_path),
						locPath,  # Absolute path - we keep it as is
						file.path(path, locPath)  # Relative path - we combine it with 'path'
					)
				) %>%
				as.data.table
		} # list file paths : either with pattern, or with fileList => filePaths
		if(!is.null(excludePattern)){
			filePaths <- filePaths %>%
				mutate(toExclude = str_detect(locPath, excludePattern)) %>%
				filter(!toExclude)
		} # exclude files matching a pattern
		files_exist <- file.exists(filePaths$fulPath)
		if (!all(files_exist)) {
			missing_files <- filePaths$fulPath[!files_exist]
			warning(paste("The following files do not exist and will be ignored:",
						  paste(missing_files, collapse = ", ")))
			filePaths <- filePaths[files_exist, ]
		} # check files existence
		if (nrow(filePaths) == 0) {
			stop("No files found or all specified files are missing")
		}
	} # get paths either with pattern, or with fileList --> filePaths
	{
		# choosing import function depending on extensions
		if (is.null(importFunction)) {
			setDT(filePaths)
			filePaths[, ext := gsub(".*\\.", "", locPath)]
			importFunsList <- tribble(
				~ext       , ~fun
				, "xlsx"   , function(x) as.data.table(openxlsx::read.xlsx(x))
				, "csv"    , fread
				, "rds"    , readRDS
				, "parquet", read_parquet
				, "feather", read_feather
				, "qs", qs::qread
			) %>%
				as.data.table

			filePaths <- merge(filePaths, importFunsList, by = "ext", all.x = TRUE)

			# unsupported extensions
			if (any(is.na(filePaths$fun))) {
				unsupported_ext <- unique(filePaths[is.na(fun), ext])
				stop(paste(
					"following extension(s) are not supported by importAll:"
					, paste(unsupported_ext, collapse = ", ")
					,"\nSupported extensions: xlsx, csv, rds"
				))
			}
		} else {
			filePaths[, cst := TRUE]
			importFunsList <- tribble(
				~cst     , ~fun
				, TRUE   , importFunction
			) %>%
				as.data.table

			filePaths <- merge(filePaths, importFunsList, by = "cst")[, cst := NULL]
		}
	} # add the import function --> filePaths$fun
	{
		multipleFileTypes <- length(unique(filePaths$fun)) > 1
		warningMessage <- paste(
			sep = "\n"
			, "Different file types detected"
			, "columns will be automatically harmonized"

		)
		if (multipleFileTypes) {
			warning(warningMessage)
			harmonize_types <- TRUE
		} else {
			harmonize_types <- FALSE
		}
	} # warning when multiple file types
	{
		importsList <- mapply(
			FUN = function(ful_path, loc_path, importFunction){
				import <- importFunction(ful_path) %>% as.data.table
				import[, fName := loc_path]

				setnames(
					import
					, renameTable$oldName
					, renameTable$newName
					, skip_absent = TRUE
				)

				# Harmonization of types if necessary
				if (harmonize_types) {
					import <- import[, lapply(.SD, function(x) {
						if (is.factor(x)) as.character(x)
						else if (is.logical(x) && all(is.na(x))) as.character(x)
						else x
					})]
					import[, fName := loc_path]  # Restore fName apfter lapply
				}

				return(import)
			}
			, ful_path = filePaths$fulPath
			, loc_path = filePaths$locPath
			, importFunction = filePaths$fun
			, SIMPLIFY = FALSE
		)
	} # import all files --> importsList
	{
		concatenation <- do.call(
			function(...) rbind(..., fill = fill)
			, importsList
		)
		return(concatenation)
	} # concatenate all imports --> concatenation
} # import and concatenate multiple files
