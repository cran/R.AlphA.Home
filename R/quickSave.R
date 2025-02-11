#' @title Save File in a Directory storing saves, prefixing it with current date
#' @description Saves a file with current date in its name in a sub directory
#' located in the same directory as the original file.
#' Optionally, a note is added after the file name.
#'
#' @param saveDir Choose the directory used to store saves. Suggested : 'old'
#' @param filePath Optional, if you want to save another file than the current
#' one : full path of the file you want to save.
#' @param saveNote An optional custom note to append to the file name
#' for the save, allowing to keep track of why this save has been done.
#' @param overwrite Logical. Should an existing save with the same name
#' be overwritten? Default is `FALSE`.
#' @param verbose logical. If turned to `TRUE`, the save path is displayed
#'
#' @return the output value of the function used to copy file
#' @import ggplot2
#' @importFrom R.utils copyDirectory
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom rstudioapi getSourceEditorContext
#' @export
#'
quickSave <- function(
		saveDir
		, filePath = NULL
		, saveNote = NULL
		, overwrite = FALSE
		, verbose = FALSE
){
	{
		if (is.null(filePath)) {
			filePath <- rstudioapi::getSourceEditorContext()$path
		} # get file path
		fileName <- filePath %>% basename # local fileName
		fileDirName <- dirname(filePath) # directory name
		saveDirName <- file.path(fileDirName, saveDir)
	} # paths - file to save, and save directory
	if(!dir.exists(saveDirName)) {
		message("creating save directory : ", saveDirName)
		dir.create(saveDirName)
	} # create saveDir if not already existing

	saveName <- paste0(Sys.Date(), " ", fileName)
	if (!is.null(saveNote)) {
		saveName <- paste0(
			tools::file_path_sans_ext(saveName)  # without ext
			, " (", saveNote, ")"                # note in parentheses
			, "."
			, tools::file_ext(saveName)          # extension only
		)
	} # add an optional note after file name
	savePath <- file.path(saveDirName, saveName)

	if(verbose == TRUE) message("saving under : ", savePath)
	if(file.exists(savePath)){
		if(overwrite) message(
			"the file : \n'"
			, savePath
			, "'\n","already exists and will be overwritten."
		)
		if(!overwrite) stop(
			"the file : \n'"
			, savePath
			, "'\n","already exists : please remove it first, or specify "
			, "'overwrite = TRUE' to overwrite it."
		)
	} # if a file already exists with this name : warning/error

	isDir <- dir.exists(filePath) # if path to save is a directory
	if(isDir){
		message("(copying a directory)")
		R.utils::copyDirectory(filePath, savePath, overwrite = overwrite)
	} else {
		file.copy(from = filePath, to = savePath, overwrite = overwrite)
	} #
}
