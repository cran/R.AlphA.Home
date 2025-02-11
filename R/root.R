#' @title Get Root Directory of Current Source File
#' @description Returns the directory path where the current source code file
#' is located.
#'
#' It is especially useful when the same source code is used by multiple users,
#' each using his own environment, with different file paths.
#'
#' the aim is to avoid writing full paths in raw text inside source codes.
#'
#' @return A character string representing the absolute path of the directory
#' containing the current source file.
#'
#' @export
#'
root <- function(){
	sourceLoc <- rstudioapi::getSourceEditorContext()$path
	rootDirname <- dirname(sourceLoc)
	return(rootDirname)
}
