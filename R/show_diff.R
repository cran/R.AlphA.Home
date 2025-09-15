#' Compare two texts or files with diffr
#'
#' This function compares two inputs (files or text strings) and displays
#' the differences using the diffr package with syntax highlighting.
#'
#' @param input1 A character string. Either a file path or text content to compare.
#' @param input2 A character string. Either a file path or text content to compare.
#'
#' @return A diffr object containing the visual comparison of the two inputs.
#'
#' @examples
#' # Compare two text strings
#' show_diff("Hello\nWorld", "Hello\nR World")
#'
#' # Compare two files
#' \dontrun{
#' show_diff("file1.txt", "file2.txt")
#' }
#'
#' # Mix file and text
#' \dontrun{
#' show_diff("file.txt", "New content\nWith changes")
#' }
#'
#' @import diffr
#' @export
show_diff <- function(input1, input2) {
	get_content <- function(input) {
		if (is.null(input) || length(input) == 0 || is.na(input) || nchar(input) == 0) {
			return(character(0))
		} else if (file.exists(input)) {
			return(readLines(input, warn = FALSE))
		} else {
			return(strsplit(input, "\n", fixed = TRUE)[[1]])
		}
	} # extrait le contenu d'une chaine ou d'un fichier
	{
		content1 <- get_content(input1)
		content2 <- get_content(input2)
		content1_clean <- trimws(content1)
		content2_clean <- trimws(content2)
	} # Supprimer les espaces en début/fin de ligne
	{
		temp1 <- tempfile(fileext = ".txt")
		temp2 <- tempfile(fileext = ".txt")
		writeLines(content1_clean, temp1)
		writeLines(content2_clean, temp2)
	} # Créer des fichiers temporaires nettoyés
	{
		result <- diffr::diffr(
			temp1,
			temp2,
			contextSize = 2,
			minJumpSize = 5,
			wordWrap = TRUE,
			before = "Original",
			after = "Modified"
		)
	} # Utiliser diffr sur les fichiers nettoyés
	unlink(c(temp1, temp2))
	return(result)
}
