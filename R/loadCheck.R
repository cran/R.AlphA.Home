#' @title Load and Install Package if Necessary
#' @description This function checks if a specified package is available in the
#' current R environment. If the package is not installed, it automatically
#' installs it with dependencies and then loads it. #' The function suppresses
#' startup messages to provide a clean loading experience.
#' @param package_names A character vector specifying the name(s) of the
#' package(s) to install (if necessary), and load.
#' @return No return value.
#' @examples
#' # Load a commonly used package
#' loadCheck("dplyr")
#'
#' # Load a package that might not be installed
#' loadCheck("ggplot2")
#' @importFrom utils install.packages
#' @export
#'
loadCheck <- function(package_names) {
	invisible(vapply(package_names,function(pkg) {
			if (!requireNamespace(pkg, quietly = TRUE)) {
				message("Install required for package: ", pkg)
				install.packages(pkg, dependencies = TRUE, quiet = TRUE)
			}
			suppressPackageStartupMessages(
				library(pkg, character.only = TRUE, quietly = TRUE)
			)
			TRUE
		},
		logical(1L) # type attendu pour vapply
	))
}

