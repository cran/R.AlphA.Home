#' @title Set Shiny Background and Sidebar Colors to a Chosen Shade of Grey
#' @description Adjust the background color of a Shiny app's main body and sidebar
#' based on a specified luminosity level.
#'
#' The purpose is the same as lum_0_100() function, avoiding problems with high
#' contrast between with graphic windows and dark themes.
#'
#' @param lum Numeric. Luminosity level, ranging from 0 (black) to 100 (white).
#'
#' @return The HTML tags for setting the background and sidebar colors.
#' @importFrom shinyWidgets setBackgroundColor
#' @importFrom shiny tags HTML
#' @rawNamespace import(dplyr, except = c(first, last, between))
#' @export
#'

shiny_lum_0_100 <- function(lum) {
	# Helper function: Generate CSS for the sidebar
	SBBGColor_lum_0_100 <- function(lum) {
		lum_pc <- lum / 100
		hex_lum <- rgb(lum_pc, lum_pc, lum_pc)
		HTMLText <- paste0(
			'#sidebar {background-color: ', hex_lum, ';}'
		)
		shiny::tags$head(shiny::tags$style(shiny::HTML(HTMLText)))
	}

	# Normalize luminosity for the body background
	lum_pc <- lum / 100

	# Create a combined list of tags for sidebar and body background
	return(list(
		SBBGColor_lum_0_100(min(lum * 1.2, 100)),  # Adjust sidebar luminosity and ensure max of 100
		shinyWidgets::setBackgroundColor(rgb(lum_pc, lum_pc, lum_pc))
	))
}

