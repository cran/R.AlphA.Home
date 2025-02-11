#' @title Adjust the Brightness of a Hex Color
#' @description Modifies the brightness of a color by multiplying its RGB
#' components by a specified factor.
#'
#' Mostly for internal usage inside lum_0_100 function.
#'
#' @param hexCol Character. The color to adjust, specified in
#' hexadecimal format (e.g., "#FF5733").
#' @param rgbFact Numeric. The luminosity factor :
#' - use a factor between 0 and 1 to decrease luminosity
#' - use a factor >1 to increase it
#' The final Brightness value will be maintained between 0 and 1.
#'
#' @return A modified hex color in hexadecimal format.
#' @importFrom grDevices rgb
#' @importFrom grDevices col2rgb
#' @export
#'
#' @examples
#' # Example 1: Lightening a color
#' ret_lum("#FF5733", 1.5)  # Returns a lighter version of the input color
#'
#' # Example 2: Darkening a color
#' ret_lum("#FF5733", 0.7)  # Returns a darker version of the input color
#'
ret_lum <- function(hexCol, rgbFact) {
	# Convert hexCol to RGB (normalized to [0, 1])
	back2rgb <- grDevices::col2rgb(hexCol) / 255

	# Apply the factor and ensure values stay in [0, 1]
	retLum <- pmax(0, pmin(back2rgb * rgbFact, 1))

	# Convert back to hex format
	back2Hex <- grDevices::rgb(retLum[1], retLum[2], retLum[3])
	return(back2Hex)
}
