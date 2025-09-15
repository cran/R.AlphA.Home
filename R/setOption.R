#' @title Set Global Option from Named List Element
#'
#' @description This function takes an element from a named list as an argument,
#' and sets a global option based on the list's name.
#'
#' Where :	optionName$element == "value",
#' calling	setOption(optionName$element)
#' triggers options(optionName = "value")
#'
#' @param listElement An element from a named list, specified as `myList$element`.
#'
#' @details
#' The function automatically extracts the list name from the argument.
#' The option is then dynamically set using `options(list_name = element)`.
#'
#' @return The function does not return anything but sets an option that can be retrieved
#' using `getOption(list_name)`.
#'
#' @examples
#' # Create a temporary list for demonstration
#' modelOption <- list(model1 = "model_1", model2 = "model_2", model3 = "model_3")
#'
#' # Set the option
#' setOption(modelOption$model1)
#'
#' # Retrieve the option
#' getOption("modelOption")  # Returns "model_1"
#'
#' # Clean up
#' options(modelOption = NULL)
#'
#' @export
setOption <- function(listElement) {
	listElement_call <- deparse(substitute(listElement))

	# Extract the part before $ : interpreted as the option name
	listNamePattern <- regexpr("^[^$]+", listElement_call)
	list_name <- regmatches(listElement_call, listNamePattern)

	# Create named list for options()
	option_value <- list(listElement)
	names(option_value) <- list_name

	# Apply the options
	do.call(options, option_value)
}
