#' @title Conditionally Print an Object
#'
#' @description This function prints `x` if `show` is `TRUE`;
#' otherwise, it returns `x` unchanged.
#' Its main usage is to "close" dplyr-like treatment chains (using %>% or |>).
#' This creates an extremely handy way to process accurate line-by-line debugging,
#' printing results when necessary and removing the print option easily without
#' having to rewrite everything or take care about the last element in the chain.
#'
#' This saves much code writing and debugging time.
#'
#' It is also useful to design functions so that users can easily stop elements
#' from being printed
#'
#' Given the purpose of this function, it is much more convenient to use 1 and 0
#' for the 'show' argument than TRUE or FALSE, as this can be switched easily
#' in the editor.
#'
#' @param x Any object.
#' @param show A logical value indicating whether to print `x` (default: `FALSE`).
#' @param ... Additional arguments passed to `print()`.
#'
#' @return The object `x`, printed if `show` is `TRUE`.
#' @export
#' @examples
#' # Basic usage
#' printif(42, show = TRUE)
#' printif(42, show = FALSE)
#'
#' # Using numeric shortcuts
#' printif("Hello", 1)
#' printif("Hello", 0)
#'
#' # Most useful usage : in a pipeline (requires dplyr)
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   library(dplyr)
#'   mtcars %>%
#'     filter(mpg > 20) %>%
#'     summarise(mean_hp = mean(hp)) %>%
#'     printif(1)
#' }
printif <- function(x, show = FALSE, ...) {
  if (missing(x)) {
    stop("printif() must be used within a pipeline or with an explicit object.", call. = FALSE)
  }
  if (show) {
    print(x, ...)
  } else {
    x
  }
}
