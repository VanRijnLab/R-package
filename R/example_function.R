#' Example function that prints Hello world!
#'
#' @param x Integer (1 = Hello World message)
#'
#' @return prints a 'Hello world!' string
#' @export
#'
#' @examples
#' x <- 1
#' example_function(x)
#'
#' y <- 0
#' example_function(y)
example_function <- function(x) {
  if (x == 1) {
    print("Hello world!")
  }
}
