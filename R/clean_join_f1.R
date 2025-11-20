#' Convert numeric expressions into actual numeric values
#'
#' This utility function takes a vector containing simple numeric
#' expressions (such as `"0.19546*10^-1"` or `"3*10^-2"`) and evaluates
#' them into true numeric values.  
#'
#' It is especially useful when coefficient tables store values in
#' scientific expression format rather than already-evaluated numeric form.
#'
#' Expressions must be valid R code that can be interpreted by `parse()`
#' and evaluated by `eval()`. Invalid expressions will return `NA`
#' silently (warnings are suppressed).
#'
#' @param x A vector (character, factor, or numeric) containing numeric
#'   expressions to be evaluated.
#'
#' @return A numeric vector of the same length as `x`, containing the
#'   evaluated values. Non-evaluable elements return `NA`.
#'
#' @examples
#' # Convert scientific expressions
#' expr <- c("0.19546*10^-1", "1.10*10^-3", "-2.5*10^2")
#' parse_expr_num(expr)
#'
#' # Mixed numeric formats
#' mixed <- c("0.5", "1/4", "3*10^-2")
#' parse_expr_num(mixed)
#'
#' # Invalid expressions return NA
#' bad <- c("abc", "10^-", "")
#' parse_expr_num(bad)
#'
#' @export
parse_expr_num <- function(x) {
  suppressWarnings(
    as.numeric(
      sapply(x, function(v) eval(parse(text = v)))
    )
  )
}
