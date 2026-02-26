#' Convert a string into a formula and/or change the associated environment
#'
#' This function is used to transform a name like `'var'` into a formula `~var`.
#' @param x Either a name (character string) or a formula
#' @param env The environment to associate with the formula
#'
#' @returns A formula
#' @export
#'
#' @examples
#' f_('var')
f_ <- function(x, env = parent.frame()) {
  if (is_formula(x)) {
    f_env(x) <- env
    x
  } else if (is.character(x) && length(x) == 1L) {
    f <- ~x
    f_rhs(f) <- as.symbol(x)
    f_env(f) <- env
    f
  } else {
    stop("'x' must be either a formula or a character string", call. = FALSE)
  }
}
