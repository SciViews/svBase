# formula_select is the equivalent of tidy-select, but using formulas
# There is a fast subset of tidy-select handled by {collapse}, but for more
# complex cases, we rely on tidyselect::eval_select().

#' Formula-select is similar to tidy-select in the tidyverse, but using formulas
#'
#' @description
#' The formula-select interface allows to give arguments in the tidy-select
#' style withing formulas, or as standard-evaluated arguments. It thus combines
#' both approaches within the same function, and makes it clearer which are the
#' intention: standard evaluation, or non-standard evaluation when formulas are
#' used.
#'
#' @param ... Arguments to be processed by formula-masking.
#' @param .fast.allowed.funs A character vector of function names that are
#'   allowed for a fast treatment (usually though collapse functions). If any
#'   other function is used, the slower tidy-select mechanism is used, see
#'   [tidyselect::eval_select()].
#' @param .max.args The maximum allowed arguments in `...`.
#' @param .must.be.named If `TRUE`, all arguments must be named.
#' @param .make.names If `TRUE`, unnamed arguments are named automatically.
#' @param .no.se If `TRUE`, standard evaluation is not allowed.
#' @param .no.se.msg The message to be used if standard evaluation is not
#'   allowed.
#' @param .envir The environment where to expand formulas (possibly superseded
#'   by the environment attached to the first formula).
#' @param .frame The frame where the focus in the calling stack should be set
#'   in error messages (not used yet).
#'
#' @returns A list with components:
#' * `dots`: the processed arguments (formulas are turned into expressions)
#' * `are_formulas`: whether the arguments were formulas
#' * `env`: The environment where the expressions should be evaluated.
#' * `fastselect`: whether fast selection can be used
#' @export
#' @seealso [formula_masking()],
#'   <https://tidyselect.r-lib.org/articles/syntax.html> or
#'   `vignette("syntax", package = "tidyselect")` for a technical
#'   description of the rules of evaluation.
#'
#' @examples
#' # TODO...
formula_select <- function(..., .fast.allowed.funs = NULL, .max.args = NULL,
  .must.be.named = FALSE, .make.names = FALSE, .no.se = FALSE,
  .no.se.msg = gettext("Standard evaluation is not allowed."),
  .envir = parent.frame(2L), .frame = parent.frame()) {

  .__top_call__. <- TRUE

  # Process the arguments with formula_masking()
  args <- formula_masking(..., .max.args = .max.args,
    .must.be.named = .must.be.named,.make.names = .make.names,
    .no.se = .no.se, .no.se.msg = .no.se.msg, .envir = .envir)

  # For SE, always OK to use fast-select
  if (!args$are_formulas || !length(.fast.allowed.funs)) {
    args$fastselect <- TRUE
  } else {# For formulas, check that all arguments are fast-selectable
    is_arg_ok <- function(arg, .fast.allowed.funs)
      !length(expr_funs(arg, exclude.names = .fast.allowed.funs))

    if (length(args$dots) == 1L) {
      args$fastselect <- is_arg_ok(args$dots[[1]],
        .fast.allowed.funs = .fast.allowed.funs)
    } else {
      args$fastselect <- all(sapply(args$dots, is_arg_ok,
        .fast.allowed.funs = .fast.allowed.funs))
    }
  }
  args
}
