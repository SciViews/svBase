#' Row-wise creation of a data frame
#'
#' @description The presentation of the data (see examples) is easier to read
#'   than with the traditional column-wise entry in [dtx()]. This could be used
#'   to enter small tables in R, but do not abuse of it!
#'
#' @param ... Specify the structure of the data frame by using formulas for
#'   variable names like `~x` for variable `x`. Then, use one argument per value
#'   in the data frame. It is possible to unquote with `!!` and to
#'   unquote-splice with `!!!`.
#'
#' @return A data frame of class **data.frame** for [dtf_rows()], **data.table**
#' for [dtt_rows()], tibble **tbl_df** for [dtbl_rows()] and the default object
#' with [dtx_rows()].
#'
#' @export
#'
#' @examples
#' df <- dtx_rows(
#'   ~x, ~y, ~group,
#'    1,  3,    "A",
#'    6,  2,    "A",
#'    10, 4,    "B"
#' )
#' df
dtx_rows <- function(...) {
  dtx(tribble(...))
}

#' @export
#' @rdname dtx_rows
dtf_rows <- function(...) {
  dtf(tribble(...))
}

#' @export
#' @rdname dtx_rows
dtt_rows <- function(...) {
  dtt(tribble(...))
}

#' @export
#' @rdname dtx_rows
dtbl_rows <- function(...) {
  tribble(...)
}
