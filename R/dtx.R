#' Create a data frame (dta.trame, base's data.frame, data.table or tibble's tbl_df)
#'
#' @param ... A set of name-value pairs. The content of the data frame. See
#' [tibble()] for more details on the way dynamic-dots are processed.
#' @param .name_repair The way problematic column names are treated, see also
#' [tibble()] for details.
#'
#' @note data.trame, data.table and tibble's tbl_df do no use row names.
#' However, you can add a column named `.rownames`(by default), or the name that
#' is in `getOption("SciViews.dtx.rownames")` and it will be automatically set as
#' row names when the object is converted into a data.frame with [as_dtf()]. For
#' [dtf()], just create a column of this name and it is directly used as row
#' names for the resulting data.frame object.
#'
#' @return
#' A data frame as a **data.trame** object for [dtrm()], a **tbl_df** object
#' for [dtbl()], a **data.frame** for [dtf()] or a **data.table** for [dtt()].
#'
#' @export
#' @seealso [dtx_rows()], [is_dtx()], [collect_dtx()]
#'
#' @examples
#' dtrm1 <- dtrm(
#'   x = 1:5,
#'   y = rnorm(5),
#'   f = letters[1:5],
#'   l = sample(c(TRUE, FALSE), 5, replace = TRUE)
#' )
#' class(dtrm1)
#'
#' dtbl1 <- dtbl(
#'   x = 1:5,
#'   y = rnorm(5),
#'   f = letters[1:5],
#'   l = sample(c(TRUE, FALSE), 5, replace = TRUE)
#' )
#' class(dtbl1)
#'
#' dtf1 <- dtf(
#'   x = 1:5,
#'   y = rnorm(5),
#'   f = letters[1:5],
#'   l = sample(c(TRUE, FALSE), 5, replace = TRUE)
#' )
#' class(dtf1)
#'
#' dtt1 <- dtt(
#'   x = 1:5,
#'   y = rnorm(5),
#'   f = letters[1:5],
#'   l = sample(c(TRUE, FALSE), 5, replace = TRUE))
#' class(dtt1)
#'
#' # Using dtx(), one construct the preferred data frame object
#' # (a data.trame by default, can be changed with options(SciViews.as_dtx = ...))
#' dtx1 <- dtx(
#'   x = 1:5,
#'   y = rnorm(5),
#'   f = letters[1:5],
#'   l = sample(c(TRUE, FALSE), 5, replace = TRUE))
#' class(dtx1) # data.trame by default
#'
#' # Use dtx_rows() to easily create a data frame:
#' dtx2 <- dtx_rows(
#'   ~x, ~y, ~f,
#'    1,  3, 'a',
#'    2,  4, 'b'
#' )
#' dtx2
#' class(dtx2)
#'
#' # This is how you specify row names for dtf (data.frame)
#' dtf(x = 1:3, y = 4:6, .rownames = letters[1:3])
dtx <- function(...,
    .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  as_dtx(tibble(..., .name_repair = .name_repair))
}

#' @export
#' @rdname dtx
dtrm <- function(...,
  .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  data.trame(..., .name_repair = .name_repair)
}


#' @export
#' @rdname dtx
dtbl <- function(...,
    .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  tibble(..., .name_repair = .name_repair)
}

#' @export
#' @rdname dtx
dtf <- function(...,
    .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  as_dtf(tibble(..., .name_repair = .name_repair))
}

#' @export
#' @rdname dtx
dtt <- function(...,
    .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  setDT(tibble(..., .name_repair = .name_repair))
}

