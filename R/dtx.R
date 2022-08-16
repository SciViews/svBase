#' Create a data frame (base's data.frame, data.table or tibble's tbl_df)
#'
#' @param ... A set of name-value pairs. The content of the data frame. See
#' [tibble()] for more details on the way dynamic-dots are processed.
#' @param .name_repair The way problematic column names are treated, see also
#' [tibble()] for details.
#'
#' @return
#' A data frame as a **tbl_df** object for [dtbl()], a **data.frame** for
#' [dtf()] and a **data.table** for [dtt()].
#'
#' @export
#'
#' @examples
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
dtx <- function(...,
.name_repair = c("check_unique", "unique", "universal", "minimal")) {
  NULL # TO be changed!
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
  as.data.frame(tibble(..., .name_repair = .name_repair))
}

#' @export
#' @rdname dtx
dtt <- function(...,
.name_repair = c("check_unique", "unique", "universal", "minimal")) {
  setDT(tibble(..., .name_repair = .name_repair))
}
