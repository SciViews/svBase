#' Create a data frame (base's data.frame, data.table or tibble's tbl_df)
#'
#' @param ... A set of name-value pairs. The content of the data frame. See
#' [tibble()] for more details on the way dynamic-dots are processed.
#' @param .name_repair The way problematic column names are treated, see also
#' [tibble()] for details.
#' @param x An object to print.
#'
#' @note data.table and tibble's tbl_df do no use row names. However, you can
#' add a column named `.rownames`(by default), or the name that is in
#' `getOption("SciViews.dtx.rownames")` and it will be automatically set as row
#' names when the object is converted into a data.frame with [as_dtf()]. For
#' [dtf()], just create a column of this name and it is directly used as row
#' names for the resulting data.frame object.
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
#'
#' # Using dtx(), one construct the preferred data frame object
#' # (a data.table by default, can be changed with options(SciViews.as_dtx = ...))
#' dtx1 <- dtx(
#'   x = 1:5,
#'   y = rnorm(5),
#'   f = letters[1:5],
#'   l = sample(c(TRUE, FALSE), 5, replace = TRUE))
#' class(dtx1) # data.table by default
#'
#' # With {svBase} data.table and data.frame objects have the same nice print as tibbles
#' dtbl1
#' dtf1
#' dtt1
#'
#' # Use tribble() inside dtx() to easily create a data frame:
#' library(tibble)
#' dtx2 <- dtx(tribble(
#'   ~x, ~y, ~f,
#'    1,  3, 'a',
#'    2,  4, 'b'
#' ))
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

#' @export
#' @rdname dtx
#' @method show data.frame
show.data.frame <- function(x, ...) {
  # With pillar, row names are **not** printed, so, instead of as_tibble()
  # we use as_dtbl() that creates .rownames, then we change its name and class
  # to make it more obvious that these are the names of the rows
  #y <- as_tibble(x)
  y <- as_dtbl(x, rownames = "\u00a0") # u00a0 is nonbreaking space
  if ("\u00a0" %in% names(y))
    class(y[["\u00a0"]]) <- c("rownames", "noquote")
  class(y) <- unique(c("dataframe", class(y)))
  show(y)
  invisible(x)
}

#' @export
#' @rdname dtx
#' @method tbl_sum dataframe
tbl_sum.dataframe <- function(x, ...) {
  lang <- attr(comment(x), "lang")
  nc <- ncol(x)
  # In case we have rownames (column with name \u00a0), we have one column less
  if ("\u00a0" %in% names(x))
    nc <- nc - 1
  if (is.null(lang)) {
    c(`A data.frame` = paste(nrow(x), "x", nc))
  } else {
    c(`A data.frame` = paste(nrow(x), "x", nc), Language = lang)
  }
}

#' @export
#' @rdname dtx
#' @method show data.table
show.data.table <- function(x, ...) {
  y <- as_tibble(x)
  class(y) <- unique(c("datatable", class(y)))
  show(y)
  invisible(x)
}

#' @export
#' @rdname dtx
#' @method tbl_sum datatable
tbl_sum.datatable <- function(x, ...) {
  lang <- attr(comment(x), "lang")
  if (is.null(lang)) {
    c(`A data.table` = paste(nrow(x), "x", ncol(x)))
  } else {
    c(`A data.table` = paste(nrow(x), "x", ncol(x)), Language = lang)
  }
}
