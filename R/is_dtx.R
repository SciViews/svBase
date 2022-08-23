#' Test if the object is a data frame (data.frame, data.table or tibble)
#'
#' @param x An object
#' @param strict Should this be strictly the corresponding class `TRUE`, by default, or could it be subclassed too (`FALSE`).
#'
#' @return These functions return `TRUE` if the object is of the correct class, otherwise they return `FALSE`. `is_dtx()` return `TRUE` if `x` is one of a data.frame, data.table or tibble.
#' @export
#'
#' @examples
#' # data(mtcars)
#' is_dtf(mtcars) # TRUE
#' is_dtx(mtcars) # Also TRUE
#' is_dtt(mtcars) # FALSE
#' is_dtbl(mtcars) # FALSE
#' # but...
#' is_dtt(as_dtt(mtcars)) # TRUE
#' is_dtx(as_dtt(mtcars)) # TRUE
#' is_dtbl(as_dtbl(mtcars)) # TRUE
#' is_dtx(as_dtbl(mtcars)) # TRUE
#'
#' is_dtx("some string") # FALSE
is_dtx <- function(x, strict = TRUE) {
  if (isTRUE(strict)) {
    # One of the three type, and not subclassed
    class1 <- class(x)[1]
    class1 == "data.frame" || class1 == "data.table" || class1 == "tbl_df"
  } else {
    # All three types inherits from data.frame
    inherits(x, "data.frame")
  }
}

#' @export
#' @rdname is_dtx
is_dtf <- function(x, strict = TRUE) {
  if (isTRUE(strict)) {
    class(x)[1] == "data.frame"
  } else {
    inherits(x, "data.frame")
  }
}

#' @export
#' @rdname is_dtx
is_dtt <- function(x, strict = TRUE) {
  if (isTRUE(strict)) {
    class(x)[1] == "data.table"
  } else {
    inherits(x, "data.table")
  }
}

#' @export
#' @rdname is_dtx
is_dtbl <- function(x, strict = TRUE) {
  if (isTRUE(strict)) {
    class(x)[1] == "tbl_df"
  } else {
    inherits(x, "tbl_df")
  }
}
