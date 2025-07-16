#' Test if the object is a data frame (data.trame, data.frame, data.table or tibble)
#'
#' @param x An object
#' @param strict Should this be strictly the corresponding class `TRUE`, by default, or could it be subclassed too (`FALSE`). With `strict = TRUE`, the **grouped_df** tibbles and **grouped_ts** tsibbles are also considered (tibbles or tsibbles where [dplyr::group_by()] was applied).
#'
#' @return These functions return `TRUE` if the object is of the correct class, otherwise they return `FALSE`. `is_dtx()` return `TRUE` if `x` is one of a data.frame, data.table, tibble, or data.trame.
#' @export
#'
#' @examples
#' # data(mtcars)
#' is_dtf(mtcars) # TRUE
#' is_dtx(mtcars) # Also TRUE
#' is_dtt(mtcars) # FALSE
#' is_dtbl(mtcars) # FALSE
#' is_dtrm(mtcars) # FALSE
#' # but...
#' is_dtt(as_dtt(mtcars)) # TRUE
#' is_dtx(as_dtt(mtcars)) # TRUE
#' is_dtbl(as_dtbl(mtcars)) # TRUE
#' is_dtx(as_dtbl(mtcars)) # TRUE
#' is_dtrm(as_dtrm(mtcars)) # TRUE
#' is_dtx(as_dtrm(mtcars)) # TRUE
#' is_dtx(as_dtbl(mtcars) |> dplyr::group_by(cyl)) # TRUE (special case)
#'
#' is_dtx("some string") # FALSE
is_dtx <- function(x, strict = TRUE) {
  if (isTRUE(strict)) {
    # One of the three type, and not subclassed (except with grouped_df)
    class1 <- class(x)[1]
    # Special cases: {datasets}, dplyr::group_by(), collapse::fgroup_by(),
    # readr::read_csv()
    # TODO: for nfnGroupedData, get labels and units
    if (class1 %in% c("nfnGroupedData", "nfGroupedData", "groupedData",
      "grouped_df", "grouped_ts", "GRP_df", "spec_tbl_df")) {
      inherits(x, "data.frame")
    } else {
      class1 %in% c("data.frame", "data.table", "tbl_df", "data.trame")
    }
  } else {
    # All three types inherits from data.frame
    inherits(x, "data.frame")
  }
}

#' @export
#' @rdname is_dtx
is_dtrm <- function(x, strict = TRUE) {
  if (isTRUE(strict)) {
    class1 <- class(x)[1]
    inherits(x, "data.trame") && class1 %in% c("data.trame", "GRP_df")
  } else {
    inherits(x, "data.trame")
  }
}

#' @export
#' @rdname is_dtx
is_dtf <- function(x, strict = TRUE) {
  if (isTRUE(strict)) {
    class1 <- class(x)[1]
    if (class1 %in% c("nfnGroupedData", "nfGroupedData", "groupedData",
      "grouped_df", "grouped_ts", "GRP_df", "spec_tbl_df")) {
      !inherits(x, c("data.table", "tbl_df", "tbl_ts"))
    } else {
      class1 == "data.frame"
    }
  } else {
    inherits(x, "data.frame")
  }
}

#' @export
#' @rdname is_dtx
is_dtt <- function(x, strict = TRUE) {
  if (isTRUE(strict)) {
    class1 <- class(x)[1]
    inherits(x, "data.table") && class1 %in% c("data.table", "GRP_df")
  } else {
    inherits(x, "data.table")
  }
}

#' @export
#' @rdname is_dtx
is_dtbl <- function(x, strict = TRUE) {
  if (isTRUE(strict)) {
    class1 <- class(x)[1]
    # Special case
    if (class1 %in% c("grouped_df", "grouped_ts", "GRP_df", "spec_tbl_df")) {
      inherits(x, "tbl_df")
    } else {
      class1 == "tbl_df"
    }
  } else {
    inherits(x, "tbl_df")
  }
}
