#' Coerce objects into data.frames, data.tables  or tibbles
#'
#' @param x An object.
#' @param ... Further arguments passed to the methods (not used yet).
#' @param rownames The name of the column with row names. If `NULL`, it is assessed from `getOptions("SciViews.dtx.rownames")`.
#' @param row.names Same as `rownames`, but for base R functions.
#' @param keep.key Do we keep the data.table key into a "key" attribute or do we restore `data.table`key from the attribute?
#' @param byref If `TRUE`, the object is modified by reference when converted into a `data.table` (faster, but not conventional). This is `FALSE` by default, or `NULL` if the argument does not apply in the context.
#' @param optional logical, If `TRUE`, setting row names and converting column names to syntactically correct names is optional.
#'
#' @return The coerced object. For `as_dtx()`, the coercion is determined from `getOption("SciViews.as_dtx")` which must return one of the three other `as_dt...()` functions (`as_dtt` by default). The `default_dtx()` does the same as `as_dtx()` if the object is a data.frame, a data.table, or a tibble, but it return the unmodified object for any other class (including subclassed data frames). This is a convenient function to force conversion only between those three objects classes.
#'
#' @note
#' Use [as_matrix()] instead of [base::as.matrix()]: it has different default
#' arguments to better account for `rownames` in data.table and tibble!
#' @export
#'
#' @examples
#' # A data.frame
#' dtf <- dtf(
#'   x = 1:5,
#'   y = rnorm(5),
#'   f = letters[1:5],
#'   l = sample(c(TRUE, FALSE), 5, replace = TRUE))
#'
#' # Convert into a tibble
#' (dtbl <- as_dtbl(dtf))
#' # Since row names are trivial (1 -> 5), a .rownames column is not added
#'
#' dtf2 <- dtf
#' rownames(dtf2) <- letters[1:5]
#' dtf2
#'
#' # Now, the conversion into a tibble adds .rownames
#' (dtbl2 <- as_dtbl(dtf2))
#' # and data frame row names are set again when converted bock to dtf
#' as_dtf(dtbl2)
#'
#' # It also work for conversions data.frame <-> data.table
#' (dtt2 <- as_dtt(dtf2))
#' as_dtf(dtt2)
#'
#' # It does not work when converting a tibble or a data.table into a matrix
#' # with as.matrix()
#' as.matrix(dtbl2)
#' # ... but as_matrix() does the job!
#' as_matrix(dtbl2)
#'
#' # The name for row in dtt and dtbl is in:
#' # (data.frame's row names are converted into a column with this name)
#' getOption("SciViews.dtx.rownames", default = ".rownames")
#'
#' # Convert into the preferred data frame object (data.table by default)
#' (dtx2 <- as_dtx(dtf2))
#' class(dtx2)
#'
#' # The default data frame object used:
#' getOption("SciViews.as_dtx", default = as_dtt)
#'
#' # default_dtx() does the same as as_dtx(),
#' # but it also does not change other objects
#' # So, it is safe to use whaterver the object you pass to it
#' (dtx2 <- default_dtx(dtf2))
#' class(dtx2)
#' # Any other object than data.frame, data.table or tbl_df is not converted
#' res <- default_dtx(1:5)
#' class(res)
#' # No conversion if the data frame is subclassed
#' dtf3 <- dtf2
#' class(dtf3) <- c("subclassed", "data.frame")
#' class(default_dtx(dtf3))
#'
#' # data.table keys are converted into a 'key' attribute and back
#' library(data.table)
#' setkey(dtt2, 'x')
#' haskey(dtt2)
#' key(dtt2)
#'
#' (dtf3 <- as_dtf(dtt2))
#' attributes(dtf3)
#' # Key is restored when converted back into a data.table (also from a tibble)
#' (dtt3 <- as_dtt(dtf3))
#' haskey(dtt3)
#' key(dtt3)
as_dtx <- function(x, ..., rownames = NULL, keep.key = TRUE,
  byref = FALSE) {
  if (is.null(rownames))
    rownames <- getOption("SciViews.dtx.rownames", default = ".rownames")
  getOption("SciViews.as_dtx", default = as_dtt)(x, ..., rownames = rownames,
    keep.key = keep.key, byref = byref)
}

#' @export
#' @rdname as_dtx
as_dtf <- function(x, ..., rownames = NULL, keep.key = TRUE,
  byref = NULL) {
  if (is.null(rownames))
    rownames <- getOption("SciViews.dtx.rownames", default = ".rownames")

  dtf <- as.data.frame(x, ...)

  # If there is a column named as rownames, convert it into row names
  if (rownames %in% names(dtf)) {
    rownames(dtf) <- dtf[[rownames]]
    dtf[[rownames]] <- NULL
  }

  # Possibly get data.table keys
  if (isTRUE(keep.key) && haskey(x))
    attr(dtf, "key") <- key(x)

  dtf
}

#' @export
#' @rdname as_dtx
as_dtt <- function(x, ..., rownames = NULL, keep.key = TRUE,
  byref = FALSE) {
  if (is.null(rownames))
    rownames <- getOption("SciViews.dtx.rownames", default = ".rownames")
  if (rownames %in% names(x) || all(rownames(x) == seq_len(nrow(x))))
    rownames <- FALSE # Otherwise, rownames is duplicated or trivial ones are added
  if (is.data.frame(x) && isTRUE(byref)) {
    if (isTRUE(keep.key)) {
      key <- attr(x, "key")
    } else {
      key <- NULL
    }
    setDT(x, keep.rownames = rownames, key = key)
    attr(x, "key") <- NULL
  } else {
    key <- attr(x, "key")
    x <- as.data.table(x, keep.rownames = rownames)
    if (isTRUE(keep.key) && !is.null(key))
      setkeyv(x, key)
    attr(x, "key") <- NULL
  }
  rownames(x) <- NULL
  x
}

#' @export
#' @rdname as_dtx
as_dtbl <- function(x, ..., rownames = NULL, keep.key = TRUE,
  byref = NULL) {
  if (is.null(rownames))
    rownames <- getOption("SciViews.dtx.rownames", default = ".rownames")
  if (is.logical(rownames) && rownames == FALSE)
    rownames <- NULL # Not FALSE here, but NULL to drop rownames
  if (rownames %in% names(x) || all(rownames(x) == seq_len(nrow(x))))
    rownames <- NULL # Otherwise, rownames is duplicated (not FALSE here, but NULL)
  # Or trivial rownames are added
  if (isTRUE(keep.key) && haskey(x)) {
    key <- key(x)
  } else {
    key <- NULL
  }
  dtbl <- as_tibble(x, ..., rownames = rownames)
  attr(dtbl, "key") <- key
  dtbl
}

#' @export
#' @rdname as_dtx
default_dtx <- function(x, ..., rownames = NULL, keep.key = TRUE,
  byref = FALSE) {
  if (is_dtx(x, strict = TRUE)) {
    # Convert
    if (is.null(rownames))
      rownames <- getOption("SciViews.dtx.rownames", default = ".rownames")
    getOption("SciViews.as_dtx", default = as_dtt)(x, ..., rownames = rownames,
      keep.key = keep.key, byref = byref)
  } else {
    # Keep intact
    x
  }
}

#' @export
#' @rdname as_dtx
#' @method as.matrix tbl_df
as.matrix.tbl_df <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.matrix(as.data.frame(x, row.names = row.names, optional = optional, ...))
}

#' @export
#' @rdname as_dtx
as_matrix <- function(x, rownames = NULL, ...) {
  if (is.null(rownames))
    rownames <- getOption("SciViews.dtx.rownames", default = ".rownames")
  if (is.logical(rownames) && isTRUE(!rownames))
    rownames <- NULL # The value to use for data.table conversion
  # Special case for tbl_df: .rownames is **not** honored.
  # So, it is transformed first
  x <- as_dtf(x)
  as.matrix(x, rownames = rownames, row.names = rownames, ...)
}
