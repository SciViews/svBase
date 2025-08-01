#' Coerce objects into data.trames, data.frames, data.tables, tibbles or matrices
#'
#' @description Objects are coerced into the desired class. For [as_dtx()], the
#' desired class is obtained from `getOption("SciViews.as_dtx")`, with a default
#' value producing a data.trame object. If the data are grouped with
#' [dplyr::group_by()], the resulting data frame is also [dplyr::ungroup()]ed
#' in the process.
#'
#' @param x An object.
#' @param ... Further arguments passed to the methods (not used yet).
#' @param rownames The name of the column with row names. If `NULL`, it is assessed from `getOptions("SciViews.dtx.rownames")`.
#' @param row.names Same as `rownames`, but for base R functions.
#' @param keep.key Do we keep the data.table key into a "key" attribute or do we restore `data.table` or `data.trame` key from the attribute?
#' @param byref If `TRUE`, the object is modified by reference when converted into a `data.table` or a `data.trame` (faster, but not conventional). This is `FALSE` by default, or `NULL` if the argument does not apply in the context.
#' @param optional logical, If `TRUE`, setting row names and converting column names to syntactically correct names is optional.
#'
#' @return The coerced object. For `as_dtx()`, the coercion is determined from `getOption("SciViews.as_dtx")` which must return one of the four other `as_dt...()` functions (`as_dtrm` by default). The `default_dtx()` does the same as `as_dtx()` if the object is a data.trame, a data.frame, a data.table, or a tibble, but it return the unmodified object for any other class (including subclassed data frames). This is a convenient function to force conversion only between those four objects classes.
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
#' # or data.frame <-> data.trame
#' (dtrm2 <- as_dtrm(dtf2))
#' as_dtf(dtrm2)
#'
#' # It does not work when converting a tibble or a data.table into a matrix
#' # with as.matrix()
#' as.matrix(dtbl2)
#' # ... but as_matrix() does the job!
#' as_matrix(dtbl2)
#'
#' # The name for row in dtrm, dtt and dtbl is in:
#' # (data.frame's row names are converted into a column with this name)
#' getOption("SciViews.dtx.rownames", default = ".rownames")
#'
#' # Convert into the preferred data frame object (data.trame by default)
#' (dtx2 <- as_dtx(dtf2))
#' class(dtx2)
#'
#' # The default data frame object used:
#' getOption("SciViews.as_dtx", default = as_dtrm)
#'
#' # default_dtx() does the same as as_dtx(),
#' # but it also does not change other objects
#' # So, it is safe to use whatever the object you pass to it
#' (dtx2 <- default_dtx(dtf2))
#' class(dtx2)
#' # Any other object than data.trame, data.frame, data.table or tbl_df
#' # is not converted
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
#'
#' # Grouped tibbles are ungrouped with as_dtbl() or as_dtx()/default_dtx()!
#' mtcars |> dplyr::group_by(cyl) -> mtcars_grouped
#' class(mtcars_grouped)
#' mtcars2 <- as_dtbl(mtcars_grouped)
#' class(mtcars2)
as_dtx <- function(x, ..., rownames = NULL, keep.key = TRUE, byref = FALSE) {
  if (is.null(rownames))
    rownames <- getOption("SciViews.dtx.rownames", default = ".rownames")
  getOption("SciViews.as_dtx", default = as_dtrm)(.ungroup_dtbl(x), ...,
    rownames = rownames, keep.key = keep.key, byref = byref)
}

#' @export
#' @rdname as_dtx
as_dtrm <- function(x, ..., rownames = NULL, keep.key = TRUE, byref = FALSE) {
  if (is.null(rownames))
    rownames <- getOption("SciViews.dtx.rownames", default = ".rownames")
  if (rownames %in% names(x) || all(rownames(x) == seq_len(nrow(x))))
    rownames <- FALSE # Otherwise, rownames duplicated or trivial ones added

  if (isTRUE(keep.key)) {
    key <- key(x)
    if (is.null(key))
      key <- attr(x, "key")
  } else {
    key <- NULL
  }

  if (is.list(x) && isTRUE(byref)) {
    x <- .ungroup_dtbl(x)
    setDT(x, keep.rownames = rownames, key = key)
  } else {
    x <- as.data.table(.ungroup_dtbl(x), keep.rownames = rownames)
    setkeyv(x, key)
  }
  setattr(x, 'key', NULL)
  rownames(x) <- NULL
  setattr(x, 'class', c('data.trame', 'data.frame'))
  x
}

#' @export
#' @rdname as_dtx
as_dtf <- function(x, ..., rownames = NULL, keep.key = TRUE, byref = NULL) {
  if (is.null(rownames))
    rownames <- getOption("SciViews.dtx.rownames", default = ".rownames")

  if (isTRUE(keep.key)) {
    key <- key(x)
    if (is.null(key))
      key <- attr(x, "key")
  } else {
    key <- NULL
  }

  dtf <- as.data.frame(.ungroup_dtbl(x), ...)

  # If there is a column named as rownames, convert it into row names
  if (rownames %in% names(dtf)) {
    rownames(dtf) <- dtf[[rownames]]
    dtf[[rownames]] <- NULL
  }

  setattr(dtf, 'key', key)
  setattr(dtf, '.internal.selfref', NULL)

  dtf
}

#' @export
#' @rdname as_dtx
as_dtt <- function(x, ..., rownames = NULL, keep.key = TRUE, byref = FALSE) {
  if (is.null(rownames))
    rownames <- getOption("SciViews.dtx.rownames", default = ".rownames")
  if (rownames %in% names(x) || all(rownames(x) == seq_len(nrow(x))))
    rownames <- FALSE # Otherwise, rownames duplicated or trivial ones added

  if (isTRUE(keep.key)) {
    key <- key(x)
    if (is.null(key))
      key <- attr(x, "key")
  } else {
    key <- NULL
  }

  if (is.list(x) && isTRUE(byref)) {
    x <- .ungroup_dtbl(x)
    setDT(x, keep.rownames = rownames, key = key)
  } else {
    key <- attr(x, "key")
    x <- as.data.table(.ungroup_dtbl(x), keep.rownames = rownames)
    setkeyv(x, key)
  }
  setattr(x, 'key', NULL)
  rownames(x) <- NULL
  x
}

#' @export
#' @rdname as_dtx
as_dtbl <- function(x, ..., rownames = NULL, keep.key = TRUE, byref = NULL) {
  if (is.null(rownames))
    rownames <- getOption("SciViews.dtx.rownames", default = ".rownames")
  if (is.logical(rownames) && rownames == FALSE)
    rownames <- NULL # Not FALSE here, but NULL to drop rownames
  if (rownames %in% names(x) || all(rownames(x) == seq_len(nrow(x))))
    rownames <- NULL # Otherwise, rownames duplicated (not FALSE here, but NULL)
  # or trivial rownames are added

  if (isTRUE(keep.key)) {
    key <- key(x)
    if (is.null(key))
      key <- attr(x, "key")
  } else {
    key <- NULL
  }

  dtbl <- as_tibble(.ungroup_dtbl(x), ..., rownames = rownames)

  setattr(dtbl, "key", key)
  setattr(dtf, '.internal.selfref', NULL)

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
    getOption("SciViews.as_dtx", default = as_dtrm)(.ungroup_dtbl(x), ...,
      rownames = rownames, keep.key = keep.key, byref = byref)
  } else {
    # Keep intact
    x
  }
}

#' @export
#' @rdname as_dtx
#' @method as.matrix tbl_df
as.matrix.tbl_df <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.matrix(as.data.frame(.ungroup_dtbl(x), row.names = row.names,
    optional = optional, ...))
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
  x <- as_dtf(.ungroup_dtbl(x))
  as.matrix(x, rownames = rownames, row.names = rownames, ...)
}

.ungroup_dtbl <- function(x) {
  if (inherits(x, "GRP_df")) {
    res <- fungroup(x)
  } else if (inherits(x, "tbl_df")) {
    res <- ungroup(x)
  } else {
    res <- x
  }
  # Special case for "groupedData"
  if (inherits(res, "groupedData")) {
    res <- as.data.frame(res)
    # If there are labels or units, apply them to res properly
    # y and x, according to formula
    f <- attr(res, "formula")
    # It is like y ~ x | z
    if (length(f) == 3 && length(f[[3]]) == 3) {
      xy <- list(x = as.character(f[[3]][[2]]), y = as.character(f[[2]]))
      labels <- attr(res, "labels")
      if (!is.null(labels) && is.list(labels)) {
        # Apply labels to each column
        nms <- names(labels)
        for (nm in nms)
          attr(res[[xy[[nm]]]], "label") <- labels[[nm]]
        attr(res, "labels") <- NULL
      }
      units <- attr(res, "units")
      if (!is.null(units) && is.list(units)) {
        # Apply labels to each column
        nms <- names(units)
        # If units are between parentheses, eliminate them
        for (nm in nms)
          attr(res[[xy[[nm]]]], "units") <-
            sub("^\\((.+)\\)$", "\\1", units[[nm]])
        attr(res, "units") <- NULL
      }
    }
  }
  res
}
