#' Speedy functions (mainly from {collapse} and {data.table}) to manipulate data frames
#'
#' @description The Tidyverse defines a coherent set of tools to manipulate
#' data frames that use a non-standard evaluation and sometimes require extra
#' care. These functions, like [mutate()] or [summarise()] are defined in the
#' {dplyr} and {tidyr} packages. The {collapse} package proposes a couple of
#' functions with similar interface, but with different and much faster code.
#' For instance, [fselect()] is similar to [select()], or [fsummarise()] is
#' similar to [summarise()]. Not all functions are implemented, arguments and
#' argument names differ, and the behavior may be very different, like
#' [frename()] which uses `old_name = new_name`, while [rename()] uses
#' `new_name = old_name`! The speedy functions all start with an "s" prefixed to
#' the function name, like [smutate()], and build on the work initiated in
#' {collapse} to propose a series of paired functions with the tidy ones. So,
#' [smutate()] and [tmutate()] are "speedy" and 'tidy" counterparts and they are
#' used in a very similar, if not identical way as the original [mutate()]
#' function. This notation using a "s" prefix is there to draw the attention on
#' their particularities. Their classes are **function** and **speedy_fn**.
#' Avoid mixing tidy, speedy and non-tidy/speedy functions in the same pipeline.
#'
#' @param .data A data frame (data.frame, data.table or tibble's tbl_df)
#' @param ... Arguments dependent to the context of the function and most of
#'   the time, not evaluated in a standard way (cf. the tidyverse approach).
#' @param .fn A function to use.
#' @param .cols The list of the column where to apply the transformation. For
#'   the moment, only all existing columns, which means `.cols = everything()`
#'   is implemented
#' @param .keep Which columns to keep. The default is `"all"`, possible values
#' are `"used"`, `"unused"`, or `"none"` (see [mutate()]).
#' @param x A data frame (data.frame, data.table or tibble's tbl_df).
#' @param y A second data frame.
#' @param by A list of names of the columns to use for joining the two data
#' frames.
#' @param suffix The suffix to the column names to use to differentiate the
#' columns that come from the first or the second data frame. By default it is
#' `c(".x", ".y")`.
#' @param copy This argument is there for compatibility with the "t" matching
#' functions, but it is not used here.
#'
#' @note The help page here is very basic and it aims mainly to list all the
#' speedy functions. For more complete help, see their "non-s" counterparts in
#' {dplyr} or {tidyr}, or use the {svMisc}'s `.?smutate` syntax to link to the
#' correct page.
#'
#' @return See corresponding "non-s" function for the full help page with
#' indication of the return values.
#'
#' @export
#'
#' @examples
#' # TODO...
speedy_functions <- function() {
  c("sfilter", "sfilter_ungroup", "sfull_join", "sgroup_by", "sinner_join",
    "sleft_join", "smutate", "smutate_ungroup", "srename", "srename_with",
    "sright_join", "sselect", "ssummarise", "stransmute", "stransmute_ungroup",
    "sungroup")
}

.src_speedy <- function(src, comment = "A speedy function, see ?speedy_functions.") {
  attr(comment, "src") <- src
  comment
}

#' @export
#' @rdname speedy_functions
sgroup_by <- structure(function(.data, ...) {
  # Different args names than fgroup_by() and not all arguments not in group_by()
  fgroup_by(.data, ...)
}, class = c("function", "speedy_fn"), comment = .src_speedy("collapse::fgroup_by"))

#' @export
#' @rdname speedy_functions
sungroup <- structure(function(.data, ...) {
  # Different args names (x= instead of X=), but we use .data= as for the other
  # tidy/speedy functions
  fungroup(.data, ...)
}, class = c("function", "speedy_fn"), comment = .src_speedy("collapse::fungroup"))

#' @export
#' @rdname speedy_functions
srename <- structure(function(.data, ...) {
  # collapse::frename() uses old_name = new_name, but dplyr::rename() uses
  # new_name = old_name -> make frename() compatible with dplyr::rename()
  # Also, the cols= argument with a function matches rename_with() instead.
  if (missing(.data))
    stop("You must provide .data")
  new_names <- ...names()
  if (any(new_names == ""))
    stop("You must provide new_name = old_name (all named arguments)")
  old_names <- as.character(match.call()[-(1:2)])
  if (length(new_names) != length(old_names))
    stop("Arguments and argument names do not match")
  dots <- as.list(new_names)
  dots <- lapply(dots, as.name)
  names(dots) <- old_names
  do.call(frename, c(list(.x = substitute(.data)), dots),
    envir = parent.frame())
}, class = c("function", "speedy_fn"), comment = .src_speedy("dplyr::rename"))

everything <- function() {
  stop("This should not be called directly, see ?mutate_with.")
}

#' @export
#' @rdname speedy_functions
srename_with <- structure(function(.data, .fn, .cols = everything(), ...) {
  # The equivalent of rename_with(),
  # but currently supporting only .cols = everything()

  # For now, only manage the case of .cols = everything()
  if (deparse(substitute(.cols)) != "everything()")
    stop("Only supporting .cols = everything() for now, sorry.")
  if (!is.function(.fn))
    stop("Can only rename with a function for now, sorry.")
  frename(.data, .fn, ...) # cols= missing to apply to all columns
}, class = c("function", "speedy_fn"), comment = .src_speedy("dplyr::rename_with"))

#' @export
#' @rdname speedy_functions
sfilter <- structure(function(.data, ...) {
  filters <- match.call()[-(1:2)]
  # fsubset() can use only one subset argument at a time. So, we run it
  # multiple times on each argument to sfilter() to mimic tfilter()
  for (i in 1:...length())
    .data <- do.call(fsubset, list(.x = .data, filters[[i]]))
  .data
}, class = c("function", "speedy_fn"), comment = .src_speedy("dplyr::filter"))

#' @export
#' @rdname speedy_functions
sfilter_ungroup <- structure(function(.data, ...) {
  fungroup(sfilter(.data, ...))
}, class = c("function", "speedy_fn"), comment = .src_speedy("dplyr::filter",
  comment = "A speedy function, see ?speedy_functions, combining filter() and ungroup()."))

#' @export
#' @rdname speedy_functions
sselect <- structure(function(.data, ...) {
  fselect(.data, ..., return = "data") # Other return modes not supported (yet)
}, class = c("function", "speedy_fn"), comment = .src_speedy("collapse::fselect"))

#' @export
#' @rdname speedy_functions
smutate <- structure(collapse::fmutate,
  # TODO: Arguments .before= and .after= not supported yet, but to be implemented
  class = c("function", "speedy_fn"), comment = .src_speedy("collapse::fmutate"))

#' @export
#' @rdname speedy_functions
smutate_ungroup <- structure(function(.data, ..., .keep = "all") {
  fungroup(fmutate(.data, ..., .keep = .keep))
}, class = c("function", "speedy_fn"), comment = .src_speedy("dplyr::mutate",
  comment = "A speedy function, see ?speedy_functions, combining mutate() and ungroup()."))

#' @export
#' @rdname speedy_functions
stransmute <- structure(function(.data, ...) {
  fmutate(.data, ...)[, ...names()]
}, class = c("function", "speedy_fn"), comment = .src_speedy("dplyr::transmute"))

#' @export
#' @rdname speedy_functions
stransmute_ungroup <- structure(function(.data, ...) {
  fungroup(fmutate(.data, ...)[, ...names()])
}, class = c("function", "speedy_fn"), comment = .src_speedy("dplyr::transmute",
  comment = "A speedy function, see ?speedy_functions, combining transmute() and ungroup()."))

#' @export
#' @rdname speedy_functions
ssummarise <- structure(function(.data, ...) {
  # keep.group_vars = FALSE not in dplyr::summarise()
  fsummarise(.data, ..., keep.group_vars = TRUE)
}, class = c("function", "speedy_fn"), comment = .src_speedy("collapse::fsummarise"))

#' @export
#' @rdname speedy_functions
sfull_join <- structure(function(x, y, by = NULL, suffix = c(".x", ".y"),
copy = FALSE, ...) {
  if (!missing(copy))
    warning("This argument is here only for compatibility with tfull_join() but it does nothing here.")
  # We transform x into a data.table, then restore the result into data.frame
  # or  tibble if needed
  is_x_dtf <- is_dtf(x)
  is_x_dtbl <- is_dtbl(x)
  x <- as_dtt(x)
  res <- merge(x, y, by = by, all = TRUE, all.x = TRUE, all.y = TRUE,
    sort = TRUE, suffixes = suffix, no.dups = TRUE, allow.cartesian = FALSE)
  # Transform if needed
  if (is_x_dtf)
    res <- as_dtf(res)
  if (is_x_dtbl)
    res <- as_dtbl(res)
  res
}, class = c("function", "speedy_fn"), comment = .src_speedy("dplyr::full_join"))

#' @export
#' @rdname speedy_functions
sleft_join <- structure(function(x, y, by = NULL, suffix = c(".x", ".y"),
copy = FALSE, ...) {
  if (!missing(copy))
    warning("This argument is here only for compatibility with tleft_join() but it does nothing here.")
  # We transform x into a data.table, then restore the result into data.frame
  # or  tibble if needed
  is_x_dtf <- is_dtf(x)
  is_x_dtbl <- is_dtbl(x)
  x <- as_dtt(x)
  res <- merge(x, y, by = by, all.x = TRUE, all.y = FALSE,
    sort = TRUE, suffixes = suffix, no.dups = TRUE, allow.cartesian = FALSE)
  # Transform if needed
  if (is_x_dtf)
    res <- as_dtf(res)
  if (is_x_dtbl)
    res <- as_dtbl(res)
  res
}, class = c("function", "speedy_fn"), comment = .src_speedy("dplyr::left_join"))

#' @export
#' @rdname speedy_functions
sright_join <- structure(function(x, y, by = NULL, suffix = c(".x", ".y"),
copy = FALSE, ...) {
  if (!missing(copy))
    warning("This argument is here only for compatibility with tright_join() but it does nothing here.")
  # We transform x into a data.table, then restore the result into data.frame
  # or  tibble if needed
  is_x_dtf <- is_dtf(x)
  is_x_dtbl <- is_dtbl(x)
  x <- as_dtt(x)
  res <- merge(x, y, by = by, all.x = FALSE, all.y = TRUE,
    sort = TRUE, suffixes = suffix, no.dups = TRUE, allow.cartesian = FALSE)
  # Transform if needed
  if (is_x_dtf)
    res <- as_dtf(res)
  if (is_x_dtbl)
    res <- as_dtbl(res)
  res
}, class = c("function", "speedy_fn"), comment = .src_speedy("dplyr::right_join"))

#' @export
#' @rdname speedy_functions
sinner_join <- structure(function(x, y, by = NULL, suffix = c(".x", ".y"),
copy = FALSE, ...) {
  if (!missing(copy))
    warning("This argument is here only for compatibility with tinner_join() but it does nothing here.")
  # We transform x into a data.table, then restore the result into data.frame
  # or  tibble if needed
  is_x_dtf <- is_dtf(x)
  is_x_dtbl <- is_dtbl(x)
  x <- as_dtt(x)
  res <- merge(x, y, by = by, all.x = FALSE, all.y = FALSE,
    sort = TRUE, suffixes = suffix, no.dups = TRUE, allow.cartesian = FALSE)
  # Transform if needed
  if (is_x_dtf)
    res <- as_dtf(res)
  if (is_x_dtbl)
    res <- as_dtbl(res)
  res
}, class = c("function", "speedy_fn"), comment = .src_speedy("dplyr::inner_join"))

