#' Tidy functions (mainly from {dplyr} and {tidyr}) to manipulate data frames
#'
#' @description The Tidyverse defines a coherent set of tools to manipulate
#' data frames that use a non-standard evaluation and sometimes require extra
#' care. These functions, like [mutate()] or [summarise()] are defined in the
#' {dplyr} and {tidyr} packages. When using variants, like {dtplyr} for
#' **data.frame** objects, or {dbplyr} to work with external databases,
#' successive commands in a pipeline are pooled together but not computed. One
#' has to [collect()] the result to get its final form. The tidy functions have
#' the same name as their {dplyr} or {tidyr} equivalent, but prefixed with "t".
#' These functions are otherwise quasi-identical and use the same code, but
#' their class is both **function** and **tidy_fn**. This notation using a "t"
#' prefix is there to draw the attention on their particularities.
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy
#' data frame (e.g. from dbplyr or dtplyr). See [mutate()] for more details.
#' @param ... Arguments dependent to the context of the function and most of
#'   the time, not evaluated in a standard way (cf. the tidyverse approach).
#' @param .fn A function to use.
#' @param .cols The list of the column where to apply the transformation. See
#'   [rename_with()] with a list of functions that can be used here to specify
#'   easily these columns.
#' @param .keep Which columns to keep. The default is `"all"`, possible values
#' are `"used"`, `"unused"`, or `"none"` (see [mutate()]).
#' @param x A data frame or lazy data frame, as for `.data`.
#' @param y A second data frame or lazy data frame.
#' @param by A list of names of the columns to use for joining the two data
#' frames.
#' @param suffix The suffix to the column names to use to differentiate the
#' columns that come from the first or the second data frame. By default it is
#' `c(".x", ".y")`.
#' @param copy If `x` and ``y`` are not from the same data source, and `copy` is
#'   `TRUE`, then `y` will be copied into the same src as `x`. This allows you
#'   to join tables across srcs, but it is a potentially expensive operation so
#'   you must opt into it..
#'
#' @note The help page here is very basic and it aims mainly to list all the
#' tidy functions. For more complete help, see their "non-t" counterparts in
#' {dplyr} or {tidyr}, or use the {svMisc}'s `.?tmutate` syntax to link to the
#' correct page.
#'
#' @return See corresponding "non-t" function for the full help page with
#' indication of the return values.
#'
#' @export
#'
#' @examples
#' # TODO...
tidy_functions <- function() {
  c("tfilter", "tfilter_ungroup", "tfull_join", "tgroup_by", "tinner_join",
    "tleft_join", "tmutate", "tmutate_ungroup", "trename", "trename_with",
    "tright_join", "tselect", "tsummarise", "ttransmute", "ttransmute_ungroup",
    "tungroup")
}

.src_tidy <- function(src, comment = "A tidy function, see ?tidy_functions.") {
  attr(comment, "src") <- src
  comment
}

#' @export
#' @rdname tidy_functions
tgroup_by <- structure(function(.data, ...) {
  # fgroup_by() does not support .add= and behaves like .drop = TRUE -> force it
  group_by(.data, ..., .add = FALSE, .drop = TRUE)
}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::group_by"))

#' @export
#' @rdname tidy_functions
tungroup <- structure(function(.data, ...) {
  # ungroup() uses x, but it is .data= everywhere else => use .data
  ungroup(.data, ...)
}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::ungroup"))

#' @export
#' @rdname tidy_functions
trename <- structure(dplyr::rename,
  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::rename"))

#' @export
#' @rdname tidy_functions
trename_with <- structure(dplyr::rename_with,
  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::rename_with"))

#' @export
#' @rdname tidy_functions
tfilter <- structure(function(.data, ...) {
  # I don't understand .preserve = FALSE of dplyr::filter() -> not used for now
  filter(.data, ...)
}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::filter"))

#' @export
#' @rdname tidy_functions
tfilter_ungroup <- structure(function(.data, ...) {
  ungroup(filter(.data, ...))
}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::filter",
  comment = "A tidy function, see ?tidy_functions, combining filter() and ungroup()."))

#' @export
#' @rdname tidy_functions
tselect <- structure(dplyr::select,
  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::select"))

#' @export
#' @rdname tidy_functions
# TODO: Arguments .before= and .after= not supported yet, but to be implemented
tmutate <- structure(dplyr::mutate,
  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::mutate",
    comment = "A tidy function, see ?tidy_functions.\nThe .before= and .after= arguments are not implemented yet."))

#' @export
#' @rdname tidy_functions
tmutate_ungroup <- structure(function(.data, ..., .keep = "all") {
  ungroup(mutate(.data, ..., .keep = .keep))
}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::mutate",
  comment = "A tidy function, see ?tidy_functions, combining mutate() and ungroup()."))

#' @export
#' @rdname tidy_functions
ttransmute <- structure(dplyr::transmute,
  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::transmute"))

#' @export
#' @rdname tidy_functions
ttransmute_ungroup <- structure(function(.data, ...) {
  ungroup(transmute(.data, ...))
}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::transmute",
  comment = "A tidy function, see ?tidy_functions, combining transmute() and ungroup()."))

#' @export
#' @rdname tidy_functions
tsummarise <- structure(function(.data, ...) {
  # Apparently, fsummarise() behaves like dplyr:: summarise() < 1.0, thus using
  # .groups = "drop_last' always
  summarise(.data, ..., groups = "drop_last")
}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::summarise"))

#' @export
#' @rdname tidy_functions
tfull_join <- structure(function(x, y, by = NULL, suffix = c(".x", ".y"),
copy = FALSE, ...) {
  full_join(x, y, by = by, copy = copy, suffix = suffix, ...,
    keep = FALSE) # Keep = TRUE not implemented in merge.data.table()
    # na_matches = "na" is the default # NA and NaN are considered equivalent
}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::full_join"))

#' @export
#' @rdname tidy_functions
tleft_join <- structure(function(x, y, by = NULL, suffix = c(".x", ".y"),
copy = FALSE, ...) {
  left_join(x, y, by = by, copy = copy, suffix = suffix, ...,
    keep = FALSE) # Keep = TRUE not implemented in merge.data.table()
  # na_matches = "na" is the default # NA and NaN are considered equivalent
}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::left_join"))

#' @export
#' @rdname tidy_functions
tright_join <- structure(function(x, y, by = NULL, suffix = c(".x", ".y"),
copy = FALSE, ...) {
  right_join(x, y, by = by, copy = copy, suffix = suffix, ...,
    keep = FALSE) # Keep = TRUE not implemented in merge.data.table()
  # na_matches = "na" is the default # NA and NaN are considered equivalent
}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::right_join"))

#' @export
#' @rdname tidy_functions
tinner_join <- structure(function(x, y, by = NULL, suffix = c(".x", ".y"),
copy = FALSE, ...) {
  inner_join(x, y, by = by, copy = copy, suffix = suffix, ...,
    keep = FALSE) # Keep = TRUE not implemented in merge.data.table()
  # na_matches = "na" is the default # NA and NaN are considered equivalent
}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::inner_join"))
