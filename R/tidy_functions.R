#' Tidy functions (mainly from dplyr and tidyr) to manipulate data frames
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' *These function are deprecated to the benefit of the functions whose name
#' ends with an underscore `_` (e.g., `select()` -> [svTidy::select_()]) in the
#' svTidy package*.
#'
#' The Tidyverse defines a coherent set of tools to manipulate
#' data frames that use a non-standard evaluation and sometimes require extra
#' care. These functions, like [dplyr::mutate()] or [dplyr::summarise()] are
#' defined in the \{dplyr\} and \{tidyr\} packages. When using variants, like
#' \{dtplyr\} for **data.frame** objects, or \{dbplyr\} to work with external
#' databases, successive commands in a pipeline are pooled together but not
#' computed. One has to [dplyr::collect()] the result to get its final form.
#' Most of the tidy functions that have their "speedy" counterpart prefixed with
#' "s" are listed with[list_tidy_functions()]. Their main usages are (excluding
#' less used arguments, or those that are not compatibles with the speedy "s"
#' counterpart functions):
#' - `group_by(.data, ...)`
#' - `ungroup(.data)`
#' - `rename(.data, ...)`
#' - `rename_with(.data, .fn, .cols = everything(), ...)`
#' - `filter(.data, ...)`
#' - `select(.data, ...)`
#' - `mutate(.data, ..., .keep = "all")`
#' - `transmute(.data, ...)`
#' - `summarise(.data, ...)`
#' - `full_join(x, y, by = NULL, suffix = c(".x", ".y"), copy = FALSE, ...)`
#' - `left_join(x, y, by = NULL, suffix = c(".x", ".y"), copy = FALSE, ...)`
#' - `right_join(x, y, by = NULL, suffix = c(".x", ".y"), copy = FALSE, ...)`
#' - `inner_join(x, y, by = NULL, suffix = c(".x", ".y"), copy = FALSE, ...)`
#' - `bind_rows(..., .id = NULL)`
#' - `bind_cols(..., .name_repair = c("unique", "universal", "check_unique", "minimal"))`
#' - `arrange(.data, ..., .by_group = FALSE)`
#' - `count(x, ..., wt = NULL, sort = FALSE, name = NULL)`
#' - `tally(x, wt = NULL, sort = FALSE, name = NULL)`
#' - `add_count(x, ..., wt = NULL, sort = FALSE, name = NULL)`
#' - `add_tally(x, wt = NULL, sort = FALSE, name = NULL)`
#' - `pull(.data, var = -1, name = NULL)`
#' - `distinct(.data, ..., .keep_all = FALSE)`
#' - `drop_na(data, ...)`
#' - `replace_na(data, replace)`
#' - `pivot_longer(data, cols, names_to = "name", values_to = "value")`
#' - `pivot_wider(data, names_from = name, values_from = value)`
#' - `uncount(data, weights, .remove = TRUE, .id = NULL)`
#' - `unite(data, col, ..., sep = "_", remove = TRUE, na.rm = FALSE)`
#' - `separate(data, col, into, sep = "[^[:alnum:]]+", remove = TRUE, convert = FALSE)`
#' - `separate_rows(data, ..., sep = "[^[:alnum:].]+", convert = FALSE)`
#' - `fill(data, ..., .direction = c("down", "up", "downup", "updown"))`
#' - `extract(data, col, into, regex = "([[:alnum:]]+)", remove = TRUE, convert = FALSE)`
#' plus the functions defined here under.

#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy
#' data frame (e.g. from dbplyr or dtplyr). See [dplyr::mutate()] for more
#' details.
#' @param ... Arguments dependent to the context of the function and most of
#'   the time, not evaluated in a standard way (cf. the tidyverse approach).
#' @param .keep Which columns to keep. The default is `"all"`, possible values
#' are `"used"`, `"unused"`, or `"none"` (see [dplyr::mutate()]).
#'
#' @note The help page here is very basic and it aims mainly to list all the
#' tidy functions. For more complete help, see the \{dplyr\} or \{tidyr\}
#' packages. From \{dplyr\}, the [slice()] and `slice_xxx()` functions are not
#' added yet because they are not available for \{dbplyr\}. Also
#' [dplyr::anti_join()], [dplyr::semi_join()] and [dplyr::nest_join()] are not
#' implemented yet. From \{dplyr\}, the [dplyr::slice()] and `slice_xxx()`
#' functions are not added yet because they are not available for \{dbplyr\}.
#' From \{tidyr\} [tidyr::expand()], [tidyr::chop()], [tidyr::unchop()],
#' [tidyr::nest()], [tidyr::unnest()], [tidyr::unnest_longer()],
#' [tidyr::unnest_wider()], [tidyr::hoist()], [tidyr::pack()] and
#' [tidyr::unpack()] are not implemented yet.
#'
#' @return See corresponding "non-t" function for the full help page with
#' indication of the return values. [list_tidy_functions()] returns a list of
#' all the tidy(verse) functions that have their speedy "s" counterpart, see
#' [speedy_functions].
#'
#' @seealso [collapse::num_vars()] to easily keep only numeric columns from a
#'   data frame, [collapse::fscale()] for scaling and centering matrix-like
#'   objects and data frames.
#'
#' @export
#' @name tidy_functions
#'
#' @examples
#' # TODO...
list_tidy_functions <- function() {
  lifecycle::deprecate_soft(when = "1.7.0",
    what = "list_tidy_functions()",
    with = I("corresponding functions in the svTidy package"))
  c("add_count", "add_tally", "arrange", "bind_cols", "bind_rows", "count",
    "distinct", "drop_na", "extract", "fill", "filter", "filter_ungroup",
    "full_join", "group_by", "inner_join", "left_join", "mutate",
    "mutate_ungroup", "pivot_longer", "pivot_wider", "pull", "rename",
    "rename_with", "replace_na", "right_join", "select", "separate",
    "separate_rows", "summarise", "tally", "transmute", "transmute_ungroup",
    "uncount", "ungroup", "unite")
}

#.src_tidy <- function(src, comment = "A tidy function, see ?tidy_functions.") {
#  attr(comment, "src") <- src
#  comment
#}

# #' @export
# #' @rdname tidy_functions
#tgroup_by <- structure(function(.data, ...) {
#  # fgroup_by() does not support .add= and behaves like .drop = TRUE -> force it
#  group_by(.data, ..., .add = FALSE, .drop = TRUE)
#}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::group_by"))

# #' @export
# #' @rdname tidy_functions
#tungroup <- structure(function(.data, ...) {
#  # ungroup() uses x, but it is .data= everywhere else => use .data
#  ungroup(.data, ...)
#}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::ungroup"))

# #' @export
# #' @rdname tidy_functions
#trename <- structure(dplyr::rename,
#  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::rename"))

# #' @export
# #' @rdname tidy_functions
#trename_with <- structure(dplyr::rename_with,
#  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::rename_with"))

# #' @export
# #' @rdname tidy_functions
#tfilter <- structure(function(.data, ...) {
#  # I don't understand .preserve = FALSE of dplyr::filter() -> not used for now
#  filter(.data, ...)
#}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::filter"))

#' @export
#' @rdname tidy_functions
filter_ungroup <- function(.data, ...) {
  lifecycle::deprecate_soft(when = "1.7.0",
    what = "filter_ungroup()",
    with = I("corresponding functions in the svTidy package"))
  ungroup(filter(.data, ...))
}
#tfilter_ungroup <- structure(function(.data, ...) {
#  ungroup(filter(.data, ...))
#}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::filter",
#  comment = "A tidy function, see ?tidy_functions, combining filter() and ungroup()."))

# #' @export
# #' @rdname tidy_functions
#tselect <- structure(dplyr::select,
#  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::select"))

# #' @export
# #' @rdname tidy_functions
## TODO: Arguments .before= and .after= not supported yet, but to be implemented
#tmutate <- structure(dplyr::mutate,
#  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::mutate",
#    comment = "A tidy function, see ?tidy_functions.\nThe .before= and .after= arguments are not #implemented yet."))

#' @export
#' @rdname tidy_functions
mutate_ungroup <- function(.data, ..., .keep = "all") {
  lifecycle::deprecate_soft(when = "1.7.0",
    what = "mutate_ungroup()",
    with = I("corresponding functions in the svTidy package"))
  ungroup(mutate(.data, ..., .keep = .keep))
}
#tmutate_ungroup <- structure(function(.data, ..., .keep = "all") {
#  ungroup(mutate(.data, ..., .keep = .keep))
#}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::mutate",
#  comment = "A tidy function, see ?tidy_functions, combining mutate() and ungroup()."))

# #' @export
# #' @rdname tidy_functions
#ttransmute <- structure(dplyr::transmute,
#  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::transmute"))

#' @export
#' @rdname tidy_functions
transmute_ungroup <- function(.data, ...) {
  lifecycle::deprecate_soft(when = "1.7.0",
    what = "transmute_ungroup()",
    with = I("corresponding functions in the svTidy package"))
  ungroup(transmute(.data, ...))
}
#ttransmute_ungroup <- structure(function(.data, ...) {
#  ungroup(transmute(.data, ...))
#}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::transmute",
#  comment = "A tidy function, see ?tidy_functions, combining transmute() and ungroup()."))

# #' @export
# #' @rdname tidy_functions
#tsummarise <- structure(function(.data, ...) {
#  # Apparently, fsummarise() behaves like dplyr:: summarise() < 1.0, thus using
#  # .groups = "drop_last' always
#  summarise(.data, ..., .groups = "drop_last")
#}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::summarise"))

# #' @export
# #' @rdname tidy_functions
#tfull_join <- structure(function(x, y, by = NULL, suffix = c(".x", ".y"),
#copy = FALSE, ...) {
#  full_join(x, y, by = by, copy = copy, suffix = suffix, ...,
#    keep = FALSE) # Keep = TRUE not implemented in merge.data.table()
#    # na_matches = "na" is the default # NA and NaN are considered equivalent
#}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::full_join"))

# #' @export
# #' @rdname tidy_functions
#tleft_join <- structure(function(x, y, by = NULL, suffix = c(".x", ".y"),
#copy = FALSE, ...) {
#  left_join(x, y, by = by, copy = copy, suffix = suffix, ...,
#    keep = FALSE) # Keep = TRUE not implemented in merge.data.table()
#  # na_matches = "na" is the default # NA and NaN are considered equivalent
#}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::left_join"))

# #' @export
# #' @rdname tidy_functions
#tright_join <- structure(function(x, y, by = NULL, suffix = c(".x", ".y"),
#copy = FALSE, ...) {
#  right_join(x, y, by = by, copy = copy, suffix = suffix, ...,
#    keep = FALSE) # Keep = TRUE not implemented in merge.data.table()
#  # na_matches = "na" is the default # NA and NaN are considered equivalent
#}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::right_join"))

# #' @export
# #' @rdname tidy_functions
#tinner_join <- structure(function(x, y, by = NULL, suffix = c(".x", ".y"),
#copy = FALSE, ...) {
#  inner_join(x, y, by = by, copy = copy, suffix = suffix, ...,
#    keep = FALSE) # Keep = TRUE not implemented in merge.data.table()
#  # na_matches = "na" is the default # NA and NaN are considered equivalent
#}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::inner_join"))

# #' @export
# #' @rdname tidy_functions
#tbind_rows <- structure(dplyr::bind_rows,
#  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::bind_rows"))

# #' @export
# #' @rdname tidy_functions
#tbind_cols <- structure(dplyr::bind_cols,
#  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::bind_cols"))

# #' @export
# #' @rdname tidy_functions
#tarrange <- structure(dplyr::arrange,
#  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::arrange"))

# #' @export
#@#' @rdname tidy_functions
# count <- structure(dplyr::count,
#  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::count"))

# #' @export
# #' @rdname tidy_functions
#ttally <- structure(dplyr::tally,
#  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::tally"))

# #' @export
# #' @rdname tidy_functions
#tadd_count <- structure(function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
#  add_count(x, ..., wt = wt, sort = sort, name = name) # .drop is deprecated
#}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::add_count"))

# #' @export
# #' @rdname tidy_functions
#tadd_tally <- structure(dplyr::add_tally,
#  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::add_tally"))

# #' @export
# #' @rdname tidy_functions
#tpull <- structure(dplyr::pull,
#  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::pull"))

# #' @export
# #' @rdname tidy_functions
#tdistinct <- structure(dplyr::distinct,
#  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::distinct"))


# tidyr verbs -------------------------------------------------------------

# #' @export
# #' @rdname tidy_functions
#tdrop_na <- structure(tidyr::drop_na,
#  class = c("function", "tidy_fn"), comment = .src_tidy("tidyr::drop_na"))

# #' @export
# #' @rdname tidy_functions
#treplace_na <- structure(tidyr::replace_na,
#  class = c("function", "tidy_fn"), comment = .src_tidy("tidyr::replace_na"))

# #' @export
# #' @rdname tidy_functions
#tpivot_longer <- structure(function(data, cols, names_to = "name",
#values_to = "values", ...) {
#  do.call(pivot_longer, list(data = data, cols = substitute(cols),
#    names_to = names_to, values_to = values_to, ...))
#},  class = c("function", "tidy_fn"), comment = .src_tidy("tidyr::pivot_longer"))

## This is needed for R CMD check otherwise itwill complain
#name <- NULL
#value <- NULL

# #' @export
# #' @rdname tidy_functions
#tpivot_wider <- structure(function(data, names_from = name,
#values_from = value, ...) {
#  do.call(pivot_wider, list(data = data, names_from = substitute(names_from),
#    values_from = substitute(values_from), ...))
#},  class = c("function", "tidy_fn"), comment = .src_tidy("tidyr::pivot_wider"))

# #' @export
# #' @rdname tidy_functions
#tuncount <- structure(tidyr::uncount,
#  class = c("function", "tidy_fn"), comment = .src_tidy("tidyr::uncount"))

# #' @export
# #' @rdname tidy_functions
#tunite <- structure(tidyr::unite,
#  class = c("function", "tidy_fn"), comment = .src_tidy("tidyr::unite"))

# #' @export
# #' @rdname tidy_functions
#tseparate <- structure(function(data, col, into, sep = "[^[:alnum:]]+",
#remove = TRUE, convert = FALSE, ...) {
#  do.call(separate, list(data, col = substitute(col), into = into, sep = sep,
#    remove = remove, convert = convert, ...))
#},  class = c("function", "tidy_fn"), comment = .src_tidy("tidyr::separate"))

# #' @export
# #' @rdname tidy_functions
#tseparate_rows <- structure(tidyr::separate_rows,
#  class = c("function", "tidy_fn"), comment = .src_tidy("tidyr::separate_rows"))

# #' @export
# #' @rdname tidy_functions
#tfill <- structure(tidyr::fill,
#  class = c("function", "tidy_fn"), comment = .src_tidy("tidyr::fill"))

# #' @export
# #' @rdname tidy_functions
#textract <- structure(tidyr::extract,
#  class = c("function", "tidy_fn"), comment = .src_tidy("tidyr::extract"))
