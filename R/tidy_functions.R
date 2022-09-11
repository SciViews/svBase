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
#' @param .id The name of the column for the origin id, either names if all
#' other arguments are named, or numbers.
#' @param .name_repair How should the name be "repaired" to avoid duplicate
#' column names? See [dplyr::bind_cols()] for more details.
#' @param .by_group Logical. If `TRUE` rows are first arranger by the grouping
#' variables in any. `FALSE` by default.
#' @param wt Frequency weights. Can be `NULL` or a variable. Use data masking.
#' @param sort If `TRUE` largest group will be shown on top.
#' @param name The name of the new column in the output (`n` by default, and no
#' existing column must have this name, or an error is generated).
#' @param var A variable specified as a name, a positive or a negative integer
#'   (counting from the end). The default is `-1` and returns last variable.
#' @param .keep_all If `TRUE` keep all variables in `.data`.
#' @param data A data frame, or for `replace_na()` a vector or a data frame.
#' @param replace If `data` is a vector, a unique value to replace `NA`s, otherwise, a list of values, one per column of the data frame.
#' @param cols A selection of the columns using tidy-select syntax, see[tidyr::pivot_longer()].
#' @param names_to A character vector with the name or names of the columns for the names.
#' @param values_to A string with the name of the column that receives the values.
#' @param names_from The column or columns containing the names (use tidy selection and do not quote the names).
#' @param values_from Idem for the column or columns that contain the values.
#' @param weights A vector of weight to use to "uncount" `data`.
#' @param .remove If `TRUE`, and `weights` is the name of a column, that column
#'   is removed from `data`.
#' @param col The name quoted or not of the new column with united variable.
#' @param sep Separator to use between values for united column.
#' @param remove If 'TRUE' the columns used to unite are removed.
#' @param na.rm If `TRUE`, `NA`s are eliminated before uniting the values.
#' @param into Name of the new column to put separated variables. Use `NA` for
#'   items to drop.
#' @param remove If `TRUE` the initial columns that are separated are also
#'   removed from `data`.
#' @param convert If `'TRUE` resulting values are converted into numeric,
#'   integer or logical.
#' @param .direction Direction in which to fill missing data: `"down"` (by
#'   default), `"up"`, or `"downup"` (first down, then up), `"updown"`
#'   (the opposite).
#' @param regex A regular expression used to extract the desired values (use one
#'   group with `(` and `)` for each element of `into`).
#'
#' @note The help page here is very basic and it aims mainly to list all the
#' tidy functions. For more complete help, see their "non-t" counterparts in
#' {dplyr} or {tidyr}, or use the {svMisc}'s `.?tmutate` syntax to link to the
#' correct page.
#' #' From {dplyr}, the [slice()] and `slice_xxx()` functions are not added yet
#' because they are not available for {dbplyr}. Also [anti_join()],
#' [semi_join()] and [nest_join()] are not implemented yet.
#' From {dplyr}, the [slice()] and `slice_xxx()` functions are not added yet
#' because they are not available for {dbplyr}. Also [anti_join()],
#' [semi_join()] and [nest_join()] are not implemented yet.
#' From {tidyr} [expand()], [chop()], [unchop()], [nest()], [unnest()],
#' [unnest_longer()], [unnest_wider()], [hoist()], [pack()] and [unpack()] are
#' not implemented yet.
#'
#' @return See corresponding "non-t" function for the full help page with
#' indication of the return values.
#'
#' @seealso [collapse::num_vars()] to easily keep only numeric columns from a data frame, [collapse::fscale()] for scaling and centering matrix-like objects and data frames.
#'
#' @export
#'
#' @examples
#' # TODO...
tidy_functions <- function() {
  c("tadd_count", "tadd_tally", "tarrange", "tbind_cols", "tbind_rows",
    "tcount", "tdistinct", "tdrop_na", "textract", "tfill", "tfilter",
    "tfilter_ungroup", "tfull_join", "tgroup_by", "tinner_join", "tleft_join",
    "tmutate", "tmutate_ungroup", "tpivot_longer", "tpivot_wider", "tpull",
    "trename", "trename_with", "treplace_na", "tright_join", "tselect",
    "tseparate", "tseparate_rows", "tsummarise", "ttally", "ttransmute",
    "ttransmute_ungroup", "tuncount", "tungroup", "tunite")
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
  summarise(.data, ..., .groups = "drop_last")
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

#' @export
#' @rdname tidy_functions
tbind_rows <- structure(dplyr::bind_rows,
  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::bind_rows"))

#' @export
#' @rdname tidy_functions
tbind_cols <- structure(dplyr::bind_cols,
  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::bind_cols"))

#' @export
#' @rdname tidy_functions
tarrange <- structure(dplyr::arrange,
  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::arrange"))

#' @export
#' @rdname tidy_functions
tcount <- structure(dplyr::count,
  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::count"))

#' @export
#' @rdname tidy_functions
ttally <- structure(dplyr::tally,
  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::tally"))

#' @export
#' @rdname tidy_functions
tadd_count <- structure(function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
  add_count(x, ..., wt = wt, sort = sort, name = name) # .drop is deprecated
}, class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::add_count"))

#' @export
#' @rdname tidy_functions
tadd_tally <- structure(dplyr::add_tally,
  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::add_tally"))

#' @export
#' @rdname tidy_functions
tpull <- structure(dplyr::pull,
  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::pull"))

#' @export
#' @rdname tidy_functions
tdistinct <- structure(dplyr::distinct,
  class = c("function", "tidy_fn"), comment = .src_tidy("dplyr::distinct"))


# tidyr verbs -------------------------------------------------------------

#' @export
#' @rdname tidy_functions
tdrop_na <- structure(tidyr::drop_na,
  class = c("function", "tidy_fn"), comment = .src_tidy("tidyr::drop_na"))

#' @export
#' @rdname tidy_functions
treplace_na <- structure(tidyr::replace_na,
  class = c("function", "tidy_fn"), comment = .src_tidy("tidyr::replace_na"))

#' @export
#' @rdname tidy_functions
tpivot_longer <- structure(function(data, cols, names_to = "name",
values_to = "values", ...) {
  do.call(pivot_longer, list(data = data, cols = substitute(cols),
    names_to = names_to, values_to = values_to, ...))
},  class = c("function", "tidy_fn"), comment = .src_tidy("tidyr::pivot_longer"))

# This is needed for R CMD check otherwise itwill complain
name <- NULL
value <- NULL

#' @export
#' @rdname tidy_functions
tpivot_wider <- structure(function(data, names_from = name,
values_from = value, ...) {
  do.call(pivot_wider, list(data = data, names_from = substitute(names_from),
    values_from = substitute(values_from), ...))
},  class = c("function", "tidy_fn"), comment = .src_tidy("tidyr::pivot_wider"))

#' @export
#' @rdname tidy_functions
tuncount <- structure(tidyr::uncount,
  class = c("function", "tidy_fn"), comment = .src_tidy("tidyr::uncount"))

#' @export
#' @rdname tidy_functions
tunite <- structure(tidyr::unite,
  class = c("function", "tidy_fn"), comment = .src_tidy("tidyr::unite"))

#' @export
#' @rdname tidy_functions
tseparate <- structure(function(data, col, into, sep = "[^[:alnum:]]+",
remove = TRUE, convert = FALSE, ...) {
  do.call(separate, list(data, col = substitute(col), into = into, sep = sep,
    remove = remove, convert = convert, ...))
},  class = c("function", "tidy_fn"), comment = .src_tidy("tidyr::separate"))

#' @export
#' @rdname tidy_functions
tseparate_rows <- structure(tidyr::separate_rows,
  class = c("function", "tidy_fn"), comment = .src_tidy("tidyr::separate_rows"))

#' @export
#' @rdname tidy_functions
tfill <- structure(tidyr::fill,
  class = c("function", "tidy_fn"), comment = .src_tidy("tidyr::fill"))

#' @export
#' @rdname tidy_functions
textract <- structure(tidyr::extract,
  class = c("function", "tidy_fn"), comment = .src_tidy("tidyr::extract"))
