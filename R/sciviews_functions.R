#' SciViews functions (mainly from collapse and data.table) to manipulate data frames
#'
#' @description A SciViews::R version of the tidyverse functions in \{dplyr\}
#' and \{tidyr\} with standard evaluation, and non-standard evaluation trough
#' formulas. These functions end with an underscore `_`. Avoid mixing tidy,
#' speedy and SciViews functions in the same pipeline.
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
#' @param .id The name of the column for the origin id, either names if all
#' other arguments are named, or numbers.
#' @param .name_repair How should the name be "repaired" to avoid duplicate
#' column names? See [dplyr::bind_cols()] for more details.
#' @param .by_group Logical. If `TRUE` rows are first arranger by the grouping
#' variables in any. `FALSE` by default.
#' @param wt Frequency weights. Can be `NULL` or a variable. Use data masking.
#' @param sort If `TRUE` largest group will be shown on top.
#' @param .drop Are levels with no observations dropped (`TRUE` by default).
#' @param sort_cat Are levels sorted (`TRUE` by default).
#' @param decreasing Is sorting done in decreasing order (`FALSE` by default)?
#' @param name The name of the new column in the output (`n` by default, and no
#' existing column must have this name, or an error is generated).4
#' @param var A variable specified as a name, a positive or a negative integer
#'   (counting from the end). The default is `-1` and returns last variable.
#' @param .keep_all If `TRUE` keep all variables in `.data`.
#' @param data A data frame, or for `replace_na()` a vector or a data frame.
#' @param replace If `data` is a vector, a unique value to replace `NA`s,
#' otherwise, a list of values, one per column of the data frame.
#' @param cols A selection of the columns using tidy-select syntax,
#'   see[tidyr::pivot_longer()].
#' @param names_to A character vector with the name or names of the columns for
#'   the names.
#' @param values_to A string with the name of the column that receives the
#'   values.
#' @param names_from The column or columns containing the names (use tidy
#'   selection and do not quote the names).
#' @param values_from Idem for the column or columns that contain the values.
#' @param weights A vector of weight to use to "uncount" `data`.
#' @param .remove If `TRUE`, and `weights` is the name of a column, that column
#'   is removed from `data`.
#' @param col The name quoted or not of the new column with united variable.
#' @param sep Separator to use between values for united or separated columns.
#' @param remove If 'TRUE' the columns used to unite are removed.
#' @param na.rm If `TRUE`, `NA`s are eliminated before uniting the values.
#' @param into Name of the new column to put separated variables. Use `NA` for
#'   items to drop.
#' @param remove If `TRUE` the initial columns that are separated are also
#'   removed from `data`.
#' @param convert If `'TRUE` resulting values are converted into numeric,
#' integer or logical.
#' @param .direction Direction in which to fill missing data: `"down"` (by
#'   default), `"up"`, or `"downup"` (first down, then up), `"updown"`
#'   (the opposite).
#' @param regex A regular expression used to extract the desired values (use one
#'   group with `(` and `)` for each element of `into`).
#'
#' @note The [ssummarise()] function does not support `n()` as does
#' [dplyr::summarise()]. You can use [fn()] instead, but then, you must give a
#' variable name as argument. The [fn()] alternative can also be used in
#' [summarise()] for homogeneous syntax between the two.
#' From \{dplyr\}, the [slice()] and `slice_xxx()` functions are not added yet
#' because they are not available for \{dbplyr\}. Also [anti_join()],
#' [semi_join()] and [nest_join()] are not implemented yet.
#' From \{tidyr\} [expand()], [chop()], [unchop()], [nest()], [unnest()],
#' [unnest_longer()], [unnest_wider()], [hoist()], [pack()] and [unpack()] are
#' not implemented yet.
#'
#' @return See corresponding "non-SciViews" function for the full help page with
#' indication of the return values.
#'
#' @export
#' @name sciviews_functions
#'
#' @examples
#' # TODO...
list_sciviews_functions <- function() {
  c("add_count_", "add_tally_", "arrange_", "bind_cols_", "bind_rows_",
    "count_", "distinct_", "drop_na_", "extract_", "fill_", "filter_",
    "filter_ungroup_", "full_join_", "group_by_", "inner_join_", "left_join_",
    "mutate_", "mutate_ungroup_", "pivot_longer_", "pivot_wider_", "pull_",
    "rename_", "rename_with_", "replace_na_", "right_join_", "select_",
    "separate_", "separate_rows_", "summarise_", "tally_", "transmute_",
    "transmute_ungroup_", "uncount_", "ungroup_", "unite_")
}

.src_sciviews <- function(src,
  comment = "A SciViews function, see ?sciviews_functions.") {
  attr(comment, "src") <- src
  comment
}

#' @export
#' @rdname sciviews_functions
group_by_ <- structure(function(.data, ...) {
  # Different args names than fgroup_by()
  # and not all arguments not in group_by()
  # TODO: accept something like this: sgroup_by(df, across(...)).
  fgroup_by(.data, ...)
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fgroup_by"))

#' @export
#' @rdname sciviews_functions
ungroup_ <- structure(function(data = (.), ...) {
  check_dots_empty0(...) # ... must be empty
  if (!is.data.frame(data)) {
    # Try to inject data = . as first argument and recall
    with_data_dot <- call_inject_first_arg(sys.call())
    if (!is.null(with_data_dot))
      return(eval_bare(with_data_dot, parent.frame()))
    # or...
    abort("Argument 'data' must be a 'data.frame'.")
  }
  fungroup(data)
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fungroup"))

#' @export
#' @rdname sciviews_functions
rename_ <- structure(function(.data, ...) {
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
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("dplyr::rename"))

everything_ <- function() {
  stop("This should not be called directly, see ?mutate_with.")
}

#' @export
#' @rdname sciviews_functions
rename_with_ <- structure(function(.data, .fn, .cols = everything(), ...) {
  # The equivalent of rename_with(),
  # but currently supporting only .cols = everything()

  # For now, only manage the case of .cols = everything()
  if (deparse(substitute(.cols)) != "everything()")
    stop("Only supporting .cols = everything() for now, sorry.")
  if (!is.function(.fn))
    stop("Can only rename with a function for now, sorry.")
  frename(.data, .fn, ...) # cols= missing to apply to all columns
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::rename_with"))

#' @export
#' @rdname sciviews_functions
filter_ <- structure(function(data = (.), ...) {
  if (!is.data.frame(data)) {
    # Try to inject data = . as first argument and recall
    with_data_dot <- call_inject_first_arg(sys.call())
    if (!is.null(with_data_dot))
      return(eval_bare(with_data_dot, parent.frame()))
    # or...
    abort("Argument 'data' must be a 'data.frame'.")
  }

  filters <- match.call()[-(1:2)]
  # fsubset() can use only one subset argument at a time. So, we run it
  # multiple times on each argument to filter_() to mimic filter()
  for (i in 1:...length()) {
    filter <- filters[[i]]
    if (is_formula(filter)) {
      if (!is_formula(filter, lhs = FALSE)) # Check for lhs
        abort("Argument 'i' must be a formula with no lhs")
      data <- do.call('fsubset', list(.x = data, f_rhs(filter)))
    } else {# Not a formula -> should be an index
      # TODO: proper error message in case it is not a valid index
      data <- data[filter, , drop = FALSE]
    }
  }
  data
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("dplyr::filter"))

#' @export
#' @rdname sciviews_functions
filter_ungroup_ <- structure(function(data = (.), ...) {
  if (!is.data.frame(data)) {
    # Try to inject data = . as first argument and recall
    with_data_dot <- call_inject_first_arg(sys.call())
    if (!is.null(with_data_dot))
      return(eval_bare(with_data_dot, parent.frame()))
    # or...
    abort("Argument 'data' must be a 'data.frame'.")
  }

  fungroup(filter_(data = data, ...))
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("dplyr::filter",
  comment = "A SciViews function, see ?sciviews_functions, combining filter() and ungroup()."))

#' @export
#' @rdname sciviews_functions
select_ <- structure(function(data, ...) {
  if (!is.data.frame(data)) {
    # Try to inject data = . as first argument and recall
    with_data_dot <- call_inject_first_arg(sys.call())
    if (!is.null(with_data_dot))
      return(eval_bare(with_data_dot, parent.frame()))
    # or...
    abort("Argument 'data' must be a 'data.frame'.")
  }

  fselect(data, ..., return = "data") # Other return modes not supported (yet)
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fselect"))

#' @export
#' @rdname sciviews_functions
mutate_ <- structure(function(.data, ..., .keep = "all") {
    fmutate(.data, ..., .keep = .keep)
  },
  # TODO: Arguments .by, .before= and .after= not supported yet, but to be
  # implemented
  class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fmutate"))

#' @export
#' @rdname sciviews_functions
mutate_ungroup_ <- structure(function(.data, ..., .keep = "all") {
  fungroup(fmutate(.data, ..., .keep = .keep))
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("dplyr::mutate",
  comment = "A SciViews function, see ?sciviews_functions, combining mutate() and ungroup()."))

#' @export
#' @rdname sciviews_functions
transmute_ <- structure(function(.data, ...) {
  fmutate(.data, ...)[, ...names()]
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::transmute"))

#' @export
#' @rdname sciviews_functions
transmute_ungroup_ <- structure(function(.data, ...) {
  fungroup(fmutate(.data, ...)[, ...names()])
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("dplyr::transmute",
  comment = "A SciViews function, see ?sciviews_functions, combining transmute() and ungroup()."))

#' @export
#' @rdname sciviews_functions
summarise_ <- structure(function(.data, ...) {
  # keep.group_vars = FALSE not in dplyr::summarise()
  fsummarise(.data, ..., keep.group_vars = TRUE)
  # TODO: implement .by and align arguments
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fsummarise"))

#' @export
#' @rdname sciviews_functions
full_join_ <- structure(function(x, y, by = NULL, suffix = c(".x", ".y"),
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
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::full_join"))

#' @export
#' @rdname sciviews_functions
left_join_ <- structure(function(x, y, by = NULL, suffix = c(".x", ".y"),
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
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::left_join"))

#' @export
#' @rdname sciviews_functions
right_join_ <- structure(function(x, y, by = NULL, suffix = c(".x", ".y"),
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
    res <-  as_dtf(res)
  if (is_x_dtbl)
    res <- as_dtbl(res)
  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::right_join"))

#' @export
#' @rdname sciviews_functions
inner_join_ <- structure(function(x, y, by = NULL, suffix = c(".x", ".y"),
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
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::inner_join"))

#' @export
#' @rdname sciviews_functions
bind_rows_ <- structure(function(..., .id = NULL) {
  if (!is.null(.id) && (length(.id) != 1 || !is.character(.id)))
    stop("`.id` must be a scalar string")
  # We transform check the class of first argument to return something similar
  is_x_dtf <- is_dtf(..1)
  is_x_dtbl <- is_dtbl(..1)
  list... <- list(...)
  # If there is at least one non-names item, bind_rows() does not use labels,
  # but rbindlist() does with "" where there is no name. Homogenise the behavior
  # by eliminating all names if at least one is ""
  if (any(...names() == ""))
    names(list...) <- NULL
  # dplyr::bind_rows() does the job more intelligently than base::rbind(): it
  # matches column names and fill missing data where needed. rbindlist() can do
  # both, but same behavior is obtained with use.names = TRUE + fill = TRUE
  res <- rbindlist(list..., use.names = TRUE, fill = TRUE, idcol = .id)
  # bind_rows() always returns characters for .id, but rbindlist() sometimes
  # returns integers
  if (!is.null(.id))
    res[[.id]] <- as.character(res[[.id]])
  # Transform if needed
  if (is_x_dtf)
    res <- as_dtf(res)
  if (is_x_dtbl)
    res <- as_dtbl(res)
  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::bind_rows"))

#' @export
#' @rdname sciviews_functions
count_ <- structure(function(x, ..., wt = NULL, sort = FALSE, name = NULL,
.drop = dplyr::group_by_drop_default(x), sort_cat = TRUE, decreasing = FALSE) {
  # TODO: .drop = FALSE not implemented yet
  if (isFALSE(.drop))
    stop(".drop = FALSE not implemented yet in scount(), use count() instead")
  # TODO: this does not work yet -> send an error message
  # starwars %>% scount(birth_decade = round(birth_year, -1))
  check <- try(rlang::check_dots_unnamed(), silent = TRUE)
  if (inherits(check, "try-error"))
    stop("scount() does not use computed values for ... yet, use count() instead")
  # TODO: align arguments with dplyr::count, currently, it is .drop = TRUE
  # but must implement .drop = FALSE too (shows 0 for levels that have no cases)
  # TODO: allow pronouns .data and .env
  if (is.null(name))
    name <- "n" # Default value is N in collapse, but n in dplyr
  # In case there are no groups defined, return just the number of rows
  if (!...length() && is.null(attr(x, "groups"))) {
    if (!missing(wt)) {
      swt <- substitute(wt)
      if (is.symbol(swt)) {
        res <- data.frame(n = sum(x[[as.character(swt)]], na.rm = TRUE))
      } else {
        if (length(wt) != NROW(x))
          stop("'wt' must be same length as the number of rows in 'x', or the name of a column in 'x'")
        res <- data.frame(n = sum(wt, na.rm = TRUE))
      }
    } else {
      res <- data.frame(n = NROW(x))
    }
    names(res) <- name
    return(default_dtx(res))
  }
  if (is.symbol(substitute(wt))) {
    res <- inject(fcount(x, ..., w = !!substitute(wt), sort = sort_cat,
      name = name, decreasing = decreasing, add = FALSE))
  } else {
    res <- fcount(x, ..., w = wt, sort = sort_cat,
      name = name, decreasing = decreasing, add = FALSE)
  }
  # sort= argument of dplyr::count sorts the frequency column indeed, not the
  # category column(s)
  if (isTRUE(sort))
    res <- res[order(res[[name]], decreasing = TRUE), ]
    # TODO: use data.table::setorder() instead
  default_dtx(res)
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fcount"))

#' @export
#' @rdname sciviews_functions
tally_ <- structure(function(x, wt = NULL, sort = FALSE, name = NULL,
sort_cat = TRUE, decreasing = FALSE) {
  # Same as scount(), but without ...; grouping must be done with sgroup_by()
  if (is.null(name))
    name <- "n" # Default value is N in collapse, but n in dplyr
  # In case there are no groups defined, return just the number of rows
  if (is.null(attr(x, "groups"))) {
    if (!missing(wt)) {
      swt <- substitute(wt)
      if (is.symbol(swt)) {
        res <- data.frame(n = sum(x[[as.character(swt)]], na.rm = TRUE))
      } else {
        if (length(wt) != NROW(x))
          stop("'wt' must be same length as the number of rows in 'x', or the name of a column in 'x'")
        res <- data.frame(n = sum(wt, na.rm = TRUE))
      }
    } else {
      res <- data.frame(n = NROW(x))
    }
    names(res) <- name
    return(default_dtx(res))
  }
  if (is.symbol(substitute(wt))) {
    res <- inject(fcount(x, w = !!substitute(wt), sort = sort_cat, name = name,
      decreasing = decreasing, add = FALSE))
  } else {
    res <- fcount(x, w = wt, sort = sort_cat, name = name,
      decreasing = decreasing, add = FALSE)
  }
  # sort= argument of dplyr::tally()sorts the frequency column indeed, not the
  # category column(s)
  if (isTRUE(sort))
    res <- res[order(res[[name]], decreasing = TRUE), ]
  # TODO: use data.table::setorder() instead
  default_dtx(res)
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fcount"))

#' @export
#' @rdname sciviews_functions
add_count_ <- structure(function(x, ..., wt = NULL, sort = FALSE, name = NULL,
  .drop = NULL, sort_cat = TRUE, decreasing = FALSE) {
  if (!missing(.drop))
    warning("the .drop= argument is deprecated in (s)add_count()")
  # TODO: this does not work yet -> send an error message
  # starwars %>% scount(birth_decade = round(birth_year, -1))
  check <- try(rlang::check_dots_unnamed(), silent = TRUE)
  if (inherits(check, "try-error"))
    stop("sadd_count() does not use computed values for ... yet, use add_count() instead")
  # TODO: align arguments with dplyr::count, currently, it is .drop = TRUE
  # but must implement .drop = FALSE too (shows 0 for levels that have no cases)
  # TODO: allow pronouns .data and .env
  if (is.null(name))
    name <- "n" # Default value is N in collapse, but n in dplyr
  # In case there are no groups defined, return just the number of rows
  if (!...length() && is.null(attr(x, "groups"))) {
    n <- numeric(0)
    if (!missing(wt)) {
      swt <- substitute(wt)
      if (is.symbol(swt)) {
        x[[name]] <- sum(x[[as.character(swt)]], na.rm = TRUE)
      } else {
        if (length(wt) != NROW(x))
          stop("'wt' must be same length as the number of rows in 'x', or the name of a column in 'x'")
        x[[name]] <- sum(wt, na.rm = TRUE)
      }
    } else {
      x[[name]] <- NROW(x)
    }
    return(default_dtx(x))
  }
  if (is.symbol(substitute(wt))) {
    res <- inject(fcount(x, ..., w = !!substitute(wt), sort = sort_cat,
      name = name, decreasing = decreasing, add = TRUE))
  } else {
    res <- fcount(x, ..., w = wt, sort = sort_cat,
      name = name, decreasing = decreasing, add = TRUE)
  }
  # sort= argument of dplyr::add_count sorts the frequency column indeed, not
  # the category column(s)
  if (isTRUE(sort))
    res <- res[order(res[[name]], decreasing = TRUE), ]
  # TODO: use data.table::setorder()
  default_dtx(res)
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fcount"))

#' @export
#' @rdname sciviews_functions
add_tally_ <- structure(function(x, wt = NULL, sort = FALSE, name = NULL,
sort_cat = TRUE, decreasing = FALSE) {
  # TODO: align arguments with dplyr::count, currently, it is .drop = TRUE
  # but must implement .drop = FALSE too (shows 0 for levels that have no cases)
  # TODO: allow pronouns .data and .env
  if (is.null(name))
    name <- "n" # Default value is N in collapse, but n in dplyr
  # In case there are no groups defined, return just the number of rows
  if (is.null(attr(x, "groups"))) {
    n <- numeric(0)
    if (!missing(wt)) {
      swt <- substitute(wt)
      if (is.symbol(swt)) {
        x[[name]] <- sum(x[[as.character(swt)]], na.rm = TRUE)
      } else {
        if (length(wt) != NROW(x))
          stop("'wt' must be same length as the number of rows in 'x', or the name of a column in 'x'")
        x[[name]] <- sum(wt, na.rm = TRUE)
      }
    } else {
      x[[name]] <- NROW(x)
    }
    return(default_dtx(x))
  }
  if (is.symbol(substitute(wt))) {
    res <- inject(fcount(x, w = !!substitute(wt), sort = sort_cat,
      name = name, decreasing = decreasing, add = TRUE))
  } else {
    res <- fcount(x, w = wt, sort = sort_cat,
      name = name, decreasing = decreasing, add = TRUE)
  }
  # sort= argument of dplyr::add_tally sorts the frequency column indeed, not
  # the category column(s)
  if (isTRUE(sort))
    res <- res[order(res[[name]], decreasing = TRUE), ]
  # TODO: use data.table::setorder() instead
  default_dtx(res)
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fcount"))


# Verbs that are not reengineered yet -------------------------------------

#' @export
#' @rdname sciviews_functions
bind_cols_ <- structure(function(...,
.name_repair = c("unique", "universal", "check_unique", "minimal")) {
  # For now, we use same function as tbind_cols() internally, but we convert
  # into the correct data frame object at the end

  # We transform check the class of first argument to return something similar
  x <- ungroup(..1)
  is_x_dtf <- is_dtf(x)
  is_x_dtt <- is_dtt(x)
  res <- bind_cols(..., .name_repair = .name_repair)
  # Transform if needed
  if (is_x_dtf)
    res <- as_dtf(res)
  if (is_x_dtt)
    res <- as_dtt(res)
  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::bind_cols"))

#' @export
#' @rdname sciviews_functions
arrange_ <- structure(function(.data, ..., .by_group = FALSE) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(.data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a sciviews one.")

  is_x_dtf <- is_dtf(.data)
  is_x_dtt <- is_dtt(.data)
  # Also if we have a GRP_by object from fgroup_by() or sgroup_by(), transform
  # it in,to regular group_by and restore the GRP_by after.
  if (inherits(.data, "GRP_df")) {
    is_x_grp_df <- TRUE
    gvars <- fgroup_vars(.data, return = "names")
    gvars <- lapply(gvars, as.name)
    # Must regroup with the regular dplyr::group_by()
    #.data <- group_by(fungroup(.data), across(gvars)) # Warning message
    .data <- fungroup(.data)
    .data <- do.call(group_by, c(list(.data = .data), gvars))
  } else {
    is_x_grp_df <- FALSE
  }
  res <- arrange(.data, ..., .by_group = .by_group)
  if (!is.data.frame(res))
    res <- collect(res)
  # Transform if needed
  if (is_x_dtf)
    res <- as_dtf(res)
  if (is_x_dtt)
    res <- as_dtt(res)
  if (is_x_grp_df)
    res <- do.call(fgroup_by, c(list(.X = res), gvars))
  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("dplyr::arrange"))

#' @export
#' @rdname sciviews_functions
pull_ <- structure(function(.data, var = -1, name = NULL, ...) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(.data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a sciviews one.")

  do.call(pull, list(.data = .data, var = substitute(var),
    name = substitute(name), ...))
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("dplyr::pull"))

#' @export
#' @rdname sciviews_functions
distinct_ <- structure(function(.data, ..., .keep_all = FALSE) {
  # For now, we use same function as txxx() counterpart... still must rework
  # Can use collapse::funique() by transforming the variables into a vector of
  # names, dropping .data$ and if .env$... is used, add it as .env$... in the
  # data frame, then use funique(), then drop or not unused variables
  if (inherits(.data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a sciviews one.")

  is_x_dtf <- is_dtf(.data)
  is_x_dtt <- is_dtt(.data)
  # Also if we have a GRP_by object from fgroup_by() or sgroup_by(), transform
  # it in,to regular group_by and restore the GRP_by after.
  if (inherits(.data, "GRP_df")) {
    is_x_grp_df <- TRUE
    gvars <- fgroup_vars(.data, return = "names")
    gvars <- lapply(gvars, as.name)
    # Must regroup with the regular dplyr::group_by()
    #.data <- group_by(fungroup(.data), across(gvars)) # Warning message
    .data <- fungroup(.data)
    .data <- do.call(group_by, c(list(.data = .data), gvars))
  } else {
    is_x_grp_df <- FALSE
  }
  res <- distinct(.data, ..., .keep_all = .keep_all)
  if (!is.data.frame(res))
    res <- collect(res)
  # Transform if needed
  if (is_x_dtf)
    res <- as_dtf(res)
  if (is_x_dtt)
    res <- as_dtt(res)
  if (is_x_grp_df)
    res <- do.call(fgroup_by, c(list(.X = res), gvars))
  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("dplyr::distinct"))


# tidyr verbs -------------------------------------------------------------

#' @export
#' @rdname sciviews_functions
drop_na_ <- structure(function(data, ...) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a sciviews one.")

  if (inherits(data, "GRP_df")) {
    is_x_grp_df <- TRUE
    gvars <- fgroup_vars(data, return = "names")
    gvars <- lapply(gvars, as.name)
  } else {
    is_x_grp_df <- FALSE
  }
  if (is_dtt(data)) {
    res <- collect(drop_na(data, ...))
    res <- as_dtt(res)
  } else {
    res <- drop_na(data, ...)
  }
  if (is_x_grp_df)
    res <- do.call(fgroup_by, c(list(.X = res), gvars))
  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("tidyr::drop_na"))

#' @export
#' @rdname sciviews_functions
replace_na_ <- structure(function(data, replace, ...) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a sciviews one.")

  if (inherits(data, "GRP_df")) {
    is_x_grp_df <- TRUE
    gvars <- fgroup_vars(data, return = "names")
    gvars <- lapply(gvars, as.name)
  } else {
    is_x_grp_df <- FALSE
  }
  if (is_dtt(data)) {
    res <- collect(replace_na(as_dtbl(data), replace, ...))
    res <- as_dtt(res)
  } else {
    res <- replace_na(data, replace, ...)
  }
  if (is_x_grp_df)
    res <- do.call(fgroup_by, c(list(.X = res), gvars))
  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("tidyr::replace_na"))

#' @export
#' @rdname sciviews_functions
pivot_longer_ <- structure(function(data, cols, names_to = "name",
values_to = "value", ...) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a sciviews one.")

  # Object is always ungrouped => we don't care of the groups!
  is_x_dtf <- is_dtf(data)
  is_x_dtt <- is_dtt(data)
  res <-  do.call(pivot_longer, list(data = data, cols = substitute(cols),
    names_to = names_to, values_to = values_to, ...))
  if (is_x_dtf)
    res <- as_dtf(res)
  if (is_x_dtt)
    res <- as_dtt(collect(res))
  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("tidyr::pivot_longer"))

# This is needed for R CMD check otherwise itwill complain
name <- NULL
value <- NULL

#' @export
#' @rdname sciviews_functions
pivot_wider_ <- structure(function(data, names_from = name,
  values_from = value, ...) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a sciviews one.")

  # Sometimes groups are kept, sometimes not... for now, we do not care.
  is_x_dtf <- is_dtf(data)
  is_x_dtt <- is_dtt(data)
  res <- do.call(pivot_wider, list(data = fungroup(data),
    names_from = substitute(names_from),
    values_from = substitute(values_from), ...))
  if (is_x_dtf)
    res <- as_dtf(res)
  if (is_x_dtt)
    res <- as_dtt(collect(res))
  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("tidyr::pivot_wider"))

#' @export
#' @rdname sciviews_functions
uncount_ <- structure(function(data, weights, .remove = TRUE, .id = NULL) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a sciviews one.")

  # Sometimes groups are kept, sometimes not... for now, we do not care
  # (we always ungroup).
  is_x_dtf <- is_dtf(data)
  is_x_dtt <- is_dtt(data)
  res <- do.call(uncount, list(data = fungroup(data),
    weights = substitute(weights), .remove = .remove, .id = .id))
  if (is_x_dtf)
    res <- as_dtf(res)
  if (is_x_dtt)
    res <- as_dtt(collect(res))
  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("tidyr::uncount"))

#' @export
#' @rdname sciviews_functions
unite_ <- structure(function(data, col, ..., sep = "_", remove = TRUE,
na.rm = FALSE) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a sciviews one.")

  # Sometimes groups are kept, sometimes not... for now, we do not care
  # (we always ungroup).
  is_x_dtf <- is_dtf(data)
  is_x_dtt <- is_dtt(data)
  res <- inject(unite(data = as_dtbl(fungroup(data)), col = !!substitute(col),
    ..., sep = sep, remove = remove, na.rm = na.rm))
  if (is_x_dtf)
    res <- as_dtf(res)
  if (is_x_dtt)
    res <- as_dtt(collect(res))
  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("tidyr::unite"))

#' @export
#' @rdname sciviews_functions
separate_ <- structure(function(data, col, into, sep = "[^[:alnum:]]+",
remove = TRUE, convert = FALSE, ...) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a sciviews one.")

  # Sometimes groups are kept, sometimes not... for now, we do not care
  # (we always ungroup).
  is_x_dtf <- is_dtf(data)
  is_x_dtt <- is_dtt(data)
  res <- do.call(separate, list(data = as_dtbl(fungroup(data)),
    col = substitute(col), into = into, sep = sep, remove = remove,
    convert = convert, ...))
  if (is_x_dtf)
    res <- as_dtf(res)
  if (is_x_dtt)
    res <- as_dtt(collect(res))
  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("tidyr::separate"))

#' @export
#' @rdname sciviews_functions
separate_rows_ <- structure(function(data, ..., sep = "[^[:alnum:].]+",
convert = FALSE) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a sciviews one.")

  # Sometimes groups are kept, sometimes not... for now, we do not care
  # (we always ungroup).
  is_x_dtf <- is_dtf(data)
  is_x_dtt <- is_dtt(data)
  res <- separate_rows(data = as_dtbl(fungroup(data)), ..., sep = sep,
    convert = convert)
  if (is_x_dtf)
    res <- as_dtf(res)
  if (is_x_dtt)
    res <- as_dtt(collect(res))
  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("tidyr::unite"))

#' @export
#' @rdname sciviews_functions
fill_ <- structure(function(data, ...,
.direction = c("down", "up", "downup", "updown")) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a sciviews one.")

  if (inherits(data, "GRP_df")) {
    is_x_grp_df <- TRUE
    gvars <- fgroup_vars(data, return = "names")
    gvars <- lapply(gvars, as.name)
  } else {
    is_x_grp_df <- FALSE
  }
  if (is_dtt(data)) {
    res <- collect(fill(as_dtbl(data), ..., .direction = .direction))
    res <- as_dtt(res)
  } else {
    res <- fill(data, ..., .direction = .direction)
  }
  if (is_x_grp_df)
    res <- do.call(fgroup_by, c(list(.X = res), gvars))
  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("tidyr::fill"))

#' @export
#' @rdname sciviews_functions
extract_ <- structure(function(data, col, into, regex = "([[:alnum:]]+)",
remove = TRUE, convert = FALSE, ...) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a sciviews one.")

  if (inherits(data, "GRP_df")) {
    is_x_grp_df <- TRUE
    gvars <- fgroup_vars(data, return = "names")
    gvars <- lapply(gvars, as.name)
  } else {
    is_x_grp_df <- FALSE
  }
  if (is_dtt(data)) {

    res <- do.call(extract, list(as_dtbl(data), col = substitute(col),
      into = into, regex = regex, remove = remove, convert = convert, ...))
    res <- as_dtt(collect(res))
  } else {
    res <- do.call(extract, list(data, col = substitute(col), into = into,
      regex = regex, remove = remove, convert = convert, ...))
  }
  if (is_x_grp_df)
    res <- do.call(fgroup_by, c(list(.X = res), gvars))
  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("tidyr::extract"))
