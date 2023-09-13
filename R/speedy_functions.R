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
#' `new_name = old_name`! The speedy functions all are prefixed with an "s",
#' like [smutate()], and build on the work initiated in {collapse} to propose a
#' series of paired functions with the tidy ones. So, [smutate()] and [mutate()]
#' are "speedy" and 'tidy" counterparts and they are used in a very similar, if
#' not identical way. This notation using a "s" prefix is there to draw the
#' attention on their particularities. Their classes are **function** and
#' **speedy_fn**. Avoid mixing tidy, speedy and non-tidy/speedy functions in the
#' same pipeline.
#' **This is a global page to present all the speedy functions in one place.**
#' It is not meant to be a clear and detailed help page of all individual "s"
#' functions. Please, refer to the corresponding help page of the non-"s" paired
#' function for more details! You can use the {svMisc}'s `.?smutate` syntax to
#' go to the help page of the non-"s" function with a message.
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
#' @param cols A selection of the columns using tidy-select syntax, see[tidyr::pivot_longer()].
#' @param names_to A character vector with the name or names of the columns for the names.
#' @param values_to A string with the name of the column that receives the values.
#' @param names_from The column or columns containing the names (use tidy selection and do not quote the names).
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
#' From {dplyr}, the [slice()] and `slice_xxx()` functions are not added yet
#' because they are not available for {dbplyr}. Also [anti_join()],
#' [semi_join()] and [nest_join()] are not implemented yet.
#' From {tidyr} [expand()], [chop()], [unchop()], [nest()], [unnest()],
#' [unnest_longer()], [unnest_wider()], [hoist()], [pack()] and [unpack()] are
#' not implemented yet.
#'
#' @return See corresponding "non-s" function for the full help page with
#' indication of the return values.
#'
#' @export
#' @name speedy_functions
#'
#' @examples
#' # TODO...
list_speedy_functions <- function() {
  c("sadd_count", "sadd_tally", "sarrange", "sbind_cols", "sbind_rows",
    "scount", "sdistinct", "sdrop_na", "sextract", "sfill", "sfilter",
    "sfilter_ungroup", "sfull_join", "sgroup_by", "sinner_join", "sleft_join",
    "smutate", "smutate_ungroup", "spivot_longer", "spivot_wider", "spull",
    "srename", "srename_with", "sreplace_na", "sright_join", "sselect",
    "sseparate", "sseparate_rows", "ssummarise", "stally", "stransmute",
    "stransmute_ungroup", "suncount", "sungroup", "sunite")
}

.src_speedy <- function(src, comment = "A speedy function, see ?speedy_functions.") {
  attr(comment, "src") <- src
  comment
}

#' @export
#' @rdname speedy_functions
sgroup_by <- structure(function(.data, ...) {
  # Different args names than fgroup_by() and not all arguments not in group_by()
  # TODO: accept somethinf like this: sgroup_by(df, across(...)).
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
smutate <- structure(function(.data, ..., .keep = "all") {
    fmutate(.data, ..., .keep = .keep)
  },
  # TODO: Arguments .by, .before= and .after= not supported yet, but to be
  # implemented
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
  # TODO: implement .by and align arguments
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

#' @export
#' @rdname speedy_functions
sbind_rows <- structure(function(..., .id = NULL) {
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
}, class = c("function", "speedy_fn"), comment = .src_speedy("dplyr::bind_rows"))

#' @export
#' @rdname speedy_functions
scount <- structure(function(x, ..., wt = NULL, sort = FALSE, name = NULL,
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
    res <- res[order(res[[name]], decreasing = TRUE), ] # TODO: use data.table::setorder() instead
  default_dtx(res)
}, class = c("function", "speedy_fn"), comment = .src_speedy("collapse::fcount"))

#' @export
#' @rdname speedy_functions
stally <- structure(function(x, wt = NULL, sort = FALSE, name = NULL,
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
    res <- res[order(res[[name]], decreasing = TRUE), ] # TODO: use data.table::setorder() instead
  default_dtx(res)
}, class = c("function", "speedy_fn"), comment = .src_speedy("collapse::fcount"))

#' @export
#' @rdname speedy_functions
sadd_count <- structure(function(x, ..., wt = NULL, sort = FALSE, name = NULL,
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
  # sort= argument of dplyr::add_count sorts the frequency column indeed, not the
  # category column(s)
  if (isTRUE(sort))
    res <- res[order(res[[name]], decreasing = TRUE), ] # TODO: use data.table::setorder()
  default_dtx(res)
}, class = c("function", "speedy_fn"), comment = .src_speedy("collapse::fcount"))

#' @export
#' @rdname speedy_functions
sadd_tally <- structure(function(x, wt = NULL, sort = FALSE, name = NULL,
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
  # sort= argument of dplyr::add_tally sorts the frequency column indeed, not the
  # category column(s)
  if (isTRUE(sort))
    res <- res[order(res[[name]], decreasing = TRUE), ] # TODO: use data.table::setorder() instead
  default_dtx(res)
}, class = c("function", "speedy_fn"), comment = .src_speedy("collapse::fcount"))


# Verbs that are not reengineered yet -------------------------------------

#' @export
#' @rdname speedy_functions
sbind_cols <- structure(function(...,
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
}, class = c("function", "speedy_fn"), comment = .src_speedy("dplyr::bind_cols"))

#' @export
#' @rdname speedy_functions
sarrange <- structure(function(.data, ..., .by_group = FALSE) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(.data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a speedy one.")

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
}, class = c("function", "speedy_fn"), comment = .src_speedy("dplyr::arrange"))

#' @export
#' @rdname speedy_functions
spull <- structure(function(.data, var = -1, name = NULL, ...) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(.data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a speedy one.")

  do.call(pull, list(.data = .data, var = substitute(var),
    name = substitute(name), ...))
}, class = c("function", "speedy_fn"), comment = .src_speedy("dplyr::pull"))

#' @export
#' @rdname speedy_functions
sdistinct <- structure(function(.data, ..., .keep_all = FALSE) {
  # For now, we use same function as txxx() counterpart... still must rework
  # Can use collapse::funique() by transforming the variables into a vector of
  # names, dropping .data$ and if .env$... is used, add it as .env$... in the
  # data frame, then use funique(), then drop or not unused variables
  if (inherits(.data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a speedy one.")

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
}, class = c("function", "speedy_fn"), comment = .src_speedy("dplyr::distinct"))


# tidyr verbs -------------------------------------------------------------

#' @export
#' @rdname speedy_functions
sdrop_na <- structure(function(data, ...) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a speedy one.")

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
}, class = c("function", "speedy_fn"), comment = .src_speedy("tidyr::drop_na"))

#' @export
#' @rdname speedy_functions
sreplace_na <- structure(function(data, replace, ...) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a speedy one.")

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
}, class = c("function", "speedy_fn"), comment = .src_speedy("tidyr::replace_na"))

#' @export
#' @rdname speedy_functions
spivot_longer <- structure(function(data, cols, names_to = "name",
values_to = "value", ...) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a speedy one.")

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
}, class = c("function", "speedy_fn"), comment = .src_speedy("tidyr::pivot_longer"))

# This is needed for R CMD check otherwise itwill complain
name <- NULL
value <- NULL

#' @export
#' @rdname speedy_functions
spivot_wider <- structure(function(data, names_from = name,
  values_from = value, ...) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a speedy one.")

  # Sometimes groups are kept, sometimes not... for now, we do not care.
  is_x_dtf <- is_dtf(data)
  is_x_dtt <- is_dtt(data)
  res <- do.call(pivot_wider, list(data = fungroup(data),
    names_from = substitute(names_from), values_from = substitute(values_from), ...))
  if (is_x_dtf)
    res <- as_dtf(res)
  if (is_x_dtt)
    res <- as_dtt(collect(res))
  res
}, class = c("function", "speedy_fn"), comment = .src_speedy("tidyr::pivot_wider"))

#' @export
#' @rdname speedy_functions
suncount <- structure(function(data, weights, .remove = TRUE, .id = NULL) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a speedy one.")

  # Sometimes groups are kept, sometimes not... for now, we do not care (we always ungroup).
  is_x_dtf <- is_dtf(data)
  is_x_dtt <- is_dtt(data)
  res <- do.call(uncount, list(data = fungroup(data),
    weights = substitute(weights), .remove = .remove, .id = .id))
  if (is_x_dtf)
    res <- as_dtf(res)
  if (is_x_dtt)
    res <- as_dtt(collect(res))
  res
}, class = c("function", "speedy_fn"), comment = .src_speedy("tidyr::uncount"))

#' @export
#' @rdname speedy_functions
sunite <- structure(function(data, col, ..., sep = "_", remove = TRUE,
na.rm = FALSE) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a speedy one.")

  # Sometimes groups are kept, sometimes not... for now, we do not care (we always ungroup).
  is_x_dtf <- is_dtf(data)
  is_x_dtt <- is_dtt(data)
  res <- inject(unite(data = as_dtbl(fungroup(data)), col = !!substitute(col),
    ..., sep = sep, remove = remove, na.rm = na.rm))
  if (is_x_dtf)
    res <- as_dtf(res)
  if (is_x_dtt)
    res <- as_dtt(collect(res))
  res
}, class = c("function", "speedy_fn"), comment = .src_speedy("tidyr::unite"))

#' @export
#' @rdname speedy_functions
sseparate <- structure(function(data, col, into, sep = "[^[:alnum:]]+",
remove = TRUE, convert = FALSE, ...) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a speedy one.")

  # Sometimes groups are kept, sometimes not... for now, we do not care (we always ungroup).
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
}, class = c("function", "speedy_fn"), comment = .src_speedy("tidyr::separate"))

#' @export
#' @rdname speedy_functions
sseparate_rows <- structure(function(data, ..., sep = "[^[:alnum:].]+",
convert = FALSE) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a speedy one.")

  # Sometimes groups are kept, sometimes not... for now, we do not care (we always ungroup).
  is_x_dtf <- is_dtf(data)
  is_x_dtt <- is_dtt(data)
  res <- separate_rows(data = as_dtbl(fungroup(data)), ..., sep = sep,
    convert = convert)
  if (is_x_dtf)
    res <- as_dtf(res)
  if (is_x_dtt)
    res <- as_dtt(collect(res))
  res
}, class = c("function", "speedy_fn"), comment = .src_speedy("tidyr::unite"))

#' @export
#' @rdname speedy_functions
sfill <- structure(function(data, ...,
.direction = c("down", "up", "downup", "updown")) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a speedy one.")

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
}, class = c("function", "speedy_fn"), comment = .src_speedy("tidyr::fill"))

#' @export
#' @rdname speedy_functions
sextract <- structure(function(data, col, into, regex = "([[:alnum:]]+)",
remove = TRUE, convert = FALSE, ...) {
  # For now, we use same function as txxx() counterpart... still must rework
  if (inherits(data, c("tbl_db", "dtplyr_step")))
    stop("You must collect results from a tidy function before using a speedy one.")

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
}, class = c("function", "speedy_fn"), comment = .src_speedy("tidyr::extract"))
