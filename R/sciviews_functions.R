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
#' @param .add If `TRUE`, the grouping variables are added to the existing ones.
#' @param .na.last How to treat missing values in groups? Assign them to the last
#'   group by default (`TRUE`).
#' @param .return.groups If `TRUE`, the grouping variables are returned in the GRP
#'   object (default).
#' @param .return.order If `TRUE`, the order of the grouping variables is
#'   returned in the object (by default, same value as `sort=`).
#' @param .method The algorithm to use for grouping:  `"radix"`, `"hash"`, or
#'   `"auto"` (by default). `"auto"` chose `"radix"` when `sort = TRUE` and
#'   `"hash"` otherwise.
#' @param .by A list of names of the columns to use for grouping the data.
#' @param .preserve When data is grouped, do we preserve grouping or recalculate
#'   it according to the new data frame obtained?
#' @param .groups How to treat the grouping variables in the result? Possible
#'   values are `"drop_last"` (default), `"drop"` (no grouping variables),
#'   `"keep"` (keep all grouping variables), or `"rowwise"` (not implemented
#'   yet).
#' @param .keep.group_vars If `TRUE` (by default), the grouping variables are
#'   kept in the result.
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
#' @param .sort If `TRUE` groups are sorted.
#' @param sort If `TRUE` largest group will be shown on top.
#' @param .drop Are levels with no observations dropped (`TRUE` by default).
#' @param sort_cat Are levels sorted (`TRUE` by default).
#' @param decreasing Is sorting done in decreasing order (`FALSE` by default)?
#' @param .decreasing Is sorting done in decreasing order (`FALSE` by default)?
#' @param name The name of the new column in the output (`n` by default, and no
#' existing column must have this name, or an error is generated).4
#' @param var A variable specified as a name, a positive or a negative integer
#'   (counting from the end). The default is `-1` and returns last variable.
#' @param .keep_all If `TRUE` keep all variables in `.data`.
#' @param .before Place new columns before this one.
#' @param .after Place new columns after this one.
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
    "full_join_", "group_by_", "inner_join_", "left_join_", "mutate_",
    "pivot_longer_", "pivot_wider_", "pull_", "reframe_", "rename_",
    "rename_with_", "replace_na_", "right_join_", "select_", "separate_",
    "separate_rows_", "summarise_", "tally_", "transmute_", "uncount_",
    "ungroup_", "unite_")
}

.src_sciviews <- function(src,
  comment = "A SciViews function, see ?sciviews_functions.") {
  attr(comment, "src") <- src
  comment
}

# Basically, all_of() does nothing, it is useless in SciViews functions,
# but we define it here to be compatible with tidy-select and dplyr.
#' @export
#' @rdname sciviews_functions
all_of <- function(x) x

#' @export
#' @rdname sciviews_functions
#' @param return What to return: `"data"` or `1`, `"unique"` or `2` for unique
#'   rows of grouping columns, `"names"` or `3` (default) for names of grouping
#'   columns, `"indices"` or `4` for integer indices of grouping columns,
#'   `"named_indices"` or `5` for named indices, `"logicial"` or `6` for logical
#'   selection vector of grouping columns, or `"named_logical"` or `7` for named
#'   logical.
group_vars_ <- function(.data = (.), return = "names") {

  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(.data) || !is.data.frame(.data))
    return(eval_data_dot(sys.call(), arg = '.data', abort_msg =
        gettext("`.data` must be a `data.frame`.")))

  if (!is_grouped_df(.data)) {
    switch(as.character(return),
      "1" =,
      "data" = .data[0L, 0L],
      "2" =,
      "unique" = unique(.data),
      "3" =,
      "names" = character(0),
      "4" =,
      "indices" =,
      "5" =,
      "named_indices" = integer(0),
      "6" =,
      "logical" =,
      "7" =,
      "named_logical" = logical(0),
      stop("Unknown {.arg return} value."))
  } else {
    fgroup_vars(.data, return = return)
  }
}

#' @export
#' @rdname sciviews_functions
group_rows_ <- function(.data = (.)) {
  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(.data))
    return(eval_data_dot(sys.call(), arg = '.data', abort_msg =
        gettext("`.data` must be a `data.frame`.")))

  if (!is_grouped_df(.data)) {
    list(seq_len(nrow(.data)))
  } else {
    # Unfortunately, gsplit() skips empty groups, so the hack!
    res <- gsplit(seq_len(nrow(.data)), GRPid(.data), use.g.names = TRUE)
    ngroups <- length(GRPN(.data, expand = FALSE))
    nms <- as.character(1:ngroups)
    res <- res[nms]
    res <- lapply(res, function(x) if (is.null(x)) integer(0) else x)
    names(res) <- NULL
    res
  }
}

#' @export
#' @rdname sciviews_functions
group_data_ <- function(.data = (.)) {
  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(.data))
    return(eval_data_dot(sys.call(), arg = '.data', abort_msg =
      gettext("`.data` must be a `data.frame`.")))

  if (!is_grouped_df(.data)) {
    res <- .data[1L, 1L]
    names(res) <- ".rows"
    res$.rows <- list(seq_len(nrow(.data)))
    res
  } else {
    res <- fgroup_vars(.data, return = "unique")
    res$.rows <- group_rows_(.data)
    res
  }
}

#' @export
#' @rdname sciviews_functions
group_indices_ <- function(.data = (.), ...) {
  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(.data))
    return(eval_data_dot(sys.call(), arg = '.data', abort_msg =
        gettext("`.data` must be a `data.frame`.")))

  if (!missing(...))
    check_dots_empty()

  if (!is_grouped_df(.data)) {
    rep_len(1, nrow(.data))
  } else {
    GRPid(.data)
  }
}

#' @export
#' @rdname sciviews_functions
group_keys_ <- function(.data = (.), ...) {
  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(.data))
    return(eval_data_dot(sys.call(), arg = '.data', abort_msg =
        gettext("`.data` must be a `data.frame`.")))

  if (!missing(...))
    check_dots_empty()

  if (!is_grouped_df(.data)) {
    .data[1L, 0L]
  } else {
    fgroup_vars(.data, return = "unique")
  }
}

#' @export
#' @rdname sciviews_functions
groups_ <- function(.data = (.)) {
  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(.data))
    return(eval_data_dot(sys.call(), arg = '.data', abort_msg =
        gettext("`.data` must be a `data.frame`.")))

  if (!is_grouped_df(.data)) {
    list()
  } else {
    lapply(as.list(fgroup_vars(.data, return = "names")), as.symbol)
  }
}

#' @export
#' @rdname sciviews_functions
group_size_ <- function(.data = (.)) {
  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(.data))
    return(eval_data_dot(sys.call(), arg = '.data', abort_msg =
        gettext("`.data` must be a `data.frame`.")))

  if (!is_grouped_df(.data)) {
    nrow(.data)
  } else {
    GRPN(.data, expand = FALSE)
  }
}

#' @export
#' @rdname sciviews_functions
n_groups_ <- function(.data = (.)) {
  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(.data))
    return(eval_data_dot(sys.call(), arg = '.data', abort_msg =
        gettext("`.data` must be a `data.frame`.")))

  if (!is_grouped_df(.data)) {
    1L
  } else {
    length(GRPN(.data, expand = FALSE))
  }
}

# TODO: default .drop as in dplytr::group_by()
# TODO: what are those "computations" that one can give to group_by()? See doc
# TODO: also compute the grouping variables in the data frame itself, like
# group_by does (+ an argument to do so or not)
#' @export
#' @rdname sciviews_functions
group_by_ <- structure(function(.data = (.), ..., .add = FALSE, .drop = NULL,
    .sort = get_collapse("sort"), .decreasing = FALSE, .na.last = TRUE,
    .return.groups = TRUE, .return.order = .sort, .method = "auto") {

  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(.data) || !is.data.frame(.data))
    return(eval_data_dot(sys.call(), arg = '.data', abort_msg =
      gettext("`.data` must be a `data.frame`.")))

  # If no grouping variables provided
  if (missing(...)) {
    if (isTRUE(.add)) {# Return unmodified .data
      return(.data)
    } else {# Return an ungrouped data frame
      #let_data.trame_to_data.table(.data)
      #on.exit(let_data.table_to_data.trame(.data))
      return(fungroup(.data))
    }
  }

  # Treat data.trames as data.tables
  to_dtrm <- is.data.trame(.data)
  if (to_dtrm) {
    let_data.trame_to_data.table(.data)
    on.exit(let_data.table_to_data.trame(.data))
  }

  args <- formula_select(..., .fast.allowed.funs = "")

  # Default value for .drop is TRUE, unless previously grouped using FALSE
  if (missing(.drop)) {
    if (is_grouped_df(.data)) {
      .drop <- attr(.data, "groups") |> attr(".drop")
      if (is.null(.drop))
        .drop <- TRUE
    } else {
      .drop <- TRUE
    }
  }

  # There are cases not handled (yet) by collapse::fgroup_by(). So, we use
  # dplyr::group_by() and convert the corresponding object into a GRP_df,
  # also reconverting the tibble into data.frame, data.table, or data.trame...
  # Not very efficient, but the only solution I fould to support those cases.

  # *  .drop = FALSE not implemented yet in fgroup_by() (should be possible,
  #    because converting a grouped_df object into GRP gives the correct groups)
  #    For now, we just use dplyr::group_by() and do the conversion...
  # *  .add = TRUE is also not implemented yet in fgroup_by(), but we have a
  #    solution for group_by_vars() when not using formulas.
  # *  expressions are handled differently by group_by() and fgroup_by(), so,
  #    for any expression found, we automatically switch to group_by().
  if ((isTRUE(.add) && args$are_formulas) || !isTRUE(.drop) || !args$fastselect) {
    # group_by() cannot handle .sort = FALSE, decreasing = TRUE or
    # .na.last = FALSE
    if (isFALSE(.sort) || isTRUE(.decreasing) || isFALSE(.na.last))
      stop("Using {.fun dplyr::group_by_} but it only sorts increasing  with {.code NA} last.",
        i = "So, keep defaults for {.arg .sort}, {.arg .decreasing} and {.arg .na.last}.")

    # Call group_by() and convert the grouped_df object into a GRP_df object
    .__dplyr_error_call__. <- environment() # For correct display of the error
    # Since dplyr::group_by() does not accept character names,
    # we have to transform them into symbols
    args$dots <- lapply(args$dots, function(x) {
      if (is.character(x)) {
        as.symbol(x)
      } else {
        x
      }
    })
    # If we have data already grouped with collapse::fgroup_by(),
    # dplyr::group_by() issues an error, so, we must ungroup it first
    if (inherits(.data, 'GRP_df')) {
      # If we have .add = TRUE; we must handle existing grouping variable too
      if (isTRUE(.add)) {
        gvars <- as.list(fgroup_vars(.data, return = "names"))
        gvars <- lapply(gvars, as.symbol)
        args$dots <- unique(c(gvars, args$dots))
      }
      res0 <- fungroup(.data)
    } else {
      res0 <- .data
    }
    res0 <- do.call(group_by, c(args$dots, list(.data = res0,
      .add = force(.add), .drop = force(.drop))))
    groups <- GRP(res0)
    # We also keep .drop = FALSE attribute
    if (!isTRUE(.drop))
      attr(groups, '.drop') <- FALSE
    res <- .data
    # Add computed group variables into res
    names_newg <- setdiff(names(res0), names(res))
    if (length(names_newg))
      res <- add_vars(res, res0[names_newg])
    attr(res, "groups") <- groups
    # Change the class the same way fgroup_by() does
    class_res <- class(.data)
    class_res <- class_res[!class_res %in% c("grouped_df", "data.frame")]
    class(res) <- c("GRP_df", class_res, "grouped_df", "data.frame")
    if (to_dtrm)
      let_data.table_to_data.trame(res)
    return(res)
  }

  # For all other cases, we can use the faster collapse functions
  if (args$are_formulas) {# Use collapse::fgroup_by()
    res <- do.call(fgroup_by, c(args$dots, list(.X = .data, sort = force(.sort),
      decreasing = force(.decreasing), na.last = force(.na.last),
      return.groups = force(.return.groups),
      return.order = force(.return.order), method = force(.method))))
    # Note: should a problem emerge again with wrong interpretation of formulas
    # We could use this check to detect it:
    ## Check if we are in the case ~c(var1, var2), while we meant ~var1, ~var2
    #ngroups <- length(fgroup_vars(res, return = "names"))
    #largs <- ...length()
    #if (largs != ngroups)
    #  stop("Incorrect grouping formula (you got {.val {ngroups}} groups instead of {.val {largs}}).",
    #    i = "You cannot use {.code ~var1:var2} in {.fun group_by_}.",
    #    i = "and you must use {.code ~var1, ~var2, ...} instead of {.code ~c(var1, var2, ...)}.")

  } else {# Use collapse::groups_by_vars() instead
    gvars <- unlist(args$dots) # Also allows c('var1', 'var2', ...) not in dplyr
    if (isTRUE(.add) && is_grouped_df(.data))
      gvars <- unique(c(fgroup_vars(.data, return = "names"), gvars))
    res <- group_by_vars(.data, by = gvars, sort = .sort,
      decreasing = .decreasing, na.last = .na.last,
      return.groups = .return.groups, return.order = .return.order,
      method = .method)
  }

  if (to_dtrm)
    let_data.table_to_data.trame(res)
  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fgroup_by"))

# Note: only standard evaluation for ... for now
# Note: in dplyr, it is ungroup(x, ...), but changed x here to .data
#' @export
#' @rdname sciviews_functions
ungroup_ <- structure(function(.data = (.), ..., .na.last = TRUE,
    .method = "auto") {

  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(.data) || !is.data.frame(.data))
    return(eval_data_dot(sys.call(), arg = '.data', abort_msg =
      gettext("`.data` must be a `data.frame`.")))

  # Treat data.trames as data.tables
  to_dtrm <- is.data.trame(.data)
  if (to_dtrm) {
    let_data.trame_to_data.table(.data)
    on.exit(let_data.table_to_data.trame(.data))
  }

  if (...length()) {# Ungroup only certain data
    un_gvars <- c(...)
    if (!is.character(un_gvars)) {
      # Try to convert only formulas like ~name into character
      un_gvars <- sapply(un_gvars, function(x) {
        if (is_formula(x)) {
          x <- f_rhs(x)
          if (is.symbol(x))
            as.character(x)
        } else {# Something else, should not happen, so place something wrong
          quote(incorrect_item)
        }
      })
    }
    if (!is.character(un_gvars))
      stop("Incorrect names of the ungrouping variables.",
        i = "Use either {.code ~name} or {.code 'name'} (and don't mix both forms).")

    # Provided variables must be in the dataset
    non_exist <- setdiff(un_gvars, names(.data))
    if (length(non_exist)) {
      stop("Can't select columns that don't exist.",
        x = "Column {.code {non_exist[1]}} doesn't exist.")
    }

    # Consider only variables that are actually in the grouping
    gvars <- setdiff(fgroup_vars(.data, return = "names"), un_gvars)
    # If there remain grouping variables, we keep them
    if (length(gvars)) {
      # If we have a GRP_df object, return a similar object
      if (inherits(.data, "GRP_df")) {
        # Get configuration to apply the same one
        grp <- attr(.data, "groups")
        sort <- isTRUE(grp$ordered[1])
        drop <- attr(grp, ".drop")
        # If drop = FALSE, we must use group_by() and convert... not handled yet!
        if (!isTRUE(drop))
          warning("Regrouping with `.drop = FALSE` is not handled yet, changing to `.drop = TRUE`.")
        decreasing <- isTRUE(grp$ordered[2]) # If sort = TRUE, no matters
        return.groups <- !is.null(grp$groups)
        return.order <- !is.null(grp$order)
        res <- group_by_vars(.data, by = gvars, sort = sort,
          decreasing = decreasing, na.last = .na.last,
          return.groups = return.groups, return.order = return.order,
          method = .method)
        if (to_dtrm)
          let_data.table_to_data.trame(res)
        return(res)

      } else {# A dplyr grouped_df object... for now, don't change it to GRP_df
        drop <- attr(.data, "groups") |> attr(".drop")
        if (is.null(drop)) drop <- TRUE
        res <- group_by(.data, gvars, .drop = drop)
        if (to_dtrm)
          let_data.table_to_data.trame(res)
        return(res)
      }
    }
  }

  # No grouping variables left, so we just ungroup
  res <- fungroup(.data)
  if (to_dtrm)
    let_data.table_to_data.trame(res)
  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fungroup"))

# rename_() seems OK
#' @export
#' @rdname sciviews_functions
rename_ <- structure(function(.data = (.), ...) {

  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(.data) || !is.data.frame(.data))
    return(eval_data_dot(sys.call(), arg = '.data', abort_msg =
        gettext("`.data` must be a `data.frame`.")))

  # If no renaming arguments, return .data
  if (missing(...))
    return(.data)

  # Treat data.trames as data.tables
  to_dtrm <- is.data.trame(.data)
  if (to_dtrm) {
    let_data.trame_to_data.table(.data)
    on.exit(let_data.table_to_data.trame(.data))
  }

  args <- formula_select(..., .must.be.named = TRUE)

  # collapse::frename() uses old_name = new_name, but dplyr::rename() uses
  # new_name = old_name -> make frename() compatible with dplyr::rename()
  # Also, the cols= argument with a function matches rename_with() instead.
  # Transform symbols into names and flatten the list
  dots_flat <- unlist(lapply(args$dots, function(x) {
    if (is.symbol(x)) {
      as.character(x)
    } else {
      x
    }
  }))
  if (is.list(dots_flat))
    stop("Incorrect renaming inputs.",
      i = "Use {.code new_name = <old_name>} with {.code <old_name>} as a single {.cls character} or {.cls symbol}.")
  # If values are numbers, these are indices -> replace then by names
  # (because frename() cannot use indices)
  # Otherwise, invert names and values

  if (is.numeric(dots_flat)) {
    # Apparently, dplyr::rename() ignore both 0 and negative indices
    dots_flat <- dots_flat[dots_flat > 0] # Keep only positive indices
    dots_inv <- names(dots_flat)
    names(dots_inv) <- names(.data)[dots_flat]
  } else {
    dots_inv <- names(dots_flat)
    names(dots_inv) <- dots_flat
  }

  # Check that all old names are in .data
  # Remember: we have now old_name = "new_name" in dots_inv
  missing_cols <- setdiff(names(dots_inv), names(.data))
  if (length(missing_cols))
    stop("Can't rename columns that don't exist.",
      x = "Column {.code {missing_cols[1]}} doesn't exist.")

  res <- do.call(frename, c(list(.x = .data, .nse = FALSE), dots_inv),
    envir = args$env)
  if (to_dtrm)
    let_data.table_to_data.trame(res)
  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("dplyr::rename"))

#everything_ <- function() {
#  stop("This should not be called directly, see ?mutate_with.")
#}

# The equivalent of rename_with() "feature complete", but as slow as the dplyr version
#' @export
#' @rdname sciviews_functions
rename_with_ <- structure(function(.data = (.), .fn,
    .cols = ~everything(), ...) {

  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(.data) || !is.data.frame(.data))
    return(eval_data_dot(sys.call(), arg = '.data', abort_msg =
        gettext("`.data` must be a `data.frame`.")))

  # Special case .cols = ~everything()
  if (length(.cols) == 2L && substitute(.cols) == quote(~everything())) {
    .cols <- NULL
  } else {# Selection of some columns
    if (is_formula(.cols))
      .cols <- eval_select(f_rhs(.cols), .data)
    if (any(is.na(.cols)))
      stop("Selections can't have missing values.")
    if (is.character(.cols)) {
      all_cols <- seq_along(.data)
      names(all_cols) <- names(.data)
      .cols2 <- .cols
      .cols <- all_cols[.cols]
      if (anyNA(.cols))
        stop("Can't select columns that don't exist.",
          x = "Column {.code {(.cols2[is.na(.cols)][1])}} doesn't exist.")
    } else if (is.numeric(.cols)) {
      wrong_cols <- abs(.cols) > ncol(.data)
      if (any(wrong_cols))
        stop("Can't select columns past the end.",
          i = "Location{?s} {as.character(abs(.cols[wrong_cols]))} {?doesn't/don't} exist.",
          i = "There {?is/are} only {ncol(.data)} column{?s}.")

    } else if (is.logical(.cols)) {
      if (length(.cols) != ncol(.data))
        stop("Logical selection must match columns ({ncol(.data)}).")

      setv(.cols, NA, FALSE) # NA are considered as FALSE here
    } else if (anyNA(.cols)) {
      stop("Selections can't have missing values.")
    }
    if (!length(.cols)) # No column to rename, return .data
      return(.data)
  }

  if (missing(.fn))
    stop("Argument {.arg .fn} is missing, with no default.")
  if (is_formula(.fn)) {
    if (is_formula(.fn, lhs = FALSE)) { # Check for lhs absence
      # The convention being to use `.x` for the argument, we put names in .x
      # and the execute the call on it
      .call <- f_rhs(.fn)
      if (is.null(.cols)) {# Change all columns
        .x <- c(names(.data)) # c() to make sure to have a copy!
        names(.data) <- eval(.call)
      } else {# Change only a selection
        .x <- names(.data)[.cols]
        names(.data)[.cols] <- eval(.call)
      }
      return(.data)
    } else {
      stop("Can't convert {.arg .fn}, a two-sided {.cls formula}, to a {.cls function}.")
    }
  }

  # Now, we are supposed to get the name of a function...
  if (length(.fn) != 1L)
    stop("Can't convert {.arg .fn}, a {.cls {typeof(.fn)}} vector, to a {.cls function}.")
  if (is.character(.fn))
    .fn <- get0(.fn, envir = parent.frame(), mode = 'function')
  if (!is.function(.fn))
    stop("Can't convert {.arg .fn}, a {.cls {typeof(.fn)}} vector, to a {.cls function}.")

  # Treat data.trames as data.tables
  to_dtrm <- is.data.trame(.data)
  if (to_dtrm) {
    let_data.trame_to_data.table(.data)
    on.exit(let_data.table_to_data.trame(.data))
  }

  if (is.null(.cols)) {# Special case .cols = ~everything()
    # cols= missing to apply to all columns
    res <- frename(.data, .fn, ..., .nse = FALSE)
  } else {
    args <- formula_select(.cols)
    res <- frename(.data, .fn, ..., cols = .cols, .nse = FALSE)
  }
    if (to_dtrm)
      let_data.table_to_data.trame(res)
    res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::rename_with"))

#' @export
#' @rdname sciviews_functions
filter_ <- structure(function(.data = (.), ..., .by = NULL, .preserve = FALSE) {

  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(.data) || !is.data.frame(.data))
    return(eval_data_dot(sys.call(), arg = '.data', abort_msg =
        gettext("`.data` must be a `data.frame`.")))

  # Case missing(...), just return the data frame
  if (missing(...))
    return(.data)

  # Apparently, I don't need to transform a data.trame into a data.table here
  # Treat data.trames as data.tables
  #to_dtrm <- is.data.trame(.data)
  #if (to_dtrm) {
  #  let_data.trame_to_data.table(.data)
  #  on.exit(let_data.table_to_data.trame(.data))
  #}

  # Process dots with formula-masking
  is_grouped <- is_grouped_df(.data)
  if (is_grouped) {
    new_gvars <- fgroup_vars(.data, return = "names")
  } else {# No grouping variables
    new_gvars <- character(0)
  }

  # Apply formula-masking on ...
  no_se_msg <- gettext(
    "Standard evaluation is not supported for grouped data frames.")
  args <- formula_masking(..., .no.se = is_grouped, .no.se.msg = no_se_msg)

  # If .by is defined, use these groups, but do not keep them
  if (!missing(.by)) {
    if (is_grouped)
      stop("Can't supply {.arg .by} when {.arg .data} is a grouped data frame.")
    if (!args$are_formulas)
      stop(no_se_msg)
    # Here, we need to take care of the object class, or we end up with a
    # data.table in .data!
    #let_data.table_to_data.trame(.data)
    # TODO: a formula-select "lite" here
    res <- group_by_vars(.data, by = .by, sort = FALSE)
    #let_data.trame_to_data.table(.data)
    new_gvars <- character(0) # Don't keep these groups
  } else {
    res <- .data
  }

  # No input can be named
  dots_names <- names(args$dots)
  if (!is.null(dots_names)) {
    named <- whichv(dots_names, "", invert = TRUE)
    if (length(named)) {
      first_named <- named[1]
      first_name <- dots_names[first_named]
      first_expr <- expr_deparse(sys.call()[[first_name]])
      if (first_expr == "NULL") {# Probably using formula like name ~ expr
        stop("We detected a named input.",
          i = "Did you used formula like {.code name ~ expr}?",
          i = "You must filter with formula having only right member, like {.code ~expr}.")
      } else {# Same error message as dplyr
        msg_expr <- paste(first_name, first_expr, sep = " == ")
        stop("We detected a named input.",
          i = "This usually means that you've used {.code =} instead of {.code ==}.",
          i = "Did you mean {.code {msg_expr}}?")
      }
    }
  }

  filters <- args$dots
  # fsubset() or ss() can use only one subset argument at a time. So, we run it
  # multiple times on each argument to filter_() to mimic filter()
  if (args$are_formulas) {# NSE argument, must us fsubset()
    # fsubset() does not support groups, so, we need to do the job ourselves
    # TODO: implement it!
    if (is_grouped)
      stop("Filtering with grouped data frames is not implemented yet with {.fun filter_}.",
        i = "Use {.fun filter} with {.fun group_by} instead.")
    for (i in 1:...length())
      res <- do.call(fsubset, list(.x = res, filters[[i]]), envir = args$env)
  } else {# SE arguments, ss() is faster
    filter <- filters[[1L]]
    lfilters <- length(filters)
    if (lfilters > 1L) {
      for (i in 2:lfilters)
        filter <- filter & filters[[i]]
    }
    res <- ss(res, filter)
  }
  #if (to_dtrm)
  #  let_data.table_to_data.trame(res)
  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::filter"))

# Do we need to transform temporarily data.trame into data.table here?
#' @export
#' @rdname sciviews_functions
select_ <- structure(function(.data = (.), ...) {

  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(.data) || !is.data.frame(.data))
    return(eval_data_dot(sys.call(), arg = '.data',
      abort_msg = gettext("`.data` must be a `data.frame`.")))

  # Treat data.trames as data.tables
  to_dtrm <- is.data.trame(.data)
  if (to_dtrm) {
    let_data.trame_to_data.table(.data)
    on.exit(let_data.table_to_data.trame(.data))
  }

  # If no selection is provided
  if (missing(...)) # Return a data frame with zero columns
    return(.data[0L, , drop = FALSE])

  # Process dots to args
  args <- formula_select(..., .fast.allowed.funs = c(":", "-", "c"))
  if (!args$fastselect) {
    #message(gettextf("Using tidyselect with `%s`",
    #  paste(args$dots, collapse = ", ")))
    eval_select2 <- function(..., data)
      eval_select(expr(c(...)), data = data, error_call = stop_top_call())
    loc <- do.call(eval_select2, c(args$dots, list(data = .data)))
    if (to_dtrm)
      let_data.table_to_data.trame(.data)
    res <- .data[, loc, drop = FALSE]
    names(res) <- names(loc)

  } else {# fastselect with collapse
    if (args$are_formulas) {
      # Other return modes not supported (yet)
      res <- do.call(fselect, c(list(.x = .data, return = "data"), args$dots),
        envir = args$env)
    } else {
      res <- get_vars(.data, args$dots, rename = TRUE)
    }
    if (to_dtrm)
      let_data.table_to_data.trame(res)
  }

  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fselect"))

# TODO: .before and .after
#' @export
#' @rdname sciviews_functions
mutate_ <- structure(function(.data = (.), ..., .by = NULL,
    .keep = "all", .before = NULL, .after = NULL, .cols = NULL) {

  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(.data) || !is.data.frame(.data))
    return(eval_data_dot(sys.call(), arg = '.data', abort_msg =
        gettext("`.data` must be a `data.frame`.")))

  if (!is.character(.keep))
    stop("{.arg .keep} must be a string or character vector.")
  if (length(.keep) != 1L)
    stop("{.arg .keep} must be a single string, not a vector of length {.val {length(.keep)}}.")
  .keep <- .keep
  if (fmatch(.keep, c("all", "used", "unused", "none"), 0L) == 0L)
    stop("{.arg .keep} must be one of \"all\", \"used\", \"unused\", or \"none\", not \"{(.keep)}\".")

  is_grouped <- is_grouped_df(.data)

  # Case missing(...), just return the original data frame
  if (missing(...))
    return(.data)

  # Treat data.trames as data.tables (not needed here?)
  #to_dtrm <- is.data.trame(.data)
  #if (to_dtrm) {
  #  let_data.trame_to_data.table(.data)
  #  on.exit(let_data.table_to_data.trame(.data))
  #}

  # Process dots with formula-masking
  no_se_msg <- gettext(
    "Standard evaluation is not supported for grouped data frames.")
  args <- formula_masking(..., .make.names = TRUE, .no.se = is_grouped,
    .no.se.msg = no_se_msg)

  # If .by is defined, use these groups, but do not keep them
  if (!missing(.by)) {
    if (is_grouped)
      stop("can't supply {.arg .by} when {.arg .data} is a grouped data frame.")
    if (!args$are_formulas)
      abort(no_se_msg)
    # Here, we need to take care of the object class, or we end up with a
    # data.table in .data!
    #let_data.table_to_data.trame(.data)
    res <- group_by_vars(.data, by = .by, sort = FALSE)
    #let_data.trame_to_data.table(.data)
  } else {
    res <- .data
  }

  res <- do.call(fmutate, c(list(.data = res), args$dots,
    list(.keep = force(.keep), .cols = force(.cols))), envir = args$env)
  if (!missing(.by))
    res <- fungroup(res)
  if (!missing(.before) || !missing(.after)) {# Reorder the columns
    # TODO: not implemented yet
    stop("{.arg .before} and {.arg .after} are not implemented yet in {.fun mutate_}.",
      i = "Use {.fun mutate} instead.")
    #res_names <- names(res)
    #data_names <- names(.data)
  }
  #if (to_dtrm)
  #  let_data.table_to_data.trame(res)
    res
},
  class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fmutate"))

#' @export
#' @rdname sciviews_functions
transmute_ <- structure(function(.data, ...) {

  .__top_call__. <- TRUE

  do.call(mutate_, list(.data, ..., .keep = "none"), envir = parent.frame())
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("dplyr::transmute"))

# On the contrary to summarise() we always do not warn if summarise() returns
# several rows (but reframe_() is there too)
# Also, .group = "rowwise" is not implemented here
# TODO: across_() that constructs a list of formulas
# TODO: n_() that gets the number of items in the group
#' @export
#' @rdname sciviews_functions
summarise_ <- structure(function(.data = (.), ..., .by = NULL,
    .groups = "drop_last", .keep.group_vars = TRUE, .cols = NULL) {

  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(.data) || !is.data.frame(.data))
    return(eval_data_dot(sys.call(), arg = '.data', abort_msg =
        gettext("`.data` must be a `data.frame`.")))

  # For grouped data, compute the grouping variables that must be returned
  is_grouped <- is_grouped_df(.data)
  if (is_grouped) {
    gvars <- fgroup_vars(.data, return = "names")
    new_gvars <- switch(.groups,
      drop_last = gvars[-length(gvars)],
      drop      = character(0),
      keep      = gvars,
      rowwise   = stop("{.arg .groups} must be {.code \"drop_last\"}, {.code \"drop\"}, or {.code \"keep\"}. {.code \"rowwise\"} is not supported."),
      stop("{.arg .groups} must be {.code \"drop_last\"}, {.code \"drop\"}, or {.code \"keep\"}.")
    )
  } else {# No grouping variables
    new_gvars <- character(0)
  }

  # Treat data.trames as data.tables
  to_dtrm <- is.data.trame(.data)
  if (to_dtrm) {
    let_data.trame_to_data.table(.data)
    on.exit(let_data.table_to_data.trame(.data))
  }

  # Case missing(...)
  # We only allow standard evaluation,... or formulas. They cannot be mixed and
  # standard evaluation is NOT possible for grouped data.
  if (missing(...)) {
    # If data is grouped, we return the number of observations par level
    if (is_grouped) {
      # We return only the grouping variables, like summarise() does
      # fsummarise() refuses to do the calculation without calculating at least
      # one column... so, we calculate and the drop it
      .zzzz <- .data[[gvars[1]]]
      res <- fsummarise(.data, .zzzz = .zzzz[1])
      res$.zzzz <- NULL
      if (to_dtrm)
        let_data.table_to_data.trame(res)
      # fsummarise() ungroup the result, but not summarise()
      if (length(new_gvars)) {
        return(group_by_vars(res, by = new_gvars))
      } else {# Only one grouping variable, so, we ungroup, OK
        return(res)
      }
    } else {# Return a df with 0 columns and 1 row (like dplyr::summarise())
      if (to_dtrm)
        let_data.table_to_data.trame(.data)
      return(.data[1L, 0L])
    }
  }

  # Process dots with formula-masking
  no_se_msg <- gettext(
    "Standard evaluation is not supported for grouped data frames.")
  args <- formula_masking(..., .make.names = TRUE, .no.se = is_grouped,
    .no.se.msg = no_se_msg)

  # If .by is defined, use these groups, but do not keep them
  if (!missing(.by)) {
    if (is_grouped)
      stop("can't supply {.arg .by} when {.arg .data} is a grouped data frame.")
    if (!args$are_formulas)
      abort(no_se_msg)
    # Here, we need to take care of the object class, or we end up with a
    # data.table in .data!
    let_data.table_to_data.trame(.data)
    .data <- group_by_vars(.data, by = .by, sort = FALSE)
    let_data.trame_to_data.table(.data)
    new_gvars <- character(0) # Don't keep these groups
  } else {
    .data <- .data
  }

  res <- do.call(fsummarise, c(list(.data = .data), args$dots,
    list(keep.group_vars = force(.keep.group_vars), .cols = force(.cols))),
    envir = args$env)
  if (length(new_gvars)) # (re)set grouping
    res <- group_by_vars(res, by = new_gvars)
  if (to_dtrm)
    let_data.table_to_data.trame(res)
  res
}, class = c("function", "sciviews_fn"),
  comment = .src_sciviews("collapse::fsummarise"))

# Reframe_() works unless some groups return a 0-row result. It also does not
# support the return of a data.frame with several columns, nor across()
# The syntax using .cols/.data does not work either
#' @export
#' @rdname sciviews_functions
reframe_ <- structure(function(.data, ..., .by = NULL, .groups = "drop",
    .keep.group_vars = TRUE, .cols = NULL) {

  .__top_call__. <- TRUE

  # Call summarise_(.groups = "drop")
  if (.groups != "drop")
    stop("{.fun reframe_} only accepts {.code .groups = \"drop\"}.",
      i = "Use {.fun summarise_} instead.")

  call <- sys.call()
  call[[1]] <- as.symbol('summarise_') # Use summarise_() instead
  if (missing(.data) || !is.data.frame(.data))
    return(eval_data_dot(sys.call(), arg = '.data', abort_msg =
        gettext("`.data` must be a `data.frame`.")))

  # Use summarise_() with same arguments
  eval_bare(call, env = parent.frame())
}, class = c("function", "sciviews_fn"),
comment = .src_sciviews("collapse::fsummarise"))

#' @export
#' @rdname sciviews_functions
#' @param .locale The locale to sort character vectors in. If `NULL`(default),
#'   use `"C"` locale.
arrange_ <- structure(function(.data = (.), ..., .by_group = FALSE,
  .locale = "C", .decreasing = FALSE) {

  .__top_call__. <- TRUE
  .__dplyr_error_call__. <- environment()

  # Implicit data-dot mechanism
  if (missing(.data) || !is.data.frame(.data))
    return(eval_data_dot(sys.call(), arg = '.data', abort_msg =
        gettext("`.data` must be a `data.frame`.")))

  # Case missing(...), or no rows, or only one row, just return the data frame
  if (missing(...) || nrow(.data) < 2L)
    return(.data)

  # Not necessary?
  # Treat data.trames as data.tables
  #to_dtrm <- is.data.trame(.data)
  #if (to_dtrm) {
  #  let_data.trame_to_data.table(.data)
  #  on.exit(let_data.table_to_data.trame(.data))
  #}

  # dplyr uses desc(var) and collapse uses -var to sort in descending order
  args <- formula_select(..., .fast.allowed.funs = c("desc", "-"))

  if (is.null(.locale) && !isTRUE(getOption("dplyr.legacy_locale")))
    .locale <- "C" # Default locale for sorting

  if (!is.null(.locale) && .locale == "C" && args$fastselect) {
    # We can use the fast collapse::roworder()/roworderv()
    if (args$are_formulas) {
      if (!missing(.decreasing))
        warning("`.decreasing` is ignored when using formulas in arrange_().")
      # Transform desc(var) into -var, since collapse does not understand desc()
      minus <- as.symbol('-')
      args$dots <- lapply(args$dots, function(x) {
        if (is.call(x) && x[[1]] == 'desc')
          x[[1]] <- minus
        x
      })
    } else {# SE mode: unlist the arguments, and possibly "extract" .decreasing=
      # from the arguments
      ovars <- unlist(args$dots)
      if (!is.character(ovars))
        stop("All arguments in {.code ...} must be column names or formulas.")
      if (missing(.decreasing)) {# Recreate it from the vars (TRUE when '-var')
        .decreasing <- startsWith(ovars, "-")
        ovars[.decreasing] <- substring(ovars[.decreasing], 2L) # Eliminate '-'
      }
    }
    # If this is a grouped_df, we ungroup, sort, and then, recalculate groups
    if (is_grouped_df(.data)) {
      gvars <- fgroup_vars(.data, return = "names")
      grp <- attr(.data, "groups")
      desc <- isTRUE(grp$ordered[2])
      drop <- attr(grp, ".drop")

      if (args$are_formulas) {
        # If we sort by groups, prepend the grouping variables to the list
        if (isTRUE(.by_group)) {
          if (desc)
            gvars <- paste0("-", gvars) # Collapse understands '-var'
          args$dots <- c(as.list(gvars), args$dots)
        }
        res <- do.call(roworder, c(list(X = fungroup(.data)), args$dots),
          envir = args$env)

      } else {# SE arguments
        # If we sort by groups, prepend the grouping variables to the list
        if (isTRUE(.by_group)) {
          ovars <- c(gvars, ovars)
          odesc <- c(rep_along(gvars, desc), rep_along(ovars, .decreasing))
        } else {
          odesc <- .decreasing
        }
        res <- roworderv(fungroup(.data), cols = ovars, decreasing = odesc)
      }

      # Recalculate groups
      # Collect required data to redo a similar grouping after sorting
      is_grp <- inherits(.data, "GRP_df") # either a GRP_df or a grouped_df
      drop <- attr(grp, ".drop")
      # Regroup
      if (is_grp) {
        # If drop = FALSE, we must use group_by() and convert... not handled yet!
        if (isFALSE(drop))
          warning("Regrouping with `.drop = FALSE` is not handled yet, changing to `.drop = TRUE`.")
        sort <- isTRUE(grp$ordered[1])
        return.groups <- !is.null(grp$groups)
        return.order <- !is.null(grp$order)
        # How do I got na.last and method? Better to always keep defaults here?
        res <- group_by_vars(res, by = gvars, sort = sort,
          decreasing = desc, na.last = TRUE, return.groups = return.groups,
          return.order = return.order, method = 'auto')

      } else {# Regroup with dplyr::group_by()
        gvars_sym <- lapply(as.list(gvars), as.symbol)
        res <- do.call(group_by, c(list(.data = res, .drop = drop), gvars_sym),
          envir = args$env)
      }

    } else {# Ungrouped dataset, no particular difficulties
      if (args$are_formulas) {
        res <- do.call(roworder, c(list(X = .data), args$dots),
          envir = args$env)
      } else {# Not using formulas
        res <- roworderv(.data, cols = ovars, decreasing = .decreasing)
      }
    }

  } else {# .locale != "C", or complex data-masking: fall back to dplyr/stringi
    desc <- as.symbol('desc')
    # If we supply .decreasing=, must transform into desc(var)
    if (!missing(.decreasing)) {
      .decrease <- rep_along(args$dots, .decreasing)
      for (i in 1:length(.decrease)) {
        if (isTRUE(.decrease[i])) {
          x <- args$dots[[i]]
          args$dots[[i]] <- as.call(list(desc, as.symbol(x))) # desc(var)
        }
      }
    }
    # Transform -var into desc(var), since dplyr does not understand -var
    args$dots <- lapply(args$dots, function(x) {
      if (is.call(x) && x[[1]] == '-') {
        x[[1]] <- desc
      } else if (is.character(x)) {# arrange() does not support 'var' here!
        # If we have '-var', we must transform it into desc(var)
        if (startsWith(x, "-")) {
          x <- substring(x, 2L) # Eliminate '-'
          x <- as.call(list(desc, as.symbol(x))) # desc(var)
        } else {
          x <- as.symbol(x) # Just a symbol
        }
      }
      x
    })
    # If we have a GRP_df object, we cannot proceed
    if (inherits(.data, "GRP_df")) {
      if (.locale != "C") {
        stop("Cannot use {.fun arrange_} with \"{(.locale)}\" locale on these data.",
          i = "Either ungroup the data first, or use {.fun group_by} for grouping.")
      } else {# Complex data-masking
        stop("Cannot use {.fun arrange_} with complex data masking on these data.",
          i = "Either ungroup the data first, or use {.fun group_by} for grouping.")
      }
    }
    res <- do.call(arrange, c(list(.data), args$dots,
      list(.by_group = .by_group, .locale = .locale)), envir = args$env)
  }

  #if (to_dtrm)
  #  let_data.table_to_data.trame(res)
  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("dplyr::arrange"))

# TODO: use default .rownames for name?
#' @export
#' @rdname sciviews_functions
pull_ <- structure(function(.data = (.), var = -1, name = NULL, ...) {

  .__top_call__. <- TRUE

  # Implicit data-dot mechanism
  if (missing(.data) || !is.data.frame(.data))
    return(eval_data_dot(sys.call(), arg = '.data', abort_msg =
        gettext("`.data` must be a `data.frame`.")))

  ## For now, we use same function as txxx() counterpart... still must rework
  #if (inherits(.data, c("tbl_db", "dtplyr_step")))
  #  stop("You must collect results before using this function.",
  #    i = "Use {.fun collect_dtx} first.")

  if (!missing(...))
    check_dots_empty()

  if (is_formula(var)) {
    env <- f_env(var)
    var <- f_rhs(var)
    if (is.symbol(var)) {
      var <- as.character(var)
    } else {
      var <- eval(var, envir = env)
    }
  }
  if (is.numeric(var) && var < 0)
    var <- ncol(.data) + 1 + var
  res <- c(.data[[var]])

  if (!is.null(name)) {
    if (is_formula(name)) {
      env <- f_env(name)
      name <- f_rhs(name)
      if (is.symbol(name)) {
        name <- as.character(name)
      } else {
        name <- eval(name, envir = env)
      }
    }
    if (is.numeric(name) && name < 0)
      name <- ncol(.data)  + 1 + name
    names(res) <- c(.data[[name]])
  }
  res
}, class = c("function", "sciviews_fn"), comment = .src_sciviews("dplyr::pull"))

#' @export
#' @rdname sciviews_functions
full_join_ <- structure(function(x, y, by = NULL, suffix = c(".x", ".y"),
copy = FALSE, ...) {

  .__top_call__. <- TRUE

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

  .__top_call__. <- TRUE

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

  .__top_call__. <- TRUE

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

  .__top_call__. <- TRUE

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

  .__top_call__. <- TRUE

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

  .__top_call__. <- TRUE

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

  .__top_call__. <- TRUE

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

  .__top_call__. <- TRUE

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

  .__top_call__. <- TRUE

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

  .__top_call__. <- TRUE

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
distinct_ <- structure(function(.data, ..., .keep_all = FALSE) {

  .__top_call__. <- TRUE

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

  .__top_call__. <- TRUE

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

  .__top_call__. <- TRUE

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

  .__top_call__. <- TRUE

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

  .__top_call__. <- TRUE

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

  .__top_call__. <- TRUE

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

  .__top_call__. <- TRUE

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

  .__top_call__. <- TRUE

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

  .__top_call__. <- TRUE

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

  .__top_call__. <- TRUE

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

  .__top_call__. <- TRUE

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
