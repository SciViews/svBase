#.onload <- function(lib, pkg) {# nocov start
#  # Nothing to do for now
#}# nocov end

#.onUnload <- function(libpath) {
#  # Do nothing for now
#}

# Need this for R CMD check to pass
. <- NULL

# We use our own stop_() and warning_(), but renamed
stop <- svMisc::stop_
warning <- svMisc::warning_

# Process ... -> could be a list of formulas, formulas, or SE expressions
formula_masking <- function(..., .max.args = NULL, .must.be.named = FALSE,
    .make.names = FALSE, .no.se = FALSE,
    .no.se.msg = gettext("Standard evaluation is not allowed."),
    .envir = parent.frame(2L), .frame = parent.frame()) {

  .__top_call__. <- TRUE
  .make.names <- isTRUE(.make.names)

  # If we have a list of formulas, use it instead of ...
  if (is.list(..1) && is_formula(..1[[1]])) {
    if (...length() > 1L)
      stop("If you provide a list of formulas, you cannot provide more.")
    dots <- ..1
    first_item <- ..1[[1]]
    ldots <- length(dots)

  } else {# Use the regular ... arguments
    dots <- list(...) # Use substitute(...()) instead for not evaluating ...
    first_item <- ..1
    ldots <- ...length()
  }

  # Check number or arguments
  if (!is.null(.max.args)) {
    if (!is.numeric(.max.args) || length(.max.args) != 1 || .max.args < 1)
      stop("{.arg .max.args} must be a single positive {.cls integer}, not {.max.args}.")
    # Check that the number of arguments is not too large
    if (ldots > .max.args)
      stop("You provided {ldots} arguments, but max allowed is {.max.args}.")
  }

  are_formulas <- is_formula(first_item)
  if (are_formulas) {# Everything is supposed to be formulas
    # The environment where to evaluate them is extracted from the first formula
    .envir <- f_env(first_item)

    # Extract expressions from the rhs of the formulas
    # Formulas are converted into expressions in extracting the right-hand side
    # (and if there are left-hand side, they become the name)
    f_to_expr <- function(x) {
      if (is_formula(x)) {
        f_rhs(x)
      } else {
        stop("You cannot mix standard evaluation and formulas.")
      }
    }
    if (ldots == 1) {
      args <- dots
      args[[1]] <- f_to_expr(first_item)
    } else {
      args <- lapply(dots, f_to_expr)
    }

    # Check names and possibly get names also from the formulas
    names_args <- names(args)
    if (is.null(names_args))
      names_args <- rep("", ldots)
    for (i in 1:ldots) {
      dots_i <- dots[[i]]
      lhs <- f_lhs(dots_i)
      if (!is.null(lhs) && names_args[i] == "")
        names_args[i] <- eval_bare(lhs, env = .envir)

      # Do we need everything named?
      if (isTRUE(.must.be.named) && anyv(names_args, ""))
        stop("All inputs must be named.")

      # Functions like fsummarise() do not support empty names!
      # In this case, construct a label from rhs of the formula
      # Note: special case: if across() is used, do not name it!
      if (.make.names && names_args[i] == "" &&
          !anyv(expr_funs(dots_i), "across"))
        names_args[i] <- f_name(dots_i)
    }
    names(args) <- names_args

  } else {# Standard evaluation
    if (isTRUE(.no.se))
      stop(.no.se.msg, i = "Use formulas instead.")

    if (any(sapply(dots, is_formula)))
      stop("You cannot mix standard evaluation and formulas.")

    # Not needed, already done!?
    #args <- lapply(dots, force) # Force SE of the arguments
    args <- dots
  }

  # Return args and a little bit of info in a list
  list(dots = args, env = .envir, are_formulas = are_formulas)
}

# formula_select is the equivalent of tidy-select, but using formulas
# There is a fast subset of tidy-select handled by {collapse}, but for more
# complex cases, we rely on tidyselect::eval_select().
formula_select <- function(..., .fast.allowed.funs = NULL,
    .max.args = NULL, .must.be.named = FALSE, .make.names = FALSE,
    .no.se = FALSE, .no.se.msg = gettext("Standard evaluation is not allowed."),
    .envir = parent.frame(2L), .frame = parent.frame()) {

  .__top_call__. <- TRUE

  # Process the arguments with formula_masking()
  args <- formula_masking(..., .max.args = .max.args,
    .must.be.named = .must.be.named,.make.names = .make.names,
    .no.se = .no.se, .no.se.msg = .no.se.msg, .envir = .envir)

  # For SE, always OK to use fast-select
  if (!args$are_formulas || !length(.fast.allowed.funs)) {
    args$fastselect <- TRUE
  } else {# For formulas, check that all arguments are fast-selectable
    is_arg_ok <- function(arg, .fast.allowed.funs)
      !length(expr_funs(arg, exclude.names = .fast.allowed.funs))

    if (length(args$dots) == 1L) {
      args$fastselect <- is_arg_ok(args$dots[[1]],
        .fast.allowed.funs = .fast.allowed.funs)
    } else {
      args$fastselect <- all(sapply(args$dots, is_arg_ok,
        .fast.allowed.funs = .fast.allowed.funs))
    }
  }
  args
}
