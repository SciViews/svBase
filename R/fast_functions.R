#' Fast (flexible and friendly) statistical functions (mainly from collapse) for matrix-like and data frame objects
#'
#' @description The fast statistical function, or fast-flexible-friendly
#' statistical functions are prefixed with "f". These vectorized functions
#' supersede the no-f functions, bringing the capacity to work smoothly on
#' matrix-like and data frame objects. Most of them are defined in the
#' \{collapse\} package
#' For instance, base [mean()] operates on a vector, but not on a data frame. A
#' matrix is recognized as a vector and a single mean is returned. On, the
#' contrary, [fmean()] calculates one mean per column. It does the same for a
#' data frame, and it does so usually quicker than base functions. No need for
#' `colMeans()`, a separate function to do so. Fast statistical functions also
#' recognize grouping with [fgroup_by()], [sgroup_by()] or [group_by()] and
#' calculate the mean by group in this case. Again, no need for a different
#' function like [stats::ave()].
#' Finally, these functions also have a `TRA=` argument that computes, for
#' instance, if `TRA = "-"`, `(x  f(x))` very efficiently (for instance to
#' calculate residuals by subtracting the mean).
#' Another particularity is the `na.rm=` argument that is `TRUE` by default,
#' while it is `FALSE` by default for [mean()].
#' These are generic functions with methods for **matrix**, **data.frame**,
#' **grouped_df** and a **default** method used for simple numeric vectors. Most
#' of them are defined in the \{collapse\} package, but there are a couple more
#' here, together with an alternate syntax to replace `TRA=` with `%_f%`.
#'
#' @param x A numeric vector, matrix, data frame or grouped data frame (class
#'   'grouped_df').
#' @param ... Further arguments passed to the method, like `w=`, a numeric
#'   vector of (non-negative) weights that may contain missing values, or
#'   `TRA=`, a quoted operator indicating the transformation to perform:
#'   `"replace"` to get a vector of same size of `x` with results,
#'   `"replace_fill"` idem but also replace missing data, `"-"` to subtract,
#'   `"+"` to add, `"-+"` to subtract and add the global statistic, `"/"`
#'   to divide, `"%"` to divide and multiply by 100 (percent), `"*"` to
#'   multiply, `"%%"` to take the modulus (remainder from division by the
#'   statistic) and `"-%%"` to subtract modulus ('i.e., to floor the data by the
#'   statistic), see [collapse::TRA()]. Also `na.rm=`, a logical indicating if
#'   we skip missing values in `x` if `TRUE`(by default). If `FALSE` for any
#'   missing data in `x`, `NA`is returned. For details and other arguments,
#'   see the corresponding help page in the collapse package.
#' @param expr The expression to evaluate as RHS of the `%__f%` operators.
#'
#' @note The page [collapse::fast-statistical-functions] gives more details.
#' [fn()] count all observations, including `NA`s, [fna()] counts
#' only `NA`s, where [fnobs()] counts non-missing observations.
#' Instead of `TRA=` one can use the `%__f%` functions where `__` is `replace`,
#' `replace_fill`, `-`, `+`, `-+`, `/`, `/*100` for `TRA="%"`, `*`, `mod` for
#' `TRA="%%"`, or `-mod` for `TRA="-%%"`. See example.
#[fquantile()] corresponds to [collapse::fnth()] where `n < 1`.
#'
#' @return The number of all observations for [fn()] or the number of
#' missing observations for [fna()]. [list_fstat_functions()] returns a list of
#' all the known fast statistical functions.
#'
#' @export
#' @name fstat_functions
#'
#' @examples
#' library(collapse)
#' data(iris)
#' iris_num <- iris[, -5] # Only numerical variables
#' mean(iris$Sepal.Length) # OK, but mean(iris_num does not work)
#' colMeans(iris_num)
#' # Same
#' fmean(iris_num)
#' # Idem, but mean by group for all 4 numerical variables
#' iris |> fgroup_by(Species) |> fmean()
#' # Residuals (x - mean(x)) by group
#' iris |> fgroup_by(Species) |> fmean(TRA = "-")
#' # The same calculation, in a little bit more expressive way
#' iris |> fgroup_by(Species) %-f% fmean()
#' # or:
#' iris_num %-f% fmean(g = iris$Species)
list_fstat_functions <- function() {
  c("ffirst", "flast", "fmax", "fmean", "fmedian", "fmin", "fmode",
    "fndistinct", "fnobs", "fn", "fna", "fnth", "fprod", #"fquantile",
    "fsd", "fsum", "fvar")
}

#.src_fast <- function(src, comment = "A fast (flexible and friendly) function, see ?fstat_functions.") {
#  attr(comment, "src") <- src
#  comment
#}

# #' @export
# #' @rdname fstat_functions
#fsum <- structure(collapse::fsum,
#  class = c("function", "fast_fn"), comment = .src_fast("collapse::fsum"))

# #' @export
# #' @rdname fstat_functions
#fprod <- structure(collapse::fprod,
#  class = c("function", "fast_fn"), comment = .src_fast("collapse::fprod"))

# #' @export
# #' @rdname fstat_functions
#fmean <- structure(collapse::fmean,
#  class = c("function", "fast_fn"), comment = .src_fast("collapse::fmean"))

# #' @export
# #' @rdname fstat_functions
#fmedian <- structure(collapse::fmedian,
#  class = c("function", "fast_fn"), comment = .src_fast("collapse::fmedian"))

# #' @export
# #' @rdname fstat_functions
#fmode <- structure(collapse::fmode,
#  class = c("function", "fast_fn"), comment = .src_fast("collapse::fmode"))

# #' @export
# #' @rdname fstat_functions
#fvar <- structure(collapse::fvar,
#  class = c("function", "fast_fn"), comment = .src_fast("collapse::fvar"))

# #' @export
# #' @rdname fstat_functions
#fsd <- structure(collapse::fsd,
#  class = c("function", "fast_fn"), comment = .src_fast("collapse::fsd"))

# #' @export
# #' @rdname fstat_functions
#fmin <- structure(collapse::fmin,
#  class = c("function", "fast_fn"), comment = .src_fast("collapse::fmin"))

# #' @export
# #' @rdname fast_stat_functions
#fmax <- structure(collapse::fmax,
#  class = c("function", "fast_fn"), comment = .src_fast("collapse::fmax"))

# #' @export
# #' @rdname fstat_functions
#fnth <- structure(function(x, n = 1, ...) {
#  if (length(n) != 1 || n < 0)
#    stop("n must be a scalar integer between 1 and NROW(x) or 0 < n < 1.")
#  if (n == 1) {
#    collapse::ffirst(x, ...)
#  } else {
#    collapse::fnth(x, n = n, ...)
#  }
#},  class = c("function", "fast_fn"), comment = .src_fast("collapse::fnth"))

# A version with probabilities, calculating something like a quantile, but it
# does not exactly returns a quantile as R calculates it with quantile()
# TODO: finalize it with the correct calculation
#fquantile <- structure(function(x, prob = 0.5, ...) {
#  if (length(prob) != 1 || prob < 0 || prob > 1)
#    stop("prob must be a scalar number between 0 and 1.")
#  if (prob == 0) {
#    collapse::ffirst(x, ...)
#  } else if (prob == 1) {
#    collapse::flast(x, ...)
#  } else {
#    collapse::fnth(x, n = prob, ...)
#  }
#}, class = c("function", "fast_fn"), comment = .src_fast("collapse::fnth"))

# #' @export
# #' @rdname fstat_functions
#ffirst <- structure(collapse::ffirst,
#  class = c("function", "fast_fn"), comment = .src_fast("collapse::ffirst"))

# #' @export
# #' @rdname fstat_functions
#flast <- structure(collapse::flast,
#  class = c("function", "fast_fn"), comment = .src_fast("collapse::flast"))

# #' @export
# #' @rdname fstat_functions
#fnobs <- structure(collapse::fnobs,
#  class = c("function", "fast_fn"), comment = .src_fast("collapse::fnobs"))

#' @export
#' @rdname fstat_functions
fn <- function(x, ...) {
  fnobs(replace_NA(x), ...)
}

#' @export
#' @rdname fstat_functions
fna <- function(x, ...) {
  fnobs(replace_NA(x), ...) - fnobs(x, ...)
}

# #' @export
# #' @rdname fstat_functions
#fndistinct <- structure(collapse::fndistinct,
#  class = c("function", "fast_fn"), comment = .src_fast("collapse::fndistinct"))

#' @export
#' @rdname fstat_functions
`%replacef%` <- function(x, expr) {
  expr <- substitute(expr)
  expr$x <- x
  expr$TRA <- "replace"
  eval.parent(expr, n = 2)
}

#' @export
#' @rdname fstat_functions
`%replace_fillf%` <- function(x, expr) {
  expr <- substitute(expr)
  expr$x <- x
  expr$TRA <- "replace_fill"
  eval.parent(expr, n = 2)
}

#' @export
#' @rdname fstat_functions
`%-f%` <- function(x, expr) {
  expr <- substitute(expr)
  expr$x <- x
  expr$TRA <- "-"
  eval.parent(expr, n = 2)
}

#' @export
#' @rdname fstat_functions
`%+f%` <- function(x, expr) {
  expr <- substitute(expr)
  expr$x <- x
  expr$TRA <- "+"
  eval.parent(expr, n = 2)
}

#' @export
#' @rdname fstat_functions
`%-+f%` <- function(x, expr) {
  expr <- substitute(expr)
  expr$x <- x
  expr$TRA <- "-+"
  eval.parent(expr, n = 2)
}

#' @export
#' @rdname fstat_functions
`%/f%` <- function(x, expr) {
  expr <- substitute(expr)
  expr$x <- x
  expr$TRA <- "/"
  eval.parent(expr, n = 2)
}

#' @export
#' @rdname fstat_functions
`%/*100f%` <- function(x, expr) {
  expr <- substitute(expr)
  expr$x <- x
  expr$TRA <- "%"
  eval.parent(expr, n = 2)
}

#' @export
#' @rdname fstat_functions
`%*f%` <- function(x, expr) {
  expr <- substitute(expr)
  expr$x <- x
  expr$TRA <- "*"
  eval.parent(expr, n = 2)
}

#' @export
#' @rdname fstat_functions
`%modf%` <- function(x, expr) {
  expr <- substitute(expr)
  expr$x <- x
  expr$TRA <- "%%"
  eval.parent(expr, n = 2)
}

#' @export
#' @rdname fstat_functions
`%-modf%` <- function(x, expr) {
  expr <- substitute(expr)
  expr$x <- x
  expr$TRA <- "-%%"
  eval.parent(expr, n = 2)
}

# #' @export
# #' @rdname fstat_functions
#fgroup_by <- function(x, ...)
#  collapse::fgroup_by(x, ...)
