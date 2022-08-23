#' Alternate assignment (multiple and/or collect results from dplyr)
#'
#' @description These alternate assignment operators can be used to perform
#' multiple assignment (also known as destructuring assignment). These are
#' imported from the {zeallot} package (see the corresponding help page at [zeallot::operator] for complete description). They also performs a [dplyr::collect()] allowing to get results from dplyr extensions like {dtplyr} for data.tables, or {dbplyr} for databases. Finally these two assignment operators also make sure that the preferred data frame object is returned by using [default_dtx()].
#' @param value The object to be assigned.
#' @param x A name, or a name structure for multiple (deconstructing) assignment.
#'
#' @return These operators invisibly return `value`.
#'
#' @details These assignation operator are overloaded to get interesting
#' properties in the context of {tidyverse} pipelines and to make sure to always
#' return our preferred data frame object (data.frame, data.table, or tibble).
#' Thus, before being assigned, `value` is modified by calling [dplyr::collect()] on it and by applying [default_dtx()].
#'
#' @export
#' @name alt_assign
#'
#' @examples
#' # The alternate assignment operator performs three steps:
#' # 1) Collect results from {dbplyr} or {dtplyr}
#' library(dplyr)
#' library(data.table)
#' library(dtplyr)
#' library(svBase)
#' dtt <- data.table(x = 1:5, y = rnorm(5))
#' dtt |>
#'   mutate(x2 = x^2) |>
#'   select(x2, y) ->
#'   res
#'
#' print(res)
#' class(res) # This is NOT a data frame
#'
#' # Same pipeline, but assigning with %->%
#' dtt |>
#'   mutate(x2 = x^2) |>
#'   select(x2, y) %->%
#'   res
#'
#' print(res)
#' class(res) # res is the preferred data frame (data.table by default)
#'
#' # 2) Convert data frame in the chosen format using default_dtx()
#' dtf <- data.frame(x = 1:5, y = rnorm(5))
#' class(dtf)
#' res %<-% dtf
#' class(res) # A data.table by default
#' # but it can be changed with options("SciViews.as_dtx)
#'
#' # 3) If the {zeallot} syntax is used, make multiple assignment
#' c(X, Y) %<-% dtf # Variables of dtf assigned to different names
#' X
#' Y
#'
#' # The %->% is meant to be used in pipelines, otherwise it does the same
`%->%` <- zeallot::`%->%`
body(`%->%`) <- bquote({
  # Collect and possibly transform data frames
  val <- try(dplyr::collect(value), silent = TRUE)
  if (inherits(val, "try-error")) val <- value
  value <- svBase::default_dtx(val)

  # This is the original code in zeallot::`%->%`
  tryCatch(multi_assign(substitute(x), value, parent.frame()),
    invalid_lhs = function(e) {
      stop("invalid `%->%` right-hand side, ", e$message, call. = FALSE)
    }, invalid_rhs = function(e) {
      stop("invalid `%->%` left-hand side, ", e$message, call. = FALSE)
    })
})

#' @export
#' @rdname alt_assign
#' @keywords NULL
`%<-%` <- zeallot::`%<-%`
body(`%<-%`) <- bquote({
  # Collect and possibly transform data frames
  val <- try(dplyr::collect(value), silent = TRUE)
  if (inherits(val, "try-error")) val <- value
  value <- svBase::default_dtx(val)

  # This is the original code in zeallot::`%<-%`
  tryCatch(multi_assign(substitute(x), value, parent.frame()),
    invalid_lhs = function(e) {
      stop("invalid `%<-%` left-hand side, ", e$message, call. = FALSE)
    }, invalid_rhs = function(e) {
      stop("invalid `%<-%` right-hand side, ", e$message, call. = FALSE)
    })
})
