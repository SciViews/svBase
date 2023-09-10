#' Force computation of a lazy tidyverse object
#'
#' @description When {dplyr} or {tidyr} verbs are applied to a **data.table** or a database connection, they do not output data frames but objects like **dtplyr_step** or **tbl_sql** that are called lazy data frames. The actual process is triggered by using [as_dtx()], or more explicitly with [dplyr::collect()] which coerces the result to a **tibble**. If you want the default {svBase} data frame object instead, use [collect_dtx()], or if you want a specific object, use one of the other variants.
#'
#' @param x A data.frame, data.table, tibble or a lazy data frame (dtplyr_step, tbl_sql, ...).
#' @param ... Arguments passed on to methods for [dplyr::collect()].
#'
#' @return A data frame (data.frame, data.table or tibble's tbl_df), the default version for [collect_dtx()].
#' @export
#'
#' @examples
#' # Assuming the default data frame for {svBase} is a data.table
#' mtcars_dtt <- as_dtt(mtcars)
#' library(dplyr)
#' library(dtplyr)
#' # A lazy data frame, not a "real" data frame!
#' mtcars_dtt |> lazy_dt() |> select(mpg:disp) |> class()
#' # A data frame
#' mtcars |> select(mpg:disp) |> class()
#' # A data table
#' mtcars_dtt |> select(mpg:disp) |> class()
#' # A tibble, always!
#' mtcars_dtt |> lazy_dt() |> select(mpg:disp) |> collect() |> class()
#' # The data frame object you want, default one specified for {svBase}
#' mtcars_dtt |> lazy_dt() |> select(mpg:disp) |> collect_dtx() |> class()
collect_dtx <- function(x, ...) {
  as_dtx(collect(x, ...))
}

#' @export
#' @rdname collect_dtx
collect_dtf <- function(x, ...) {
  as_dtf(collect(x, ...))
}

#' @export
#' @rdname collect_dtx
collect_dtt <- function(x, ...) {
  as_dtt(collect(x, ...))
}

#' @export
#' @rdname collect_dtx
collect_dtbl <- function(x, ...) {
  collect(x, ...)
}
