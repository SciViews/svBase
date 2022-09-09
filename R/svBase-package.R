#' @details
#' The {svBase} package sets up the way data frames (with objects like R base's **data.frame**, **data.table** and tibble **tbl_df**) are managed in SciViews. The user can select the class of object it uses by default and many other SciViews functions return that format. Also conversion from one to the other is smoothed, including for the management of **data.frame**'s row names or **data.table**'s keys. Also homogeneous ways to create a data frame or to print it are also provided.
#'
#' @section Important functions:
#'
#' - [dtx()] creates a data frame in the preferred format, with the
#' following functions [dtbl()], [dtf()] and [dtt()] that force respectively
#' the creation of a data frame in one of the specified three formats. Use
#' `getOption("SciViews.as_dtx", default = as_dtt)` to specify which function to
#' use to convert into the preferred format.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom data.table as.data.table haskey key setDT setkeyv
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr collect filter full_join group_by inner_join left_join mutate rename rename_with right_join select summarise transmute ungroup
#' @importFrom collapse fselect fsummarise fmutate frename fsubset fgroup_by fungroup
#' @importFrom pillar tbl_sum
#' @import zeallot
# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
