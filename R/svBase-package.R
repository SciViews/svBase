#' Base Objects like Data Frames for 'SciViews::R'
#'
#' The \{svBase\} package sets up the way data frames (with objects like R
#' base's **data.frame**, **data.table** and tibble **tbl_df**) are managed in
#' SciViews::R. The user can select the class of object it uses by default and
#' many other SciViews::R functions return that format. Conversion from one to
#' the other is made easier, including for the management of **data.frame**'s
#' row names or **data.table**'s keys. Also homogeneous ways to create a data
#' frame or to print it are also provided.
#'
#' @section Important functions:
#'
#' - [dtx()] creates a data frame in the preferred format, with the
#' following functions [dtbl()], [dtf()] and [dtt()] that force respectively
#' the creation of a data frame in one of the specified three formats. Use
#' `getOption("SciViews.as_dtx", default = as_dtt)` to specify which function to
#' use to convert into the preferred format.

## usethis namespace: start
#' @importFrom data.table as.data.table haskey key rbindlist setattr setDT setkeyv
#' @importFrom data.trame as.data.trame data.trame is.data.trame let_data.trame_to_data.table let_data.table_to_data.trame
#' @importFrom tibble as_tibble tibble tribble
#' @importFrom svMisc assign_temp eval_data_dot
#' @importFrom dplyr across add_count add_tally arrange bind_cols collect count distinct filter full_join group_by inner_join is_grouped_df left_join mutate pull rename rename_with right_join select summarise tally transmute ungroup
#' @importFrom tidyr drop_na extract fill pivot_longer pivot_wider replace_na separate separate_rows uncount unite
#' @importFrom collapse allNA ckmatch fcount fnobs fselect fsummarise fmutate frename fsubset fgroup_vars fungroup get_collapse group_by_vars replace_NA ss fgroup_by
#' @importFrom rlang abort caller_env check_dots_empty0 eval_bare f_env f_name f_lhs f_rhs inject is_formula warn
# @importFrom pillar tbl_sum
#' @import zeallot
## usethis namespace: end
"_PACKAGE"
