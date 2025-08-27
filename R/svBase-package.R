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

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
# @importFrom pillar tbl_sum
#' @import zeallot
#' @importFrom compiler cmpfun
#' @importFrom cli cli_abort
#' @importFrom collapse anyv fcount fgroup_by fgroup_vars fmatch fmutate fnobs frename fselect fsubset fsummarise fungroup qDF qDT qTBL replace_na
#' @importFrom data.table as.data.table key rbindlist setattr setDT setkeyv
#' @importFrom data.trame as.data.trame data.trame is.data.trame
#' @importFrom dplyr across add_count add_tally anti_join arrange bind_cols bind_rows collect count distinct filter full_join group_by inner_join is_grouped_df left_join mutate pull rename rename_with semi_join right_join select summarise tally transmute ungroup
#' @importFrom lifecycle deprecated
#' @importFrom rlang abort eval_bare expr_text f_env f_lhs f_name f_rhs inject is_formula rep_along
#' @importFrom svMisc expr_funs
#' @importFrom utils .DollarNames apropos
#' @importFrom tibble as_tibble is_tibble tibble tribble
#' @importFrom tidyr drop_na extract fill pivot_longer pivot_wider separate separate_rows uncount unite
## usethis namespace: end
"_PACKAGE"

#importFrom tidyselect eval_select
#importFrom utils type.convert
