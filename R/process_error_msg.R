# Useful to remplace error messages by better versions
#' Process error message (replace it with a better version)
#'
#' @description
#' The `tra` list indicates which (partial) error message should be replaced
#' by a more elaborate version, using the [cli::cli_abort()] syntax.
#'
#' @param msg The original error message.
#' @param tra The translation list, where the names are the partial error and
#'   the content is the new error message(s).
#' @param fixed Should a fixed or a regular expression comparison be done
#'   between the names of `tra` and the `msg`? Defaults to `TRUE`, which is
#'   faster. In case you need different behavior for different items, you can
#'   also supply a logical vector of the same length as `tra`.
#'
#' @returns The replaced error message.
#' @export
#'
#' @examples
#' dir <- "/temp/dir"
#' file <- "~/some file.txt"
#' tra = list(
#'  "Directory not found" = c("The directory {.path {dir}} does not exist.",
#'    i = "Please create it first."),
#'  "file does not exist" = c("The file {.file {file}} does not exist.",
#'    i = "Check the path and try again.")
#' )
#' msg <- "The file does not exist"
#' try(cli::cli_abort(process_error_msg(msg, tra)))
#'
#' msg <- "Directory not found on this machine"
#' try(cli::cli_abort(process_error_msg(msg, tra)))
process_error_msg <- function(msg, tra, fixed = TRUE) {
  fixed <- rep_along(tra, as.logical(fixed))
  rex <- names(tra)
  res <- msg
  for (i in 1:length(tra)) {
    if (grepl(rex[i], msg, fixed = fixed[i], perl = !fixed[i])) {
      res <- tra[[i]]
      break
    }
  }
  res
}
