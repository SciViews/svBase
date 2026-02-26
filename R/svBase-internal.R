.onLoad <- function(lib, pkg) {# nocov start
  # Allow implicit (dot) data by default
  # These two functions are redefined here to avoid a dependency to svMisc
  .temp_env <- function() {
    pos <-  match("SciViews:TempEnv", search())
    if (is.na(pos)) { # Must create it
      `SciViews:TempEnv` <- list()
      attach_env <- function(...) get("attach", mode = "function")(...)
      attach_env(`SciViews:TempEnv`, pos = length(search()) - 1L)
      rm(`SciViews:TempEnv`)
      pos <- match("SciViews:TempEnv", search())
    }
    pos.to.env(pos)
  }

  .assign_temp <- function(x, value, replace.existing = TRUE) {
    t_env <- .temp_env()
    if (replace.existing || !exists(x, envir = t_env, mode = "any",
      inherits = FALSE))
      assign(x, value, envir = t_env)
  }
  .assign_temp('.SciViews.implicit.data.dot', TRUE)
}# nocov end

#.onUnload <- function(libpath) {
#  # Do nothing for now
#}

# Also define it here... temporarily for peaceful R CMD check
.zap <- function(...) rm(list = c(...), envir = parent.frame())
.SciViews.implicit.data.dot <- TRUE
.zap('.SciViews.implicit.data.dot')

# Need this for R CMD check to pass
. <- NULL
.zap('.')

# Internal options
.op <- new.env()
.op$verbose <- FALSE

# We use our own stop_() and warning_(), but renamed
stop <- stop_
warning <- warning_

# Similar to glue::glue() but using {{var}} instead of {var} and allowing only
# variable names inside {{ }}, not expressions. Also it eliminates ~ in front of
# var content in case it contians a right-sided formula like var <- ~expr
# It is 5 to 10x faster than glue::glue() and is used to resolve variable names
# in constructs like mutate("my_{{var}}" := x^2)
.glue <- function(str, env = parent.frame()) {
  # Extract all variable names form the string
  str <- strsplit(str, "{{", fixed = TRUE)[[1]]
  str <- unlist(strsplit(str, "}}", fixed = TRUE))
  # If there is nothing to replace, return the string
  if (length(str) < 2) return(str)
  # Now, even items are names of variables to be replaced
  vars <- as.character(mget(str[c(FALSE, TRUE)], envir = env))
  # Eliminate ~ from formulas in front of vars
  is_tilde <- startsWith(vars, "~")
  if (any(is_tilde))
    vars[is_tilde] <- substring(vars[is_tilde], 2)
  str[c(FALSE, TRUE)] <- vars
  paste(unlist(str), sep = "", collapse = "")
}

# This is rlang::check_required(), but modified for translatable errors
#check_required <- function(x) {
#  if (missing(x))
#    stop("{.arg {substitute(x)}} is absent but must be supplied.",
#      class = "missing_argument", call = stop_top_call(2L))
#  invisible(TRUE)
#}
