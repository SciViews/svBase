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

# This is rlang::check_required(), but modified for translatable errors
#check_required <- function(x) {
#  if (missing(x))
#    stop("{.arg {substitute(x)}} is absent but must be supplied.",
#      class = "missing_argument", call = stop_top_call(2L))
#  invisible(TRUE)
#}
