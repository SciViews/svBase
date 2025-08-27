#.onload <- function(lib, pkg) {# nocov start
#  # Nothing to do for now
#}# nocov end

#.onUnload <- function(libpath) {
#  # Do nothing for now
#}

# Need this for R CMD check to pass
. <- NULL

# Internal options
.op <- new.env()
.op$verbose <- FALSE

# We use our own stop_() and warning_(), but renamed
stop <- svMisc::stop_
warning <- svMisc::warning_
