.onload <- function(lib, pkg) {# nocov start
  # Allow implicit (dot) data by default
  assign_temp('.SciViews.implicit.data', TRUE)
}# nocov end

#.onUnload <- function(libpath) {
#  # Do nothing for now
#}

# Also define it here
.SciViews.implicit.data <- TRUE

# Global definition of `.` for the package
. <- NULL
