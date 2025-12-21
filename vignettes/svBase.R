## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(svBase)

## -----------------------------------------------------------------------------
foo <- structure(function(x, type = c("histogram", "boxplot"), ...) {
  type <- match.arg(type, c("histogram", "boxplot"))
  switch(type,
    histogram = hist(x, ...),
    boxplot = boxplot(x, ...),
    stop("unknow type")
  )
}, class = c("function", "subsettable_type"))
foo

# This function can be used as usual:
foo(rnorm(50), type = "histogram")
# ... but also this way:
foo$histogram(rnorm(50))
foo$boxplot(rnorm(50))

## -----------------------------------------------------------------------------
# Create a regular base R data.frame
dtf <- dtf(
  x = 1:5,
  y = rnorm(5)
)
# Convert it into the preferred version
dtx <- as_dtx(dtf)
class(dtx)

