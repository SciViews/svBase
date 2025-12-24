## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(svBase)

## -----------------------------------------------------------------------------
capabilities("NLS")

## -----------------------------------------------------------------------------
get_language() # Current R session language
get_sciviews_lang() # Current alternate language for gettext_()...

## -----------------------------------------------------------------------------
olang <- set_language("de")
olang2 <- set_sciviews_lang("fr")
# Check
get_language()
get_sciviews_lang()

## ----eval=FALSE---------------------------------------------------------------
# 1:2 + 1:3

## ----echo=FALSE, warning=TRUE-------------------------------------------------
suppressWarnings(1:2 + 1:3)
message("Warning in 1:2 + 1:3: Länge des längeren Objektes\n   ist kein Vielfaches der Länge des kürzeren Objektes")

## ----eval=FALSE---------------------------------------------------------------
# nonexisting

## ----echo=FALSE, error=TRUE---------------------------------------------------
try({
base::stop("Objekt 'nonexisting' nich gefunden")
})

## ----eval=FALSE---------------------------------------------------------------
# gettext_("Test of svBase's `gettext()` and `gettextf()`:", domain = "R-svBase")

## ----echo=FALSE---------------------------------------------------------------
"Test des fonctions `gettext()` et `gettextf()` de svBase :"

## ----eval=FALSE---------------------------------------------------------------
# gettext_("Test of svBase's `gettext()` and `gettextf()`:", domain = "R-svBase",
#   lang = "en_US")

## ----echo=FALSE---------------------------------------------------------------
"Test of svBase's `gettext()` and `gettextf()`:"

## -----------------------------------------------------------------------------
gettext <- gettext_
gettextf <- gettextf_
ngettext <- ngettext_

## ----eval=FALSE---------------------------------------------------------------
# test_that("The .po translation files are up to date", {
#   skip_on_cran()
#   skip_on_ci()
#   # Update .po and .mo files (only test in the source package, not R CMD check)
#   if (file.exists("../../DESCRIPTION")) {# This is the source of the package
#     cat("\nCompiling .po files...\n")
#     res <- try(tools::update_pkg_po("../.."), silent = TRUE)
#     expect_false(inherits(res, "try-error"),
#       "Updating .po files failed. Run tools::update_pkg_po() manually to debug.")
#   }
# })

## -----------------------------------------------------------------------------
ngettext(1, "You asked for only one item", "You asked for several items",
  domain = "R-svBase/fr") # Using svBase::ngettext_(), renamed ngettext() above

## -----------------------------------------------------------------------------
set_language(olang)
set_sciviews_lang(olang2)

## -----------------------------------------------------------------------------
# Use svBase stop_() and warning_(), but renamed
# in your package (don't export stop and warning)
stop <- stop_
warning <- warning_

## ----error=TRUE---------------------------------------------------------------
try({
stop("You shouldn't end up here.")
})

## ----error=TRUE---------------------------------------------------------------
try({
base::stop("You shouldn't end up here.")
})

## ----error=TRUE---------------------------------------------------------------
try({
# A simple function that raises an error
err_fun <- function() {
  stop("You shouldn't end up here.")
}
err_fun()
})

## -----------------------------------------------------------------------------
# Classed error message
err_fun <- function() {
  stop("You shouldn't end up here.", class = "my_error_class")
}

## ----eval=FALSE---------------------------------------------------------------
# expect_error(err_fun(), class = "my_error_class")

## ----error=TRUE---------------------------------------------------------------
try({
# An enhanced error message with formatting
decrement <- function(x) {
  if (!is.numeric(x))
    stop("{.var x} must be a numeric vector.",
      i = "You've supplied a {.cls {class(x)}}.",
      class = "x_not_numeric")
  x - 1
}
decrement("a string")
})

## ----error=TRUE---------------------------------------------------------------
try({
# Trying to use our decrement() function on a data frame
df <- dtx(x = 1:5, y = rnorm(5))
decrement(df)

# Idem, but when providing the argument as `.`
.= df
decrement(.)
})

## ----error=TRUE---------------------------------------------------------------
try({
# A data-dot function
my_head <- function(.data = (.), rows = 6L) {
  # This makes it a data-dot function
  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())
  
  # Checking rows (note, for simplicity, we consider data has several rows)
  if (!is.numeric(rows) || length(rows) != 1L ||
      rows < 1 || rows > nrow(.data))
    stop("Incorrect {.arg rows} argument.",
      i = "You must provide a single integer between 1 and {nrow(.data)}.",
      class = "rows_wrong_value")
  
  .data[1:rows, ]
}
my_head(df, 2L) # OK
my_head(df, -1L) # Error
})

## ----error=TRUE---------------------------------------------------------------
try({
.= df
my_head(2L) # OK
my_head(-1L) # Error message with additional info for data-dot
})

## ----error=TRUE---------------------------------------------------------------
try({
check_rows <- function(x, arg = "x", max_value) {
  if (!is.numeric(x) || length(x) != 1L ||
      x < 1 || x > max_value)
    stop("Incorrect {.arg {arg}} argument.",
      i = "You must provide a single integer between 1 and {max_value}.",
      class = "rows_wrong_value")
}

my_head <- function(.data = (.), rows = 6L) {
  # This makes it a data-dot function
  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())
  
  check_rows(rows, "rows", nrow(.data))

  .data[1:rows, ]
}
my_head(df, 10L) # Error
})

## ----error=TRUE---------------------------------------------------------------
try({
check_rows2 <- function(x, arg = "x", max_value) {
  if (!is.numeric(x) || length(x) != 1L ||
      x < 1 || x > max_value)
    stop(call = environment(),
      "Incorrect {.arg {arg}} argument.",
      i = "You must provide a single integer between 1 and {max_value}.",
      class = "rows_wrong_value")
}

my_head2 <- function(.data = (.), rows = 6L) {
  # This makes it a data-dot function
  if (!prepare_data_dot(.data))
    return(recall_with_data_dot())
  
  check_rows2(rows, "rows", nrow(.data))

  .data[1:rows, ]
}
my_head2(df, 10L) # Error
})

## ----error=TRUE---------------------------------------------------------------
try({
my_fun <- function(x, rows, ...) {
  my_head(x, rows = rows)
}
my_fun(df, 10L) # Error
})

## ----error=TRUE---------------------------------------------------------------
try({
my_fun <- function(x, rows, ...) {
  .__top_call__. <- TRUE
  
  my_head(x, rows = rows)
}
my_fun(df, 10L) # Error
})

