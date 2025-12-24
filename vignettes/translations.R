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
# gettext_("Test of svBase's `gettext()` and `gettextf()`:", domain = "R-svBase",
#   lang = "fr")

## ----echo=FALSE---------------------------------------------------------------
"Test des fonctions `gettext()` et `gettextf()` de svBase :"

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
hist(rnorm(500))

## -----------------------------------------------------------------------------
my_hist <- function(x, ..., ylab = NULL, main = NULL, lang = get_sciviews_lang()) {
  hist(x, ...,
    ylab = ylab %||% gettext("Frequency", lang = lang),
    main = main %||% gettextf("Histogram of %s", deparse(substitute(x)), lang = lang))
}

## ----eval=FALSE---------------------------------------------------------------
# my_hist(rnorm(500))

## ----echo=FALSE---------------------------------------------------------------
my_hist_fr <- function(x, ..., ylab = NULL, main = NULL, lang = get_sciviews_lang()) {
  hist(x, ...,
    ylab = ylab %||% "Fréquence",
    main = main %||% sprintf("Histogramme de %s", deparse(substitute(x))))
}
my_hist_fr(rnorm(500))

## ----eval=FALSE---------------------------------------------------------------
# my_hist(rnorm(500), lang = "it")

## ----echo=FALSE---------------------------------------------------------------
my_hist_it <- function(x, ..., ylab = NULL, main = NULL, lang = get_sciviews_lang()) {
  hist(x, ...,
    ylab = ylab %||% "Frequenza",
    main = main %||% sprintf("Istogramma di %s", deparse(substitute(x))))
}
my_hist_it(rnorm(500), lang = "it")

## -----------------------------------------------------------------------------
set_language(olang)
set_sciviews_lang(olang2)

