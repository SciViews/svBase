test_that("The .po files are up to date", {
  skip_on_cran()
  skip_on_ci()
  # Update .po and .mo files (only test in the source package, not R CMD check)
  if (file.exists("../../DESCRIPTION")) {# This is the source of the package
    cat("\nCompiling .po files...\n")
    res <- try(tools::update_pkg_po("../.."), silent = TRUE)
    expect_false(inherits(res, "try-error"),
      "Updating .po files failed. Run tools::update_pkg_po() manually to debug.")
  }
})

test_that("gettext_(), gettextf_() and ngettext_() behave like their base equivalent", {
  # Update .po and .mo files (only test in the source package, not R CMD check)
  if (file.exists("../../DESCRIPTION")) {# This is the source of the package
    cat("\nCompiling .po files...\n")
    res <- try(tools::update_pkg_po("../.."), silent = TRUE)
  }

  # Test with English language first
  old_lang <- Sys.setLanguage("en")

  gettext <- gettext_
  gettextf <- gettextf_
  ngettext <- ngettext_

  expect_identical(
    gettext("Test of svBase's `gettext()` and `gettextf()`:",
      "This should be transtlated, if '%s' language is supported.",
      domain = "R-svBase"),
    base::gettext("Test of svBase's `gettext()` and `gettextf()`:",
      "This should be transtlated, if '%s' language is supported.",
      domain = "R-svBase")
  )
  expect_identical(
    gettextf("This is message number %i", 1L,
      domain = "R-svBase"),
    base::gettextf("This is message number %i", 1L,
      domain = "R-svBase")
  )
  expect_identical(
    ngettext(0,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase"),
    base::ngettext(0,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase")
  )
  expect_identical(
    ngettext(1,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase"),
    base::ngettext(1,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase")
  )
  expect_identical(
    ngettext(2,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase"),
    base::ngettext(2,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase")
  )

  # Test with French translations
  Sys.setLanguage("fr")

  expect_identical(
    gettext("Test of svBase's `gettext()` and `gettextf()`:",
      "This should be transtlated, if '%s' language is supported.",
      domain = "R-svBase"),
    base::gettext("Test of svBase's `gettext()` and `gettextf()`:",
      "This should be transtlated, if '%s' language is supported.",
      domain = "R-svBase")
  )
  expect_identical(
    gettextf("This is message number %i", 1L,
      domain = "R-svBase"),
    base::gettextf("This is message number %i", 1L,
      domain = "R-svBase")
  )
  expect_identical(
    ngettext(0,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase"),
    base::ngettext(0,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase")
  )
  expect_identical(
    ngettext(1,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase"),
    base::ngettext(1,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase")
  )
  expect_identical(
    ngettext(2,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase"),
    base::ngettext(2,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase")
  )

  # Strange, but correct, arguments
  expect_identical(
    gettext("a", character(0), "", NULL, 1),
    base::gettext("a", character(0), "", NULL, 1)
  )
  expect_identical(
    gettext(" a\n", trim = FALSE),
    base::gettext(" a\n", trim = FALSE)
  )

  # Wrong arguments
  # Wrong domain
  # TODO...

  Sys.setLanguage(old_lang)
})

test_that("gettext_(), gettextf_() and ngettext_() can use lang=", {
  # Make sure R is in English language
  old_lang <- Sys.setLanguage("en")

  gettext <- gettext_
  gettextf <- gettextf_
  ngettext <- ngettext_

  # Getting English messages, using lang= argument
  expect_identical(
    gettext("Test of svBase's `gettext()` and `gettextf()`:",
      "This should be transtlated, if '%s' language is supported.",
      domain = "R-svBase", lang = "en"),
    base::gettext("Test of svBase's `gettext()` and `gettextf()`:",
      "This should be transtlated, if '%s' language is supported.",
      domain = "R-svBase")
  )
  expect_identical(
    gettextf("This is message number %i", 1L,
      domain = "R-svBase", lang = "en"),
    base::gettextf("This is message number %i", 1L,
      domain = "R-svBase")
  )
  expect_identical(
    ngettext(0,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase_en"),
    base::ngettext(0,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase")
  )
  expect_identical(
    ngettext(1,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase_en"),
    base::ngettext(1,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase")
  )
  expect_identical(
    ngettext(2,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase_en"),
    base::ngettext(2,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase")
  )

  # Test with French translations when R is in English language
  # First, get French translation from regular base functions
  Sys.setLanguage("fr")
  gettext_fr <- base::gettext("Test of svBase's `gettext()` and `gettextf()`:",
    "This should be transtlated, if '%s' language is supported.",
    domain = "R-svBase")
  gettextf_fr <- base::gettextf("This is message number %i", 1L,
    domain = "R-svBase")
  ngettext_fr0 <- base::ngettext(0,
    "You asked for only one item", "You asked for several items",
    domain = "R-svBase")
  ngettext_fr1 <- base::ngettext(1,
    "You asked for only one item", "You asked for several items",
    domain = "R-svBase")
  ngettext_fr2 <- base::ngettext(2,
    "You asked for only one item", "You asked for several items",
    domain = "R-svBase")
  # Check that trim=TRUE/FALSE works as expected in gettext()
  gettext_fr_trim <- base::gettext(
    "  Test of svBase's `gettext()` and `gettextf()`:\n",
    domain = "R-svBase", trim = TRUE)
  gettext_fr_no_trim <- base::gettext(
    "  Test of svBase's `gettext()` and `gettextf()`:\n",
    domain = "R-svBase", trim = FALSE)

  # Switch R back to English
  Sys.setLanguage("en")

  expect_identical(
    gettext("Test of svBase's `gettext()` and `gettextf()`:",
      "This should be transtlated, if '%s' language is supported.",
      domain = "R-svBase", lang = "fr"), gettext_fr
  )
  expect_identical(
    gettextf("This is message number %i", 1L,
      domain = "R-svBase", lang = "fr"), gettextf_fr
  )
  expect_identical(
    ngettext(0,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase_fr"), ngettext_fr0
  )
  expect_identical(
    ngettext(1,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase_fr"), ngettext_fr1
  )
  expect_identical(
    ngettext(2,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase_fr"), ngettext_fr2
  )
  expect_identical(
    gettext("  Test of svBase's `gettext()` and `gettextf()`:\n",
      domain = "R-svBase", lang = "fr", trim = TRUE), gettext_fr_trim
  )
  expect_identical(
    gettext("  Test of svBase's `gettext()` and `gettextf()`:\n",
      domain = "R-svBase", lang = "fr", trim = FALSE), gettext_fr_no_trim
  )

  # Test an non-existing language
  expect_identical(
    gettext("Test of svBase's `gettext()` and `gettextf()`:",
      "This should be transtlated, if '%s' language is supported.",
      domain = "R-svBase", lang = "xx"),
    base::gettext("Test of svBase's `gettext()` and `gettextf()`:",
      "This should be transtlated, if '%s' language is supported.",
      domain = "R-svBase")
  )
  expect_identical(
    gettextf("This is message number %i", 1L,
      domain = "R-svBase", lang = "xx"),
    base::gettextf("This is message number %i", 1L,
      domain = "R-svBase")
  )
  expect_identical(
    ngettext(0,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase_xx"),
    base::ngettext(0,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase")
  )

  Sys.setLanguage(old_lang)
})

test_that("test_gettext_lang() is working as expected", {
  # Make sure R is in English language
  old_lang <- Sys.setLanguage("en")

  # default
  expect_snapshot(test_gettext_lang(), cran = TRUE)
  expect_snapshot(test_gettext_lang(n = 0), cran = TRUE)
  expect_snapshot(test_gettext_lang(n = 1), cran = TRUE)
  expect_snapshot(test_gettext_lang(n = 2), cran = TRUE)

  # en
  expect_snapshot(test_gettext_lang("en", n = 0), cran = TRUE)
  expect_snapshot(test_gettext_lang("en", n = 1), cran = TRUE)
  expect_snapshot(test_gettext_lang("en", n = 2), cran = TRUE)
  expect_snapshot(test_gettext_lang("en_US.UTF-8", n = 2), cran = TRUE)
  expect_snapshot(test_gettext_lang("en", n = 9), cran = TRUE)

  # fr
  expect_snapshot(test_gettext_lang("fr", n = 0), cran = TRUE)
  expect_snapshot(test_gettext_lang("fr", n = 1), cran = TRUE)
  expect_snapshot(test_gettext_lang("fr", n = 2), cran = TRUE)
  expect_snapshot(test_gettext_lang("fr_FR.UTF-8", n = 2), cran = TRUE)

  # C
  expect_snapshot(test_gettext_lang("C", n = 0), cran = TRUE)
  expect_snapshot(test_gettext_lang("C", n = 1), cran = TRUE)
  expect_snapshot(test_gettext_lang("C", n = 2), cran = TRUE)

  # Empty lang= defaults to en
  expect_snapshot(test_gettext_lang(NULL), cran = TRUE)
  expect_snapshot(test_gettext_lang(character(0)), cran = TRUE)

  # Wrong arguments
  # Wrong language, accepted, providing it is a string starting with two letters
  # In the case of a wrong or unknown language, no translation is done
  expect_snapshot(test_gettext_lang("zz"), cran = TRUE)
  expect_snapshot(test_gettext_lang("zzzz"), cran = TRUE)
  # Other cases are not accepted
  expect_error(test_gettext_lang("e"), class = "lang_wrong_code",
    info = "Too short language code in lang=")
  expect_error(test_gettext_lang(NA_character_), class = "lang_wrong_code",
    info = "Missing value provided to lang=")
  expect_error(test_gettext_lang("EN"), class = "lang_wrong_code",
    info = "Uppercase language code for lang=")

  # Wrong n
  expect_error(test_gettext_lang(n = -1), class = "n_negative",
    info = "Negative number or {.code NA} for n= in ngettext()")
  expect_error(test_gettext_lang(n = NA_integer_), class = "n_negative",
    info = "Not a number for n= in ngettext()")
  # Something else
  expect_error(test_gettext_lang(n = "a"), class = "n_not_numeric",
    info = "Not a number for n= in ngettext()")
  expect_error(test_gettext_lang(n = NULL), class = "n_not_numeric",
    info = "Not a number for n= in ngettext()")

  Sys.setLanguage(old_lang)
})
