test_that("set/get_language() & set/get_sciviews_lang() work properly", {
  # Make sure R and SciViews are in English language
  old_lang <- set_language("en")
  old_lang2 <- set_sciviews_lang("en")

  expect_identical(get_language(), "en")
  expect_identical(get_sciviews_lang(), "en")

  set_language("fr")
  expect_identical(get_language(), "fr")
  set_sciviews_lang("fr")
  expect_identical(get_sciviews_lang(), "fr")
  set_sciviews_lang("FR")
  expect_identical(get_sciviews_lang(), "FR")
  set_language("en_US.UTF-8")
  expect_identical(get_language(), "en_US.UTF-8")
  set_sciviews_lang("en_US.UTF-8")
  expect_identical(get_sciviews_lang(), "en_US.UTF-8")
  set_sciviews_lang("FR_BE.UTF-8")
  expect_identical(get_sciviews_lang(), "FR_BE.UTF-8")

  # check_lang() catches wrong language codes
  expect_true(check_lang("en"))
  expect_true(check_lang("en_US.UTF-8"))
  expect_true(check_lang("FR", allow_uppercase = TRUE))
  # But these are incorrect
  expect_error(check_lang(), class = "lang_missing_or_null")
  expect_error(check_lang(NULL), class = "lang_missing_or_null")
  expect_error(check_lang(42), class = "lang_not_single_string")
  expect_error(check_lang(c("en", "fr")), class = "lang_not_single_string")
  expect_error(check_lang("EN"), class = "lang_wrong_code")
  expect_error(check_lang("Fr", allow_uppercase = TRUE),
    class = "lang_wrong_code")
  expect_error(check_lang(""), class = "lang_wrong_code")
  expect_error(check_lang(NA_character_), class = "lang_wrong_code")

  # When LANGUAGE="", still report "en" correctly
  Sys.setenv(LANGUAGE = "")
  expect_identical(get_language(), "en")

  # Reset languages
  set_language(old_lang)
  set_sciviews_lang(old_lang2)
})

test_that("gettext_(), gettextf_() and ngettext_() behave like their base equivalent", {
  # Update .po and .mo files (only test in the source package, not R CMD check)
  if (file.exists("../../DESCRIPTION")) {# This is the source of the package
    cat("\nCompiling .po files...\n")
    res <- try(tools::update_pkg_po("../.."), silent = TRUE)
  }

  # Test with English languages first
  old_lang <- set_language("en") # This is the regular R language
  old_lang2 <- set_sciviews_lang("en") # This is the second "SciViews" language

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
  set_language("fr") # used by base functions
  set_sciviews_lang("fr") # used by svBase functions

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
  expect_identical(
    gettext(" a\n", trim = TRUE),
    base::gettext(" a\n", trim = TRUE)
  )
  # No translation
  expect_identical(gettext("test", lang = NA_character_), "test")
  expect_identical(gettextf("This is a %s", "test", lang = NA_character_),
    "This is a test")
  expect_identical(ngettext(1, "test1", "test2", domain = "NULL/"), "test1")
  expect_identical(ngettext(1, "test1", "test2", domain = "NULL/zz"), "test1")
  expect_identical(gettext("test", lang = "zz"), "test")
  expect_identical(gettextf("This is a %s", "test", lang = "zz"),
    "This is a test")

  # Trivial cases
  expect_identical(gettext(), NULL)
  expect_identical(gettextf(""), "")
  expect_identical(ngettext(1, "", ""), "")

  # Wrong arguments
  # Wrong domain
  expect_error(gettext("test", domain = 1))
  expect_error(gettextf("%s", "test", domain = 1))
  expect_error(ngettext(1, "test1", "test2", domain = 1))
  # Wrong lang
  expect_error(gettext("test", lang = 1), class = "lang_wrong_code")
  expect_error(gettext("test", lang = ""), class = "lang_wrong_code")
  expect_error(gettextf("%s", "test", lang = 1), class = "lang_wrong_code")
  expect_error(gettextf("%s", "test", lang = ""), class = "lang_wrong_code")
  expect_error(ngettext(1, "test1", "test2", domain = "NULL/1"),
    class = "lang_wrong_code")
  # Missing or wrong fmt
  expect_error(gettextf(), class = "fmt_missing")
  # TODO: better catch sprintf() errors
  expect_error(gettextf("%s"))
  expect_error(gettextf("%i", "a"))
  expect_warning(gettextf("%s", "a", "b", lang = "fr"))
  # Missing or wrong arguments for n, msg1, or msg2
  expect_error(ngettext(), class = "n_missing")
  expect_error(ngettext(1), class = "msg1_missing")
  expect_error(ngettext(1, "test1"), class = "msg2_missing")
  expect_error(ngettext(-1, "test", "test2"), class = "n_negative")
  expect_error(ngettext(NA_integer_, "test", "test2"), class = "n_negative")
  expect_error(ngettext("a", "test", "test2"), class = "n_not_numeric")
  expect_error(ngettext(1, 2, "test2"), class = "msg1_not_single_string")
  expect_error(ngettext(1, c("a", "b"), "test2"),
    class = "msg1_not_single_string")
  expect_error(ngettext(1, character(0), "test2"),
    class = "msg1_not_single_string")
  expect_error(ngettext(1, "test", 2), class = "msg2_not_single_string")
  expect_error(ngettext(1, "test", c("a", "b")),
    class = "msg2_not_single_string")
  expect_error(ngettext(1, "test", character(0)),
    class = "msg2_not_single_string")

  # Reset languages
  set_language(old_lang)
  set_sciviews_lang(old_lang2)
})

test_that("gettext_(), gettextf_() and ngettext_() can use lang=", {
  # Make sure R and SciViews are in English language
  old_lang <- set_language("en")
  old_lang2 <- set_sciviews_lang("en")

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
      domain = "R-svBase/en"),
    base::ngettext(1,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase")
  )
  expect_identical(
    ngettext(2,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase/en"),
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
  set_language("en")

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
      domain = "R-svBase/fr"), ngettext_fr0
  )
  expect_identical(
    ngettext(1,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase/fr"), ngettext_fr1
  )
  expect_identical(
    ngettext(2,
      "You asked for only one item", "You asked for several items",
      domain = "R-svBase/fr"), ngettext_fr2
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

  # Reset languages
  set_language(old_lang)
  set_sciviews_lang(old_lang2)
})

test_that("test_gettext_lang() is working as expected", {
  # Make sure R and SciViews are in English language
  old_lang <- set_language("en")
  old_lang2 <- set_sciviews_lang("en")

  # default
  expect_snapshot(test_gettext_lang(), cran = TRUE)
  expect_snapshot(test_gettext_lang(n = 0), cran = TRUE)
  expect_snapshot(test_gettext_lang(n = 1), cran = TRUE)
  expect_snapshot(test_gettext_lang(n = 2), cran = TRUE)

  # en
  expect_snapshot(test_gettext_lang("en", n = 0), cran = TRUE)
  expect_snapshot(test_gettext_lang("en", n = 1), cran = TRUE)
  expect_snapshot(test_gettext_lang("en", n = 2), cran = TRUE)
  expect_snapshot(test_gettext_lang("EN", n = 2), cran = TRUE)
  expect_snapshot(test_gettext_lang("en_US.UTF-8", n = 2), cran = TRUE)
  expect_snapshot(test_gettext_lang("en", n = 9), cran = TRUE)

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
  expect_error(test_gettext_lang("e"), class = "lang_wrong_code")

  # Wrong n
  expect_error(test_gettext_lang(n = -1), class = "n_negative")
  expect_error(test_gettext_lang(n = NA_integer_), class = "n_negative")
  # Something else
  expect_error(test_gettext_lang(n = "a"), class = "n_not_numeric")
  expect_error(test_gettext_lang(n = NULL), class = "n_not_numeric")

  set_language(old_lang)

  # fr (apparently, it does not work on Ubuntu in GitHub actions)
  skip_on_os("linux") # For now...
  old_lang <- set_language("en")
  expect_snapshot(test_gettext_lang("fr", n = 0), cran = TRUE)
  expect_snapshot(test_gettext_lang("fr", n = 1), cran = TRUE)
  expect_snapshot(test_gettext_lang("fr", n = 2), cran = TRUE)
  expect_snapshot(test_gettext_lang("fr_FR.UTF-8", n = 2), cran = TRUE)

  # Reset languages
  set_language(old_lang)
  set_sciviews_lang(old_lang2)
})
