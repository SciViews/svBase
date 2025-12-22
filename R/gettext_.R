.gettext_lang_factory <- function() {
  # This function makes necessary checking only once and keeps the results
  # Also, it keeps memory of languages already used and languages that failed
  # to speed up switching from one language to another.

  # These are the original gettext(), gettextf() and ngettext() functions
  gettext_base <- base::gettext # nocov start
  gettextf_base <- base::gettextf
  ngettext_base <- base::ngettext

  # Is natural language supported?
  no_nls <- (!capabilities("NLS") || is.na(.popath) ||
      Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))

  # The (growing) list of supported languages (each time a successful switch
  # to such a language is done, it is recorded here)
  # Obviously, current R language is already supported
  known_lang <- Sys.getenv("LANGUAGE", unset = "")
  if (!nzchar(known_lang))
    known_lang <- "en"
  # For debugging purpose
  #message("Language: ", known_lang[1])

  # The growing list of failed languages (e.g., strings that do not match any
  # known language)
  failed_lang <- character(0)

  # Run Sys.setLanguage() from base R on current language to make sure
  # everything is fine
  if (!no_nls) {
    res <- try(attr(Sys.setLanguage(known_lang[1]), "ok"), silent = TRUE)
    # If it fails, consider no_nls with a warning
    if (inherits(res, "try-error") || !res) {
      no_nls <- TRUE
      warning("Natural language supported on this system, ",
        "but unable to properly switch language.", call. = FALSE)
    }
  }

  # Make sure current R language is defined in the LANGUAGE envir var
  Sys.setenv(LANGUAGE = known_lang[1]) # nocov end
  # Note: code up to here cannot be tested because it is executed only once
  # when the package loads... but subsequent tests of following functions
  # cannot succeed if this code did not run correctly.

  # The function that retrieves one or more translated character strings in a
  # given language (may be different to current R language, and defaults to
  # SciViews language)
  gettext_lang <- compiler::cmpfun(
    function(..., domain = NULL, trim = TRUE, lang = get_sciviews_lang()) {

      .__top_call__. <- TRUE

      if (missing(lang)) # Use default base::gettext()
        return(gettext_base(..., domain = domain, trim = trim))

      if (!is.null(domain))
        domain <- as.character(domain)[1L]

      # If no_nls, same as current language, or failed lang, just run gettext()
      # Use only first string, without warning
      lang <- as.character(lang)
      if (!length(lang)) lang <- "en" else lang <- lang[1]
      if (is.na(lang)) lang <- "en" # Default language
      # Uppercase lang must be transformed into lowercase
      if (grepl("^[A-Z]{2}", lang))
        substring(lang, 1L, 2L) <- tolower(substring(lang, 1L, 2L))
      cur_lang <- Sys.getenv("LANGUAGE", unset = "en")
      # For debugging purpose
      #message("lang=", lang, "; cur_lang=", cur_lang)

      if (no_nls || lang == cur_lang || any(failed_lang == lang) ||
          !is.null(domain) && (is.na(domain) || domain == "")) {
        #message("Optimization #1!")
        return(gettext_base(..., domain = domain, trim = isTRUE(trim)))
      }

      # If the language is already known, switch faster
      if (any(known_lang == lang)) {
        #message("Optimization #2!")
        Sys.setenv(LANGUAGE = lang)
        on.exit(Sys.setenv(LANGUAGE = cur_lang))
      } else {
        # Use the slower set_language() to switch language properly the first
        # time, then, record it in known_lang if it works
        cur_lang <- set_language(lang)
        on.exit(Sys.setenv(LANGUAGE = cur_lang))
        if (attr(cur_lang, "ok")) {
          known_lang <<- c(known_lang, lang) # Record the new language
        } else {# Failed to switch to this lang
          failed_lang <<- c(failed_lang, lang) # Record the failed language
          msg <- paste("Unable to switch to language '%s'. Using current",
            "language '%s' instead\n(displayed only once per session).")
          warning(gettextf_base(msg, lang, cur_lang), call. = FALSE)
          return(gettext_base(..., domain = domain, trim = isTRUE(trim)))
        }
      }

      # Flush the cache of translations since we switch to another language
      bindtextdomain(NULL)

      res <- gettext_base(..., domain = domain, trim = isTRUE(trim))

      # Flush the cache of translations again before switching back to the
      # current language (done in on.exit)
      bindtextdomain(NULL)

      # This is for debugging purposes only
      #message(no_nls)
      #message(paste0(known_lang, collapse = ", "))
      #message(paste0(failed_lang, collapse = ", "))

      res
    }
  )

  gettextf_lang <- compiler::cmpfun(
    function(fmt, ..., domain = NULL, trim = TRUE, lang = get_sciviews_lang()) {

      if (missing(fmt))
        stop("Argument {.arg fmt} is missing but must be provided.",
          class = "fmt_missing")

      if (missing(lang)) {
        sprintf(gettext_base(fmt, domain = domain, trim = isTRUE(trim)), ...)
      } else {
        sprintf(gettext_lang(fmt, domain = domain, trim = trim, lang = lang),
          ...)
      }
    }
  )

  ngettext_lang <- compiler::cmpfun(
    function(n, msg1, msg2, domain = NULL) {

      .__top_call__. <- TRUE

      if (missing(n))
        stop("Argument {.arg n} is missing but must be provided.",
          class = "n_missing")
      if (missing(msg1))
        stop("Argument {.arg msg1} is missing but must be provided.",
          class = "msg1_missing")
      if (missing(msg2))
        stop("Argument {.arg msg2} is missing but must be provided.",
          class = "msg2_missing")

      def_lang <- get_sciviews_lang()

      if (!is.numeric(n))
        stop("Argument {.arg n} must be numeric.",
          i = "You provided an object of class {.cls {class(n)}}.",
          class = "n_not_numeric")
      n <- n[1L] # Only consider first bvalue, like base::ngettext() does
      if (is.na(n) || n < 0)
        stop("Argument {.arg n} must be a non-negative integer.",
          i = "You provided: {.code {deparse(n)}}.",
          class = "n_negative")

      if (!is.character(msg1) || length(msg1) != 1L)
        stop("Argument {.arg msg1} must be a single string.",
          i = "You provided: {.code {deparse(msg1)}}.",
          x = ifelse(length(msg1) == 1L,
            " This is an object of class {.cls {class(msg1)}}.",
            " This is an object of length {length(msg1)}."),
          class = "msg1_not_single_string")

      if (!is.character(msg2) || length(msg2) != 1L)
        stop("Argument {.arg msg2} must be a single string.",
          i = "You provided: {.code {deparse(msg2)}}.",
          x = ifelse(length(msg2) == 1L,
            " This is an object of class {.cls {class(msg2)}}.",
            " This is an object of length {length(msg2)}."),
          class = "msg2_not_single_string")

      if (is.null(domain)) {
        lang <- def_lang
      } else {# Try to separate domain from lang (should be domain/lang)
        dom_lang <- strsplit(domain, "/", fixed = TRUE)[[1]]
        if (length(dom_lang) > 1L) {
          domain <- dom_lang[1L]
          if (domain == "NULL") domain <- NULL # Default value for domain
          lang <- dom_lang[2L]
        } else {
          lang <- def_lang
        }
      }

      if (lang == "") # Use default base::ngettext()
        return(ngettext_base(n = n, msg1 = msg1, msg2 = msg2, domain = domain))

      # If no_nls or same as current language or failed lang, just run gettext()
      lang <- as.character(lang)
      if (!length(lang)) lang <- "en" else lang <- lang[1]
      # Never happens: if (is.na(lang)) lang <- "en" # Default language
      # Uppercase lang must be transformed into lowercase
      if (grepl("^[A-Z]{2}", lang))
        substring(lang, 1L, 2L) <- tolower(substring(lang, 1L, 2L))
      cur_lang <- Sys.getenv("LANGUAGE", unset = "en")
      if (no_nls || lang == cur_lang || any(failed_lang == lang) ||
          !is.null(domain) && (is.na(domain) || domain == "")) {
        #message("Optimization #1!")
        return(ngettext_base(n = n, msg1 = msg1, msg2 = msg2, domain = domain))
      }

      # If the language is already known, switch faster
      if (any(known_lang == lang)) {
        #message("Optimization #2!")
        Sys.setenv(LANGUAGE = lang)
        on.exit(Sys.setenv(LANGUAGE = cur_lang))
      } else {
        # Use the slower set_language() to switch language properly the first
        # time, then, record it in known_lang if it works
        cur_lang <- set_language(lang)
        on.exit(Sys.setenv(LANGUAGE = cur_lang))
        if (attr(cur_lang, "ok")) {
          known_lang <<- c(known_lang, lang) # Record the new language
        } else {# Failed to switch to this lang
          failed_lang <<- c(failed_lang, lang) # Record the failed language
          msg <- paste("Unable to switch to language '%s'. Using current",
            "language '%s' instead\n(displayed only once per session).")
          warning(gettextf_base(msg, lang, cur_lang), call. = FALSE)
          return(ngettext_base(n = n, msg1 = msg1, msg2 = msg2,
            domain = domain))
        }
      }

      # Flush the cache of translations since we switch to another language
      bindtextdomain(NULL)

      res <- ngettext_base(n = n, msg1 = msg1, msg2 = msg2, domain = domain)

      # Flush the cache of translations again before switching back to the
      # current language (done in on.exit)
      bindtextdomain(NULL)

      # This is for debugging purposes only
      #message(no_nls)
      #message(paste0(known_lang, collapse = ", "))
      #message(paste0(failed_lang, collapse = ", "))

      res
    }
  )

  # Return the functions in a list
  list(gettext = gettext_lang, gettextf = gettextf_lang, # nocov
    ngettext = ngettext_lang) # nocov
}

.gettext_lang <- .gettext_lang_factory()

#' Translate messages in a different language than the one in the R session
#'
#' Message translation in R is obtained with [base::gettext()] or
#' [base::ngettext()]. But, there is no way to specify that one needs translated
#' messages in a different language than the current one in R. Here are
#' alternate functions that have an additional `lang=` argument allowing to do
#' so. If the `lang=` argument is not provided in the call, they use an
#' alternate language defined by [set_sciviews_lang()]. This is useful, for
#' instance, to keep R error and warning messages in English, but to generate
#' translation for tables and figures in a different language in a report.
#'
#' @param fmt  a character vector of format strings, each of up to 8192 bytes.
#' @param ... one of more character vectors.
#' @param domain the 'domain' for the translation, a character string or `NULL`;
#' see [base::gettext()] for more details. For `ngettext_()`, it should combine
#' the domain and the lang, like `"domain/lang"` (e.g., `"NULL/en_US"` or
#' `"R-stats/fr"`). This is a workaround to define the language, because base
#' version of that function does not allow additional arguments and we have to
#' remain compatible here.
#' @param trim logical indicating if the white space trimming should happen.
#' @param n a non-negative integer.
#' @param msg1 the message to be used in English for `n = 1`.
#' @param msg2 the message to be used in English for `n = 0, 2, 3, ...`
#' @param lang the target language (could be two lowercase letters, e.g., "en"
#' for English, "fr" for French, "de" for German, etc.). One can also further
#' specify variants, e.g., "en_US", or "en_GB", or even "fr_FR.UTF-8". For
#' `get_language()` and `set_language()`, it is the default language of the R
#' session. For the other functions, it is the alternate language used by
#' SciViews. One can specify it globally with either the SCIVIEWS_LANG
#' environment variable, or with the R option `SciViews_lang`, but it is a
#' better practice to use `set_sciviews_lang()` in the R session. If missing,
#' `NULL`, or `""`, the default is used from `unset`. For the SciViews language,
#' uppercase letters are accepted, and they mean "translate more" (typically,
#' **factor** and **ordered** levels are also translated, for instance).
#' @param unset The default language to use if not defined yet, "en" (English)
#' by default for regular R language, and the currently defined R language for
#' the alternate SciViews language.
#' @param allow_uppercase logical indicating if uppercase letters are allowed
#' for the first two letters of the language code (`FALSE` by default, but
#' should be `TRUE` for the SciViews language).
#'
#' @details
#' To prepare your package for translation with these functions, you should
#' import `gettext_()`, `gettextf_()` and `ngettext_()` from svBase. Then, you
#' define `gettext <- gettext_`, `gettextf <- gettextf_` and
#' `ngettext <- ngettext_` somewhere in your package. To prepare translation
#' strings, you change the current directory of your R console to the base
#' folder of the sources of your package and you issue
#' `tools::update_pkg_po(".")` in R (or you include it in the tests: for an
#' example, see tests/testthat/test-translations.R in the source of the svBase
#' package). Then, you perform the translation for different languages with,
#' say, [poEdit](https://poedit.net/), and recompile your package.
#'
#' @returns A character vector with translated messages for the `gettext...()`
#' functions.
#'
#' `test_gettext_lang()` just serves to test and demonstrate the translation in
#' a given language.
#'
#' `get_language()` and `get_sciviews_lang()` return the current language.
#' `set_language()`and `set_sciviews_lang()` return the previous language
#' invisibly (with an attribute `attr(*, "ok")` a **logical** indicating
#' success.
#'
#' `check_lang()` validates a `lang=` argument by returning `TRUE` invisibly,
#' otherwise, it `stop()`s.
#'
#' @export
#'
#' @seealso [base::gettext()], [base::gettextf()], [tools::update_pkg_po()].
#'
#' @examples
#' get_language()
#' get_sciviews_lang()
#'
#' old_lang <- set_language("fr") # Switch to French for R language
#' old_sv_lang <- set_sciviews_lang("fr") # Switch to French for SciViews also
#'
#' # R look for messages to be translated into gettext() calls, not gettext_()
#' # So, rename accordingly in your package:
#' gettext <- svBase::gettext_
#' gettextf <- svBase::gettextf_
#' ngettext <- svBase::ngettext_
#'
#' # Retrieve strings in same language
#' gettext("empty model supplied", "incompatible dimensions",
#'  domain="R-stats", lang = "fr")
#'
#' # Retrieve strings in different languages
#' gettext("empty model supplied", "incompatible dimensions",
#'   domain="R-stats", lang = "en")
#' gettext("empty model supplied", "incompatible dimensions",
#'   domain="R-stats", lang = "de")
#'
#' # Try to get strings translated in an unknown language (just return the strings)
#' gettext("empty model supplied", "incompatible dimensions",
#'   domain="R-stats", lang = "xx")
#'
#' # Test with some translations from the svMisc package itself:
#' svBase::test_gettext_lang()
#' svBase::test_gettext_lang("fr", n = 1)
#' svBase::test_gettext_lang("fr", n = 2)
#' svBase::test_gettext_lang("en", n = 1)
#' svBase::test_gettext_lang("en", n = 2)
#'
#' # Restore original languages
#' set_language(old_lang)
#' set_sciviews_lang(old_sv_lang)
#' rm(old_lang, old_sv_lang, gettext, gettextf, ngettext)
#'
#' # In case you must check if a lang= argument gets a correct value:
#' check_lang("en")
#' check_lang("en_US.UTF-8")
#' # Only for SciViews language!
#' check_lang("FR", allow_uppercase = TRUE)
#' # But these are incorrect
#' try(check_lang("EN"))
#' try(check_lang(""))
#' try(check_lang(NA_character_))
#' try(check_lang(NULL))
#' try(check_lang(42))
#' try(check_lang(c("en", "fr")))
#' try(check_lang("Fr", allow_uppercase = TRUE))
gettext_ <- .gettext_lang$gettext

#' @export
#' @rdname gettext_
gettextf_ <- .gettext_lang$gettextf

#' @export
#' @rdname gettext_
ngettext_ <- .gettext_lang$ngettext

#' @export
#' @rdname gettext_
get_language <- function(unset = "en") {
  lang <- Sys.getenv("LANGUAGE", unset = unset)
  if (!nzchar(lang))
    lang <- unset
  lang
}

#' @export
#' @rdname gettext_
set_language <- function(lang, unset = get_language()) {
  .__top_call__. <- TRUE
  check_lang(lang)

  invisible(Sys.setLanguage(lang, unset = unset))
}

#' @export
#' @rdname gettext_
get_sciviews_lang <- function(unset = get_language()) {
  getOption("SciViews_lang",
    default = Sys.getenv("SCIVIEWS_LANG", unset = unset))
}

#' @export
#' @rdname gettext_
set_sciviews_lang <- function(lang, unset = "en") {
  .__top_call__. <- TRUE
  check_lang(lang, allow_uppercase = TRUE)

  olang <- get_sciviews_lang(unset = unset)

  options(SciViews_lang = lang)
  Sys.setenv(SCIVIEWS_LANG = lang)

  invisible(olang)
}

#' @export
#' @rdname gettext_
check_lang <- function(lang, allow_uppercase = FALSE) {
  if (missing(lang) || is.null(lang))
    stop("Argument {.arg lang} is missing , or {.code NULL}.",
      i = "Provide a valid language code ('en', 'en_US.UTF-8', 'fr', 'de'...)",
      class = "lang_missing_or_null")

  if (!is.character(lang) || length(lang) != 1L)
    stop("The argument {.arg lang} must be a single string.",
      x = "It is {.code {deparse(lang)}}.",
      i = "Provide a valid language code ('en', 'en_US.UTF-8', 'fr', 'de'...)",
      class = "lang_not_single_string")

  rex <- if (isFALSE(allow_uppercase)) "^[a-z]{2}" else "^[a-z]{2}|^[A-Z]{2}"
  if (is.na(lang) || (lang != "C" && !grepl(rex, lang)))
    stop("Wrong {.arg lang} argument (must be either 'C' or a language code).",
      x = "It is {.code {deparse(lang)}}.",
      i = "Provide a valid language code ('en', 'en_US.UTF-8', 'fr', 'de'...)",
      class = "lang_wrong_code")

  invisible(TRUE)
}

#' @export
#' @rdname gettext_
test_gettext_lang <- function(lang = get_sciviews_lang(), n = 1) {
  # You should import gettext_(), gettextf_() and ngettext_() from svMisc and
  # rename them gettext, gettextf, and ngettext respectively instead of using
  # the base functions to get the lang= argument working properly.
  # This is better done in your package directly.
  gettext <- gettext_
  gettextf <- gettextf_
  ngettext <- ngettext_

  .__top_call__. <- TRUE

  if (!length(lang)) lang <- "en" # Default language

  # Test the gettext() function with lang= attribute
  res <- gettext("Test of svBase's `gettext()` and `gettextf()`:",
    "This should be transtlated, if '%s' language is supported.",
    lang = lang, domain = "R-svBase")
  #cat(res[1], "\n", sep = "")
  #cat(sprintf(res[2], lang), "\n", sep = "")
  res[2] <- sprintf(res[2], lang)

  # It is easier to use gettextf() for formatted messages
  res <- c(res, gettextf("This is message number %i", 3L,
    lang = lang, domain = "R-svBase"))

  # For pluralisation, use ngettext() and append _lang to the domain
  # (because it is not possible to add a lang= argument to ngettext())
  domain_lang <- paste("R-svBase", lang, sep = "/")
  res <- c(res, ngettext(n, "You asked for only one item", "You asked for several items",
    domain = domain_lang))
  noquote(res)
}
