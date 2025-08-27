# TODO: resolve .?fun$type(obj) to the right page + explanations
# TODO: get completion for regular fun$types + hybrid mode

#' Define a function as being 'subsettable' using $ operator
#'
#' @description In case a textual argument allows for selecting the result, for
#' instance, if `plot()` allows for several charts that you can choose with a
#' `type=` or `which=`, making the function 'subsettable' also allows to
#' indicate `fun$variant()`. The `subsettable_type2` variant is faster for only
#' internal implementation of various types, while `subsettable_type` first
#' searches for a function with `name.<generic>$type()`. See examples.
#' @export
#' @name subsettable
#' @param x A `subsettable_type` function.
#' @param name The value to use for the `type=` argument.
#' @method $ subsettable_type
#' @keywords utilities
#' @concept create 'subsettable' functions
#' @examples
#' # Simple selection of type with a switch inside the function itself
#' foo <- structure(function(x, type = c("histogram", "boxplot"), ...) {
#'   type <- match.arg(type, c("histogram", "boxplot"))
#'   switch(type,
#'     histogram = hist(x, ...),
#'     boxplot = boxplot(x, ...),
#'     stop("unknow type")
#'   )
#' }, class = c("function", "subsettable_type2"))
#' foo
#'
#' # This function can be used as usual:
#' foo(rnorm(50), type = "histogram")
#' # ... but also this way:
#' foo$histogram(rnorm(50))
#' foo$boxplot(rnorm(50))
#'
#' # A more complex use, where it is possible to define additional types easily.
#' # It also allow for completion after fun$... and completion of functions
#' # arguments, depending on the selected type (to avoid putting all arguments
#' # for all types together, otherwise, it is a mess)
#' head2 <- structure(function(data, n = 10, ..., type = "default") {
#'   # This was the old (static) aaproach: not possible to add a new type
#'   # without modifying the function head2()
#'   #switch(type,
#'   #  default = `.head2$default`(data, n = n, ...),
#'   #  fun = `.head2$fun`(data, n = n, ...)
#'   #)
#'   # This is the new (dynamic) approach
#'   get_type("head2", type = type)(data, n = n, ...)
#' }, class = c("subsettable_type", "function", "head2"))
#'
#' # We define two types for head2(): default and fun
#' `head2_default` <- function(data, n = 10, ...) {
#'   head(data, n = n)
#' }
#'
#' # Apply a fun on head() - just an example, not necessarily useful
#' `head2_fun` <- function(data, n = 10, fun = summary, ...) {
#'   head(data, n = n) |> fun(...)
#' }
#'
#' head2(iris)
#' head2(iris, type = "default") # Idem
#' head2$default(iris) # Idem
#' head2$fun(iris) # The other type, with fun = summary()
#' head2$fun(iris, fun = str)
#'
#' # Now, the completion (e.g., in RStudio or Positron)
#' # 1. Type head2$ and you got the list of available types
#' # 2. Select "default" then hit <tab>, you got the list of args for default
#' # 3. Do the same but select "fun", now you got the arguments for the fun type
#' # 4. Just write a new `.head2_<type>` function and <type> is automatically
#' #    integrated!
`$.subsettable_type` <- function(x, name) {
  cl <- class(x)
  generic <- cl[length(cl)] # Last item
  fun_type <- paste0(generic, "_", name)
  fun <- get0(fun_type, envir = parent.frame(), mode = "function")
  if (is.null(fun)) {# If not found, recall the original function with type arg
    function(...) x(type = name, ...)
  } else {
    fun
  }
}

#' @export
#' @rdname subsettable
#' @method $ subsettable_type2
`$.subsettable_type2` <- function(x, name)
  function(...) x(type = name, ...)

#' @export
#' @rdname subsettable
#' @method $ subsettable_which
`$.subsettable_which` <- function(x, name)
  function(...) x(which = name, ...)

#' @export
#' @rdname subsettable
#' @param fun The name of the function (as a scalar string).
#' @param method An optional method name (as a scalar string).
#' @param type The type to select (as a scalar string).
name_function_type <- function(fun, method = NULL, type) {
  stopifnot(is.character(fun), is.character(type),
    length(fun) == 1, length(type) == 1)
  if (is.null(method)) {
    paste0(fun, "_", type)
  } else {# Method provided
    stopifnot(is.character(method), length(method) == 1)
    paste0(fun, ".", method, "_", type)
  }
}

#' @export
#' @rdname subsettable
list_types <- function(fun, method = NULL) {
  fun_search <- name_function_type(fun, method, "")
  #fun_search <- sub("\\$", "\\\\$", fun_search)
  fun_search <- gsub("\\.", "\\\\.", fun_search)
  fun_search <- paste0("^", fun_search)
  res <- apropos(fun_search, mode = "function")
  sub(paste0(fun_search, "(.+)$"), "\\1", res)
}

#' @export
#' @rdname subsettable
#' @param stop.if.missing If `TRUE` (default), an error is raised if the
#'   requested type is not found. If `FALSE`, `NULL` is returned instead.
get_type <- function(fun, method = NULL, type, stop.if.missing = TRUE) {
  fun_name <- name_function_type(fun, method, type)
  fun_body <- get0(fun_name, envir = parent.frame(), mode = "function")
  if (is.null(fun_body) && isTRUE(stop.if.missing)) {
    types <- list_types(fun, method)
    if (is.null(method)) {
      fun_str <- paste0(fun, "()")
    } else {
      fun_str <- paste0(fun, ".", method, "()")
    }
    if (!length(types)) {
      stop("Type {.code {type}} not found for function {.fun {fun_str}}",
        i = "No type found for this function.")
    } else {
      stop("Type {.code {type}} not found for function {.fun {fun_str}}",
        i = "Known types: {types}.")
    }
  }
  fun_body
}

#' @export
#' @rdname subsettable
args_type <- function(fun, method = NULL, type) {
  fun_name <- name_function_type(fun, method, type)
  fun_body <- get0(fun_name, envir = parent.frame(), mode = "function")
  if (is.null(fun_body)) {
    if (is.null(method)) {
      fun_str <- paste0(fun, "()")
    } else {
      fun_str <- paste0(fun, ".", method, "()")
    }
    stop("Type {.code {type}} not found for function {.fun {fun_str}}")
  }
  args(fun_body)
}

#' @export
#' @rdname subsettable
#' @param pattern A regular expression. Only matching names are returned.
#' @method .DollarNames subsettable_type
.DollarNames.subsettable_type <- function(x, pattern = "") {
  cl <- class(x)
  generic <- cl[length(cl)] # Last item
  res <- list_types(generic)
  if (!length(res))
    return("")
  res <- res[grepl(pattern, res)]
  # Indicate that these are functions by adding '('
  paste0(res, "(")
}
