
# This is the function that injects data = . as first argument in a call
# In case it cannot do it, it returns NULL
#' Inject first argument in a call, usually data = (.)
#'
#' @description In case the `data = (.)` is missing in a call to a "dot-data"
#' function (a function that defines that its first argument `data=` could be
#' missing, and it this case, it is considered to be `.`), this function does
#' the injection of the first argument in the call.
#'
#' @param call A call object, usually a function call.
#' @param arg The name of the argument to inject, usually 'data'.
#' @param value The value to inject, usually the symbol '.'.
#' @param if_not If the call is not a function call, this value is returned
#'   (by default, `NULL`).
#' @param abort_msg The message to use in case the call is not a function call.
#' @param abort_frame The environment to use for the error message, by default,
#'   the caller environment.
#'
#' @returns A modified call with the first argument injected, or `if_not` if the
#'   the `data=` argument is already explicitly in the initial call. For
#'   `is_call_fn()`, `TRUE` if call is a call to a function (either `fun(args)`
#'   or `ns::fun(args)`), `FALSE` otherwise.
#' @export
#' @rdname call_inject
#'
#' @examples
#' call1 <- quote(my_function(x, y))
#' is_call_fn(call1)
#' call_inject_first_arg(call1)
#'
#' # This is not a correct function call
#' call2 <- quote(letters)
#' is_call_fn(call2)
#' #call_inject_first_arg(call2) # Would raise an error
#'
#' # This is a function call, but data= is already explicitly defined
#' call3 <- quote(my_function(x, y, data = my_data))
#' is_call_fn(call3)
#' call_inject_first_arg(call3) # Return NULL
call_inject_first_arg <- function(call, arg = 'data', value = as.symbol('.'),
  if_not = NULL,
  abort_msg = "Argument 'call' must be a 'call' object with a function call.",
  abort_frame = caller_env()) {

  if (!is_call_fn(call))
    abort(abort_msg, .frame = abort_frame)

  if (isTRUE(.SciViews.implicit.data) && all(names(call) != arg)) {
    # Inject arg = value in first position
    l <- length(call)
    if (l == 1) {
      call <- call[1:2] # Add a new argument in pos 2
    } else {
      call <- call[c(1, l + 1, 2:l)] # Add a new argument in pos 2
    }
    call[[2]] <- value
    names(call)[2] <- arg
    call
  } else {# Cannot inject arg = value
    if_not
  }
}

#' @rdname call_inject
#' @export
is_call_fn <- function(call) {
  is.call(call) && (is.symbol(call[[1]]) || # fun(args)
      (is.call(call[[1]]) &&
          as.character(call[[1]][[1]]) %in% c('::', ':::'))) # ns::fun(args)
}
