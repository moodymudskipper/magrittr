# Wrap an expression in a function
# 
# This function takes the "body" part of a function and wraps it in
# a function.
#
# @param body an expression which will serve as function body in single-argument
#    function with an argument names `.` (a dot)
# @param pipe a quoted magrittr pipe, which determines how the function is made.
# @param env The environment in which to contruct the function.
#
# @return a function of a single argument, named `.`.
wrap_function <- function(body, pipe, env)
{
  wrap <- attr(eval(pipe),"wrap")
  body <- eval(substitute(substitute(WRAP,list(BODY = body)), list(WRAP = wrap)))
  eval(call("function", as.pairlist(alist(.=)), body), env, env)
}

# about freduce:
# The github version of magrittr has a new way to code freduce since 2016
# CRAN version is from 2014
# the new version seems to execute `x %>% f1 %>% f2` as `f2(f1(x))`
# this is not good for our pipes with side effects
# as it's been 2 years since this change and was not yet commited to CRAN, it
# must not be that important, so we hardcode the current version to be safe

#' Apply a list of functions sequentially
#'
#' This function applies the first function to `value`, then the
#' next function to the result of the previous function call, etc. 
#' 
#' @param value initial value.
#' @param function_list a list of functions.
#' @return The result after applying each function in turn.
#'
#'
#' @export
freduce <- function (value, function_list) 
{
  k <- length(function_list)
  if (k > 1) {
    for (i in 1:(k - 1L)) {
      value <- function_list[[i]](value)
    }
  }
  value <- withVisible(function_list[[k]](value))
  if (value[["visible"]]) 
    value[["value"]]
  else invisible(value[["value"]])
}

# new github version :

# freduce <- function(value, function_list)
# {
#   if (length(function_list) == 1L)
#     function_list[[1L]](value)
#   else 
#     Recall(function_list[[1L]](value), function_list[-1L])
# }
