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

# The github version of magrittr has a new way to code freduce since 2016
# CRAN version is from 2014
# the new version seems to execute `x %>% f1 %>% f2` as `f2(f1(x))`
# this is not good for our pipes with side effects
# as it's been 2 years since this change and was not yet commited to CRAN, it
# must not be that important, so we hardcode the current version to be safe

# freduce <- function(value, function_list)
# {
#   if (length(function_list) == 1L)
#     function_list[[1L]](value)
#   else 
#     Recall(function_list[[1L]](value), function_list[-1L])
# }
