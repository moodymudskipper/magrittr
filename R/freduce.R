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

# we needed to roll back to an older version of freduce because the new one
# starts to execute from the right (inside out) so our text displays arrive in
# the wrong order, maybe we can understand how it works and fix it 

# freduce <- function(value, function_list)
# {
#   if (length(function_list) == 1L)
#     function_list[[1L]](value)
#   else 
#     Recall(function_list[[1L]](value), function_list[-1L])
# }
