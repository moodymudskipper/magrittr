#' Create a new pipe operator.
#'
#' Create a new pipe by wrapping new code around what would have been executed
#' by the standard `%>%` pipe.
#' 
#' Printing existing pipes from the *pipes* package will provide examples to
#' build on. 
#' 
#' @param wrap an expression that will be executed after `BODY` is replaced by
#'   the code that would have been run by `%>%` and `.` replaced by the
#'   input
#'   
#' @examples 
#' \dontrun{
#' # How `%T>%` and `%$>%` were defined in this package 
#' `%T>%` <- new_pipe({local(BODY);.})
#' `%$%`  <- new_pipe(with(., BODY)) 
#' }
#' # print the number of rows before and after
#' `%nrowba>%` <- new_pipe({
#'   print(nrow(.))
#'   res <- BODY
#'   print(nrow(res))
#'   res})
#' iris %nrowba>% head
#' @export
new_pipe <- function(wrap)
{
  # build a standard magrittr pipe
  fun <- pipe()
  
  # give it a pipe class
  class(fun) <- c("pipe","function")
  
  # give it an attribute containing wrapper code to be used in wrap_function
  wrap <- substitute(wrap)
  attr(fun,"wrap") <- if(missing(wrap)) NULL else  substitute(wrap)
  
  fun
}

#' Check whether a symbol is a valid pipe.
#'
#' @param pipe A quoted symbol
#' @return logical - TRUE if a valid pipe, FALSE otherwise.
#' @export
is_pipe <- function(pipe)
{
  inherits(eval(pipe),"pipe")
}


#' Print pipe operator
#'
#' Method for friendly printing of pipe operators, traditional output can be
#' obtained by calling `print.function(x)` 
#'
#' @param x operator
#' @param ... not used, for compatibility with other methods
#' 
#' @export
print.pipe <- function(x,...){
  wrap <- attr(x,"wrap")
  if(is.null(wrap)){
    message("Standard pipe operator")
  } else {
    message("Special pipe operator")
    cat("wrap:\n")
    print(wrap)
  }
  invisible(x)
}
# 
# library(pipes)
# `%p>%` <- pipes::`%>%`
# `%>%`  <- magrittr::`%>%`
# microbenchmark::microbenchmark(
#   magrittr = 1 %>% force %>% force %>% force,
#   pipes = 1 %p>% force %p>% force %p>% force, times = 1000)
# # Unit: microseconds
# #                               expr  min    lq     mean median    uq   max neval cld
# #    1 %>% force %>% force %>% force 75.1  77.8  88.7577   80.6  83.3 570.2  1000  a 
# # 1 %p>% force %p>% force %p>% force 97.3 100.6 113.9058  104.2 108.3 681.4  1000   b
# 
