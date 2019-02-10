#' Pipe friendly conditional operation
#'
#' Apply a transformation on the data only if a condition is met,
#' by default if condition is not met the input is returned unchanged.
#' 
#' Names of columns or list elements are made accessible as variable to all 3
#' arguments wether they're given as expressions, functions or formulas
#'
#' @param x An object
#' @param p A predicate function, a formula describing such a predicate function, or an expression.
#' @param true,false Functions to apply to the data, formulas describing such functions, or expressions.
#'
#' @return The output of \code{true} or \code{false}, either as expressions or applied on data as functions
#' @export
#'
#' @examples
#' # using functions
#' iris %>% pif(is.data.frame, dim, nrow)
#' # using formulas
#' iris %>% pif(~is.numeric(Species), ~"numeric :)",~paste(class(Species)[1],":("))
#' # using expressions
#' iris %>% pif(nrow(.) > 2, head(.,2))
pif <- function(x, p, true, false = identity){
  if (!requireNamespace("rlang"))
    stop("The package `rlang` must be installed to use `pif`")
  
  is_list_x <- is.list(x)
  
  # p is evaluated in x
  p <- eval.parent(substitute(with(.,p)))
  # if p is a formula, turn it into a function
  if (inherits(p, "formula")) p <- rlang::as_function(p)
  # if it's a function, evaluate `with` x
  if (is.function(p)) p <- if (is_list_x) with(x, p(x)) else p(x)
  
  # res is evaluated in x
  res <- if (p) eval.parent(substitute(with(.,true))) else 
    eval.parent(substitute(with(.,false)))
  # if res is a formula, turn it into a function
  if (inherits(res, "formula")) res <- rlang::as_function(res)
  # if it's a function, evaluate `with` x
  if (is.function(res)) res <- if (is_list_x) with(x, res(x)) else res(x)
  
  res
}
# #  
# debugonce(pif)
# iris %>% pif(is.numeric(Species), "numeric :)",paste(class(Species)[1],":("))
# iris %>% pif(~is.numeric(Species), ~"numeric :)",~paste(class(Species)[1],":("))