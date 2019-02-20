#' Pipe operators to enhance magrittr
#'
#' These operators include  magrittr's standard pipe operators `%>%`,
#' `%T>%`, `%$%` and `%<>%` and additional ones which 
#' provide side effects but mostly return the same output as `%>%`. 
#' 
#' In *magrittr* pipe operators are all the same functions, and are recognized
#' in the code by their names, in *mmpipe* they inherit from the class `pipe` 
#' and their behavior is ruled by their attribute `wrap` explained in \link{new_pipe}.
#' They also have their own printing method.
#'
#' \describe{
#'   \item{\%>\%}{Pipe an object forward into a function or call expression.
#'     (see \code{\link[magrittr]{pipe}})}
#'   \item{\%T>\%}{Pipe a value forward into a function- or call expression and
#'     return the original value instead of the result (see 
#'     \code{\link[magrittr]{tee}})}
#'   \item{\%$\%}{Expose the names in `lhs` to the `rhs` expression. This is 
#'     useful when functions do not have a built-in data argument (see
#'     \code{\link[magrittr]{exposition}})}
#'   \item{\%<>\%}{Pipe an object forward into a function or call expression and update the 
#' `lhs` object with the resulting value (see \code{\link[magrittr]{compound}})}
#'   \item{\%D>\%}{Debug the pipe chain at the relevant step}
#'   \item{\%V>\%}{Use \code{View()} on the output}
#'   \item{\%L>\%}{Log the relevant call and execution time to the console}
#'   \item{\%P>\%}{Use \code{print()} on the output}
#'   \item{\%summary>\%}{Print the \code{summary()} off the output}
#'   \item{\%glimpse>\%}{Use \code{tibble::glimpse} on the output}
#'   \item{\%skim>\%}{Use \code{skimr::skim} on the output}
#'   \item{\%ae>\%}{Use \code{all.equal} on the input and output}
#'   \item{\%compare>\%}{Use \code{arsenal::compare} and open the report of the
#'     differences in the default browser window}
#'   \item{\%gg>\%}{Apply the `rhs` to the data of a `gg` object and return the modified `gg` object}
#'   \item{\%nowarn>\%}{Silence warnings}
#'   \item{\%nomsg>\%}{Silence messages}
#'   \item{\%strict>\%}{Fail on warning}
#'   \item{\%quietly>\%}{ Use `purrr::quietly` to capture outputs of all kind and print them}
#'   \item{\%try>\%}{Try, and in case of failure prints error and returns input}
#' }
#'
#' @inheritParams magrittr::`%>%`
#'
#' @examples
#' # print a pipe objects
#' `%T>%`
#' 
#' \dontrun{
#' # debug the chain
#' iris %>% head(2) %D>% `[`(4:5)
#' 
#' # View steps of chain in the viewer
#' iris %V>% head(2) %V>% `[`(4:5)
#'
#' # Log steps in the console
#' iris %L>% {Sys.sleep(1);head(.,2)} %L>% {Sys.sleep(2);.[4:5]}
#' }
#' 
#' # Use print or summary on output
#' iris %P>% head(2) %P>% `[`(4:5)
#' iris %summary>% head(2) %summary>% `[`(4:5)
#' 
#' \dontrun{
#' # Use glimpse or skim on output
#' iris %glimpse>% head(2) %glimpse>% `[`(4:5) 
#' iris %skim>% head(2) %skim>% `[`(4:5) 
#' }
#' 
#' # Try an operation and ignore if failure
#' iris %try>% {stop("FAILURE!")} %>% head(2)
#' 
#' # Silence warnings or messages
#' -1 %nowarn>% sqrt
#' iris %nomsg>% {message("allo?");head(.,2)}
#'
#' # Fail on a warning
#' \dontrun{
#' -1 %strict>% sqrt
#' }
#' 
#' @name pipeops
NULL

#' @rdname pipeops
#' @export
`%>%`  <- new_pipe()

#' @rdname pipeops
#' @export
`%<>%` <- new_pipe({BODY})

#' @rdname pipeops
#' @export
`%T>%` <- new_pipe({local(BODY);.})

#' @rdname pipeops
#' @export
`%$%` <- new_pipe(with(., BODY)) 

#' @rdname pipeops
#' @export
`%nowarn>%` <- new_pipe(suppressWarnings(BODY))

#' @rdname pipeops
#' @export
`%nomsg>%` <- new_pipe(suppressMessages(BODY))

#' @rdname pipeops
#' @export
`%strict>%` <- new_pipe({
  current_warn <- options()$warn
  options(warn = 2)
  on.exit(options(warn = current_warn))
  BODY})

#' @rdname pipeops
#' @export
`%try>%` <- new_pipe(
  {res <- try(BODY); if (inherits(res,"try-error")) . else res})

#' @rdname pipeops
#' @export
`%P>%` <- new_pipe({
  message(deparse(quote(BODY)))
  . <- print(BODY)
  cat("\n")
  .})

#' @rdname pipeops
#' @export
`%V>%` <- new_pipe({
  . <- BODY
  View(., deparse(quote(BODY)))
  .})

#' @rdname pipeops
#' @export
`%D>%` <- new_pipe({
    pipe_browse <- as.function(alist(BODY))
    debugonce(pipe_browse)
    pipe_browse()})

#' @rdname pipeops
#' @export
`%summary>%` <- new_pipe({
  message(deparse(quote(BODY)))
  . <- BODY; print(summary(.))
  cat("\n")
  .})

#' @rdname pipeops
#' @export
`%glimpse>%` <- new_pipe({
  if (!requireNamespace("tibble"))
    stop("The package `tibble` must be installed to use `%glimpse>%`")
  message(deparse(quote(BODY)))
  . <- BODY
  print(tibble::glimpse(.))
  cat("\n")
  .})

#' @rdname pipeops
#' @export
`%skim>%` <- new_pipe({
  if (!requireNamespace("skimr"))
    stop("The package `skimr` must be installed to use `%skim>%`")
  message(deparse(quote(BODY)))
  . <- BODY
  print(skimr::skim(.))
  cat("\n")
  .})

#' @rdname pipeops
#' @export
`%L>%` <- new_pipe({
  cat(paste(deparse(quote(BODY)), collapse = "\n"),"  ~ ...")
  cat("\b\b\b",system.time(. <- BODY)[3],"sec\n")
  .
  })

#' @rdname pipeops
#' @export
`%ae>%` <- new_pipe({
  message(deparse(quote(BODY)))
  output <- BODY
  # `ignore*` parameters are passed to the tbl_df method
  ae <- all.equal(., output, 
                  ignore_col_order = FALSE, 
                  ignore_row_order = FALSE)
  if (isTRUE(ae)) cat("The objects are the same\n") else print(ae)
  cat("\n")
  output
})

#' @rdname pipeops
#' @export
`%compare>%` <- new_pipe({
  if (!requireNamespace("arsenal"))
    stop("The package `arsenal` must be installed to use `%compare>%`")
  message(deparse(quote(BODY)))
  output <- BODY
  txt <- paste(collapse = "\n",capture.output(summary(arsenal::compare(., output))))
  f <- tempfile(fileext = ".html")
  cat(txt,file = f)
  rmarkdown::render(f)
  shell(f)             
  cat("\n")
  output
})

#' @rdname pipeops
#' @export
`%quietly>%` <- new_pipe({
  if (!requireNamespace("purrr"))
    stop("The package `purrr` must be installed to use `%quietly>%`")
  if (!requireNamespace("rlang"))
    stop("The package `rlang` must be installed to use `%quietly>%`")
  cat(deparse(quote(BODY)),"\n")
  fml <- eval(bquote(~.(quote(BODY))))
  fun <- rlang::as_function(fml)
  output <- purrr::quietly(fun)(.)
  msg <- purrr::compact(output[-1])
  msg <- msg[msg!=""]
  if(length(msg)) print (msg)
  cat("\n")
  output[[1]]
})


#' @rdname pipeops
#' @export
`%gg>%` <- new_pipe({
  rhs <- quote(BODY)
  ggm <- eval(substitute(ggmethod(FUN), list(FUN = rhs[[1]])))
  rhs[[1]] <- quote(ggm)
  eval(rhs)
})


# copied from gg fun but not exported here
# from a function that modifies a data.frame, create a function than modifies
# the data of a gg oject
ggmethod <- function(f){
  rlang::new_function(
    # arguments of the new function are the same, except that 1st argument is gg
    args = c(alist(gg = ),formals(f)[-1]),
    
    body = substitute({
      # build the body of the fun pplied on `data`
      # this fun's body will be a variation on the match.call() of current fun
      mc <- match.call()
      mc[[1]] <- quote(f)
      pos_gg <- which(names(mc) == "gg")
      fun_call <- as.call(c(mc[[1]],quote(x), as.list(mc[-c(1,pos_gg)])))
      data <- gg$data
      fun <- rlang::new_function(
        # the fun applied on data should have only one arg
        alist(x = ),
        fun_call)
      if (is.function(data)) {
        # if data is function, compose with our function
        gg$data <- purrr::compose(fun, data)
      } else if (inherits(data,"waiver")) {
        # if data is a waiver, replace it by our function
        gg$data <- fun
      } else if (is.data.frame(data)) {
        # if data is a data frame, apply function on it
        gg$data <- fun(data)
      } else {
        stop("unexpected class")
      }
      gg
    }))
}