# new functions are in this file
# other modified functions are :
# is_pipe in is_something.R
# pipe in pipe.R
# `%T>%` in pipe.R
# `%$>%` in pipe.R
# wrap_function in wrap_function.R
# we had to go back to the old freduce so that our debug calls are called
# in order

print.pipe <- function(x,...){
  message("Pipe operator")
  cat("body:\n")
  print(attr(x,"body"))
  invisible(x)
}

#' Pipe operators to enhance magrittr
#'
#' These operators return the same as magrittr's standard pipe operator but have
#' additional side effects.
#'
#' \describe{
#'   \item{\%D>\%}{debugs the pipe chain at the relevant step}
#'   \item{\%V>\%}{uses \code{View()} on the output}
#'   \item{\%L>\%}{logs the relevant call to the console and time it}
#'   \item{\%P>\%}{uses \code{print()} on the output}
#'   \item{\%summary>\%}{prints the \code{summary()} off the output}
#'   \item{\%glimpse>\%}{uses \code{tibble::glimpse} on the output}
#'   \item{\%skim>\%}{uses \code{skimr::skim} on the output}
#'   \item{\%C>\%}{clocks the relevant step}
#'   \item{\%nowarn>\%}{silence warnings}
#'   \item{\%nomsg>\%}{silence messages}
#'   \item{\%strict>\%}{fail on warning}
#'   \item{\%try>\%}{tries, if failure prints error and returns input}
#' }
#'
#' @inheritParams magrittr::`%>%`
#'
#' @examples
#' # silence a warning
#' data.frame(a= c(1,-1)) %nowarn>% transform(a=sqrt(a))
#'
#' # view stepps of chain in the viewer
#' iris %V>% head(2) %V>% `[`(4:5)
#'
#' # log steps in the console
#' iris %L>% {Sys.sleep(1);head(.,2)} %L>% {Sys.sleep(2);.[4:5]}
#'
#' # use print, summary or glimpse on output
#' iris %P>% head(2) %P>% `[`(4:5)
#' iris %S>% head(2) %S>% `[`(4:5)
#' iris %G>% head(2) %G>% `[`(4:5)
#'
#' # debug the chain
#' iris %>% head(2) %D>% `[`(4:5)
#' @name pipeops
NULL

#' @rdname pipeops
#' @export
`%nowarn>%` <- pipe(substitute(suppressWarnings(BODY), list(BODY = body)))

#' @rdname pipeops
#' @export
`%nomsg>%` <- pipe(substitute(suppressMessages(BODY), list(BODY = body)))

#' @rdname pipeops
#' @export
`%strict>%` <- pipe(substitute(
      {options(warn = 2); on.exit(options(warn = CURRENT_WARN)); BODY},
      list(CURRENT_WARN = options()$warn, BODY = body)))

#' @rdname pipeops
#' @export
`%try>%` <- pipe(substitute(
  {res <- try(BODY); if (inherits(res,"try-error")) . else res},
  list(BODY = body)))

#' @rdname pipeops
#' @export
`%P>%` <- pipe(substitute({. <- print(BODY);cat("\n");.}, list(BODY = body)))

#' @rdname pipeops
#' @export
`%L>%` <- pipe(substitute({print(quote(BODY)); BODY}, list(BODY = body)))

#' @rdname pipeops
#' @export
`%V>%` <- pipe(substitute(
  {. <- BODY; View(., TITLE); .},
  list(BODY = body, TITLE = deparse(body))))

#' @rdname pipeops
#' @export
`%D>%` <- pipe(substitute({
    pipe_browse <- as.function(alist(BODY))
    debugonce(pipe_browse)
    pipe_browse()},
    list(BODY = body)))

#' @rdname pipeops
#' @export
`%summary>%` <- pipe(substitute(
  {. <- BODY; print(summary(.)); cat("\n"); .}, 
  list(BODY = body)))

#' @rdname pipeops
#' @export
`%glimpse>%` <- pipe(substitute({
  if (!requireNamespace("tibble"))
    stop("The package `tibble` must be installed to use `%glimpse>%`")
  . <- BODY
  print(tibble::glimpse(.))
  cat("\n")
  .}, 
  list(BODY = body)))

#' @rdname pipeops
#' @export
`%skim>%` <- pipe(substitute({
  if (!requireNamespace("skimr"))
    stop("The package `skimr` must be installed to use `%skim>%`")
  . <- BODY
  print(skimr::skim(.))
  cat("\n")
  .}, 
  list(BODY = body)))

#' @rdname pipeops
#' @export
`%C>%` <- pipe(substitute({
  cat(deparse(quote(BODY)),"  ~...")
  cat("\b\b\b",system.time(. <- BODY)[3],"sec\n")
  .
  }, list(BODY = body)))

#' @rdname pipeops
#' @export
`%ae>%` <- pipe(substitute({
  message(paste("%>%",deparse(quote(b))))
  output <- b
  # `ignore*` parameters are passed to the tbl_df method
  ae <- all.equal(., output, 
                  ignore_col_order = FALSE, 
                  ignore_row_order = FALSE)
  if (isTRUE(ae)) cat("The objects are the same\n") else print(ae)
  cat("\n")
  output
}, list(b = body)))

#' @rdname pipeops
#' @export
`%compare>%` <- pipe(substitute({
  message(paste("%>%",deparse(quote(b))))
  output <- b
  # `ignore*` parameters are passed to the tbl_df method
  print(summary(arsenal::compare(., output)))
  cat("\n")
  output
}, list(b = body)))