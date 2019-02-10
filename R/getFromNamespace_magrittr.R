# we have our own versions of
# freduce (the current CRAN version hardcoded to be safe as github version is different)
# is_pipe
# wrap_functions

# we added pipe operators and functions
# new_pipe
# print.pipe
# pif
# pprint

# pipe is imported but we need it in our own namespace to be able to use
# our freduce, and wrap_functions
# we will need to do the same with split_chain so we can use our is_pipe
pipe0 <- getFromNamespace("pipe","magrittr")
pipe  <-  as.function(c(list(body(pipe0))))
rm(pipe0)

split_chain0 <- getFromNamespace("split_chain","magrittr")
split_chain <-   as.function(c(formals(split_chain0),body(split_chain0)))
rm(split_chain0)

is_parenthesized <- getFromNamespace("is_parenthesized","magrittr")
is_dollar        <- getFromNamespace("is_dollar","magrittr")
is_funexpr       <- getFromNamespace("is_funexpr","magrittr")
is_function      <- getFromNamespace("is_function","magrittr")
prepare_function <- getFromNamespace("prepare_function","magrittr")
prepare_first    <- getFromNamespace("prepare_first","magrittr")
is_placeholder   <- getFromNamespace("is_placeholder","magrittr")
is_compound_pipe   <- getFromNamespace("is_compound_pipe","magrittr")
is_first        <- getFromNamespace("is_first","magrittr")



# from debug_pipe.R

#' debug_pipe
#' 
#' Reexported from \code{\link[magrittr]{debug_pipe}}
#' @inheritParams magrittr::debug_pipe
#' @export
debug_pipe <- magrittr::debug_pipe

#' debug_fseq
#' 
#' Reexported from \code{\link[magrittr]{debug_fseq}}
#' @inheritParams magrittr::debug_fseq
#' @export
debug_fseq <- magrittr::debug_fseq

#' undebug_fseq
#' 
#' Reexported from \code{\link[magrittr]{debug_fseq}}
#' @inheritParams magrittr::undebug_fseq
#' @export
undebug_fseq <- magrittr::undebug_fseq

# from functions.R

#' functions
#' 
#' Reexported from \code{\link[magrittr]{functions}}
#' @inheritParams magrittr::functions
#' @export
functions <- magrittr::functions

#' `[.fseq`
#' 
#' Reexported from \code{\link[magrittr]{[.fseq}}
#' @inheritParams magrittr::`[.fseq`
#' @export
`[.fseq` <- getFromNamespace("[.fseq","magrittr")

#' `[[.fseq`
#' 
#' Reexported from \code{\link[magrittr]{[.fseq}}
#' @inheritParams magrittr::`[.fseq`
#' @export
`[[.fseq` <- getFromNamespace("[.fseq","magrittr")

#' print.fseq
#' 
#' Reexported from \code{\link[magrittr]{print.fseq}}
#' @inheritParams magrittr::print.fseq
#' @export
print.fseq <- getFromNamespace("print.fseq","magrittr")


