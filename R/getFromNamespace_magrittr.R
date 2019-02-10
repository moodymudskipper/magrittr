# we have our own versions of
# freduce (the current CRAN version hardcoded to be safe as github version is different)
# is_pipe
# wrap_functions

# we added pipe operators and functions
# new_pipe
# print.pipe
# pif
# pprint

# the only unexported function we need as pretty much everything is called
# through it
pipe <- getFromNamespace("pipe","magrittr")

# from debug_pipe.R
#' @export
debug_pipe <- getFromNamespace("debug_pipe","magrittr")
#' @export
debug_fseq <- getFromNamespace("debug_fseq","magrittr")
#' @export
undebug_fseq <- getFromNamespace("undebug_fseq","magrittr")

# from functions.R
#' @export 
functions <- getFromNamespace("functions","magrittr")
#' @export 
print.fseq <- getFromNamespace("print.fseq","magrittr")

# from getters.R
#' @export 
`[[.fseq` <- getFromNamespace("[[.fseq","magrittr")
#' @export 
`[.fseq` <- getFromNamespace("[.fseq","magrittr")

