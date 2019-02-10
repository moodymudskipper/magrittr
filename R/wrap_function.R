# Wrap an expression in a function
# 
# This function takes the "body" part of a function and wraps it in
# a function. The return value depends on whether the function is created
# for its side effect with the tee operator. If the operator is \code{\%$\%}
# then the expression will be evaluated in a `with(., )` statement.
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