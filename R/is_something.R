# Check whether a symbol is a valid magrittr pipe.
#
# @param pipe A quoted symbol
# @return logical - TRUE if a valid magrittr pipe, FALSE otherwise.
is_pipe <- function(pipe)
{
  inherits(eval(pipe),"pipe")
}
