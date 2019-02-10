# Create a pipe operator.
#
# This function is used to a new pipe operators.
# @export
new_pipe <- function(wrap = {BODY})
{
  # build a standard magrittr pipe
  fun <- pipe()
  
  # give it a pipe class
  class(fun) <- c("pipe","function")
  
  # give it an attribute containing wrapper code to be used in wrap_function
  attr(fun,"wrap") <- substitute(wrap)
  
  fun
}

pipe <- function() {
  function(lhs, rhs)
  {
    # the parent environment
    parent <- parent.frame()
    
    # the environment in which to evaluate pipeline
    env    <- new.env(parent = parent)
    
    # split the pipeline/chain into its parts.
    chain_parts <- split_chain(match.call(), env = env)
    
    pipes <- chain_parts[["pipes"]] # the pipe operators.
    rhss  <- chain_parts[["rhss" ]] # the right-hand sides.
    lhs   <- chain_parts[["lhs"  ]] # the left-hand side.
    
    # Create the list of functions defined by the right-hand sides.
    env[["_function_list"]] <- 
      lapply(seq_along(rhss), 
             function(i) wrap_function(rhss[[i]], pipes[[i]], parent))
    
    # Create a function which applies each of the above functions in turn.
    env[["_fseq"]] <-
      `class<-`(eval(quote(function(value) freduce(value, `_function_list`)), 
                     env, env), c("fseq", "function"))
    
    # make freduce available to the resulting function 
    # even if magrittr is not loaded.
    env[["freduce"]] <- freduce 
    
    # Result depends on the left-hand side.
    if (is_placeholder(lhs)) {
      # return the function itself.
      env[["_fseq"]]
    } else {
      # evaluate the LHS
      env[["_lhs"]] <- eval(lhs, parent, parent)
      
      # compute the result by applying the function to the LHS
      result <- withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      
      # If assignment pipe is used, assign result
      if (is_compound_pipe(pipes[[1L]])) {
        eval(call("<-", lhs, result[["value"]]), parent, parent)
        # Otherwise, return it.
      } else {
        if (result[["visible"]]) 
          result[["value"]] 
        else 
          invisible(result[["value"]])
      }
    }
  }
}