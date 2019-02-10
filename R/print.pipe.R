print.pipe <- function(x,...){
  message("Pipe operator")
  cat("wrap:\n")
  print(attr(x,"wrap"))
  invisible(x)
}