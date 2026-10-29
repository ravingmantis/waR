# assign.all(x = 1, y = 2) is approximately x <- 1 ; y <- 2
# This is useful for copy-pasting function arguments to set as if we're within the function
assign.all <- function (...) {
  args <- list(...)
  for (n in names(args)) {
    if (nzchar(n)) assign(n, args[[n]], envir = globalenv())
  }
}
if (FALSE) {
  assign.all(x = 5, y = 4)
}
