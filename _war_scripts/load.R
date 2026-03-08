# Force load to be verbose by default
load <- function (file, envir = parent.frame(), verbose = TRUE) base::load(file, envir = envir, verbose = verbose)
