strl <- function(...) {
    args <- if (...length() > 0) list(...) else as.list(parent.frame())
    if (is.null(names(args))) names(args) <- rep("", length(args))

    missing_args <- !nzchar(names(args))
    # NB: Skip "strl" name
    names(args)[missing_args] <- as.character(sys.call(0))[c(FALSE, missing_args)]

    return(str(
        args,
        vec.len = 1e5,
        digits.d = 5,
        nchar.max = 1e3,
        list.len = 1e5 ))
}
if (FALSE) {
    moo <- 5
    strl(moo, "oink")
}
