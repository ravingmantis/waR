# See also: dump.frames()
hijack <- function (...) {
    if (...length() > 0) {
        args <- list(...)
    } else if (!identical(parent.frame(), globalenv())) {
        # Dump environment of caller
        args <- as.list(parent.frame())
    } else if (length(sys.frames()) >= 3) {
        # Parent is globalenv, not useful to dump probably called via options(error = hijack)
        # First 2 are our machinery, want third
        args <- as.list( tail(sys.frames(), 3)[[1]] )
    } else {
        args <- list()
    }

    anames <- if (is.null(names(args))) rep("", length(args)) else names(args)
    for (i in seq_along(args)) {
        assign(
            (if (nzchar(anames[[i]])) anames[[i]] else as.character(sys.call(0)[[i + 1]])),
            args[[i]],
            globalenv())
    }
    return(args[[1]])
}
if (FALSE) {
    (function () { camel <- 2 ; hijack()  })()
    (function () { camel <- 2 ; hijack(camel)  })()
    (function () { camel <- 4 ; frog  <- 7 ; hijack(camel, ribbit = frog)  })()
}
