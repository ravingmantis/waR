psource <- function(file_path, environ = c(), r_options = list(), ...) {
    senv <- function (e) {
        e <- split(e, !is.na(e))

        if (length(e[["TRUE"]]) > 0) do.call(Sys.setenv, as.list(e[["TRUE"]]))
        if (length(e[["FALSE"]]) > 0) Sys.unsetenv(names(e[["FALSE"]]))
    }
    if (length(environ) > 0) {
        old_environ <- Sys.getenv(names(environ), unset = NA, names = TRUE)
        on.exit(senv(old_environ), add = TRUE, after = TRUE)
        senv(environ)
    }

    if (length(r_options) > 0) {
        old_options <- options()[names(r_options)]
        names(old_options) <- names(r_options)  # NB: unset options won't have a name, restore it
        on.exit(options(old_options), add = TRUE, after = TRUE)
        options(r_options)
    }

    as_ut_script <- FALSE

    if (is.character(file_path)) {
        old_wd <- getwd()
        on.exit(setwd(old_wd), add = TRUE, after = TRUE)

        parts <- strsplit(file_path, "/")[[1]]
        for (i in seq_along(parts)) {
            # File might be in waR root
            if (!dir.exists(parts[[i]])) break

            # Assume if Makefile.waR exists we're in waR directory
            if (!file.exists('Makefile.waR')) break
            setwd(parts[[i]])
        }
        file_path <- do.call(file.path, as.list(parts[i:length(parts)]))

        if (endsWith(file_path, ".Rmd") || endsWith(file_path, ".qmd")) {
            tmp_script <- tempfile(basename(file_path), fileext = ".R")
            knitr::purl(file_path, output = tmp_script)
            file_path <- tmp_script
        }
        if (endsWith(file_path, ".R")) {
            as_ut_script <- TRUE
        }
    } else if (is.call(file_path)) {
        file_path <- textConnection(deparse1(file_path, collapse = "\n"))
    }

    source_opts <- c(list(...), list(
        echo = TRUE,
        max.deparse.length = 1e6L,
        width.cutoff = 1e6L,
        deparseCtrl = "all" ))
    source_opts <- source_opts[!duplicated(names(source_opts))]

    # Save command history in case script blows up R session
    if (interactive()) utils::savehistory(.war_histfile)

    if (as_ut_script) {
        out <- unittest:::ut_with_report(do.call(source, c(list(file_path), source_opts)))
    } else {
        out <- do.call(source, c(list(file_path), source_opts))
    }
    return(out)
}
