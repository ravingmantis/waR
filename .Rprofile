# .libPaths("./Rpackages")

if(interactive()) {
    # https://stackoverflow.com/questions/1975110/printing-stack-trace-and-continuing-after-error-occurs-in-r
    options(warn = 0, keep.source = TRUE, error = quote({
      # Debugging in R
      #   http://www.stats.uwo.ca/faculty/murdoch/software/debuggingR/index.shtml
      #
      # Post-mortem debugging
      #   http://www.stats.uwo.ca/faculty/murdoch/software/debuggingR/pmd.shtml
      #
      # Relation functions:
      #   dump.frames
      #   recover
      # >>limitedLabels  (formatting of the dump with source/line numbers)
      #   sys.frame (and associated)
      #   traceback
      #   geterrmessage
      #
      # Output based on the debugger function definition.

      # TODO: setup option for dumping to a file (?)
      # Set `to.file` argument to write this to a file for post-mortem debugging
      dump.frames()  # writes to last.dump
      n <- length(last.dump)
      if (n > 0) {
        calls <- names(last.dump)
        cat("Environment:\n", file = stderr())
        cat(paste0("  ", seq_len(n), ": ", calls), sep = "\n", file = stderr())
        cat("\n", file = stderr())
      }

      if (!interactive()) q()
    }))

    hijack <- function (...) {
        args <- if (...length() > 0) list(...) else as.list(parent.frame())
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

    psource <- function(file_path, environ = c(), ...) {
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

        old_wd <- getwd()
        on.exit(setwd(old_wd), add = TRUE, after = TRUE)

        parts <- strsplit(file_path, "/")[[1]]
        for (i in seq_along(parts)) {
            # Assume if .Rprofile exists we're in waR directory
            if (!file.exists('.Rprofile') && (file.exists('DESCRIPTION') || dir.exists('.git'))) break
            print(c(i, parts[[i]], file.exists('.Rprofile'), file.exists('DESCRIPTION'), dir.exists('.git')))
            setwd(parts[[i]])
        }
        file_path <- do.call(file.path, as.list(parts[i:length(parts)]))

        # Save command history in case script blows up R session
        if (interactive()) utils::savehistory()

        if (endsWith(file_path, ".Rmd")) {
            tmp_script <- tempfile(basename(file_path), fileext = ".R")
            knitr::purl(file_path, output = tmp_script)
            source(tmp_script, ...)
        } else {
            source(file_path, ...)
        }
    }

    print.formula <- function (x, ...) {
        print.default(x);
        if (!identical(environment(x), globalenv())) str(as.list(environment(x)))
    }

    .First <- function () {
        run_cmd <- function(cmd, echo = TRUE, hist_append = FALSE) {
            if (isTRUE(hist_append)) {
                # Write out history to temporary file
                tmp_path <- tempfile("run_cmd")
                on.exit(unlink(tmp_path), add = TRUE)
                utils::savehistory(tmp_path)

                # Append to it
                tmp_f <- file(tmp_path, open = "at")
                writeLines(cmd, con = tmp_f)
                close(tmp_f)

                # Load back in again
                utils::loadhistory(tmp_path)
            }

            if (isTRUE(echo)) writeLines(paste0("> ", cmd))
            eval(parse(text = cmd))
        }

        # Run base::.First.sys now, so defaultPackages are attached.
        # Normally this would happen immediately after running .First(),
        # but we need them attached so commands can run in a sane environment.
        base::.First.sys()

        # Act on each argument
        for (arg in commandArgs(trailingOnly = TRUE)) {
            old_warn <- getOption('warn')
            if (fs::is_dir(arg)) {
                # Directory ==> Try to install it
                options(warn = 2)
                remotes::install_local(arg, force = TRUE, upgrade = "never")
                options(warn = old_warn)
                library(arg, character.only = TRUE, verbose = TRUE)
            } else if (fs::is_file(arg)) {
                # File ==> Try to source it
                psource(arg, echo = TRUE)
            } else if (identical(arg, 'last')) {
                # 'last' ==> Run last command in history
                get_last <- function () {
                    # Inspired by utils::history
                    tmp_path <- tempfile("run_last")
                    on.exit(unlink(tmp_path), add = TRUE)
                    utils::savehistory(tmp_path)

                    utils::tail(readLines(tmp_path), 1)
                }

                run_cmd(get_last())
            } else if (!identical(attr((m <- regexec( '^([A-Za-z0-9_]+)=(.*)$', arg ))[[1]], 'match.length'), -1L)) {
                # MOO=baa ==> set an environment variable
                m <- regmatches(arg, m)[[1]]
                out <- call("Sys.setenv", m[[3]])
                names(out)[[2]] <- m[[2]]

                run_cmd(deparse1(out))
            } else {
                # Otherwise assume R code
                run_cmd(arg, hist_append = TRUE)
            }
        }
    }
    .Last <- function() {
        if (interactive()) utils::savehistory()
    }
}