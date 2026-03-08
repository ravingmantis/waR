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

    for (s in Sys.glob("_war_scripts/*.R")) source(s)

    .war_histfile <- normalizePath('.Rhistory', mustWork = FALSE)
    if (!file.exists(.war_histfile)) writeLines("", con = .war_histfile)
    .war_histfile <- normalizePath('.Rhistory', mustWork = TRUE)
    writeLines(sprintf("R history saved to %s\n", .war_histfile))

    options(tibble.print_max = 40)
    options(tibble.width = Inf)
    options(unittest.stop_on_fail=TRUE)

    .First <- function () {
        run_cmd <- function(cmd, hist_append = FALSE, ...) {
            if (is.call(cmd)) cmd <- deparse1(cmd, collapse = "\n")

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

            # NB: base::withAutoprint is nearly what we want, but doesn't parse strings
            psource(
                textConnection(cmd),
                echo = TRUE,
                max.deparse.length = 1e6L,
                width.cutoff = 1e6L,
                deparseCtrl = "all",
                ... )
        }

        # Run base::.First.sys now, so defaultPackages are attached.
        # Normally this would happen immediately after running .First(),
        # but we need them attached so commands can run in a sane environment.
        base::.First.sys()

        # Act on each argument
        renv_activated <- FALSE
        for (arg in commandArgs(trailingOnly = TRUE)) {
            if (isTRUE(file.info(arg)$isdir)) {
                if (file.exists(file.path(arg, "renv.lock"))) {
                    # Activate contained renv environment
                    run_cmd(call("psource", file.path(arg, "renv", "activate.R"), echo = FALSE))
                    renv_activated <- TRUE
                }
                if (file.exists(file.path(arg, "DESCRIPTION"))) {
                    if (FALSE && isTRUE(renv_activated)) {
                        run_cmd(substitute(
                            renv::install(paste0("local::", arg), prompt = FALSE, dependencies = c()),
                            list( arg = arg )), r_options = list(warn = 2))
                    } else if (FALSE && requireNamespace("pak", quietly = TRUE)) {
                        run_cmd(substitute(
                            pak::local_install(arg, upgrade = FALSE, ask = FALSE, dependencies = FALSE),
                            list( arg = arg )), r_options = list(warn = 2))
                    } else if (FALSE && requireNamespace("remotes", quietly = TRUE)) {
                        run_cmd(substitute(
                            remotes::install_local(arg, force = TRUE, upgrade = "never"),
                            list( arg = arg )), r_options = list(warn = 2))
                    } else {
                        run_cmd(substitute(
                            utils::install.packages(arg, repos = NULL, dependencies = FALSE, verbose = TRUE),
                            list( arg = arg )), r_options = list(warn = 2))
                    }
                }
            } else if (file.exists(arg)) {
                # File ==> Try to source it
                run_cmd(call("psource", arg), hist_append = TRUE)
            } else if (identical(arg, 'last')) {
                # 'last' ==> Run last command in history
                get_last <- function () {
                    # Inspired by utils::history
                    tmp_path <- tempfile("run_last")
                    on.exit(unlink(tmp_path), add = TRUE)
                    utils::savehistory(tmp_path, .war_histfile)

                    utils::tail(readLines(tmp_path), 1)
                }

                run_cmd(get_last())
            } else if (!identical(attr((m <- regexec( '^([A-Za-z0-9_]+)=(.*)$', arg ))[[1]], 'match.length'), -1L)) {
                # MOO=baa ==> set an environment variable
                m <- regmatches(arg, m)[[1]]
                out <- call("Sys.setenv", m[[3]])
                names(out)[[2]] <- m[[2]]

                run_cmd(out)
            } else {
                # Otherwise assume R code
                run_cmd(arg, hist_append = TRUE)
            }
        }
    }
    .Last <- function() {
        utils::savehistory(.war_histfile)
    }
}
