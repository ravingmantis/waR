# Merge multi-line commands in R history into single lines
# The R REPL adds each line of a multi-line command to history separately,
# this regroups them by parsing, so they can be recalled as one.
hist_merge <- function (lines, max_group = 200L) {
    parse_status <- function (txt) tryCatch({
        parse(text = txt, keep.source = FALSE)
        "ok"
    }, error = function (e) {
        if (grepl("unexpected (end of input|INCOMPLETE_STRING)", conditionMessage(e))) "incomplete" else "error"
    })

    # Turn a multi-line command into a single line, or NULL if we can't
    join_lines <- function (txt) {
        pd <- tryCatch(
            utils::getParseData(parse(text = txt, keep.source = TRUE), includeText = TRUE),
            error = function (e) NULL)
        if (is.null(pd)) return(NULL)
        pd <- pd[pd$terminal, , drop = FALSE]
        pd <- pd[order(pd$line1, pd$col1), , drop = FALSE]

        # Tokens spanning lines (e.g. multi-line strings) can't be flattened
        if (any(pd$line1 != pd$line2)) return(NULL)

        comments <- pd$text[pd$token == "COMMENT"]
        pd <- pd[pd$token != "COMMENT", , drop = FALSE]

        # Tokens after which a newline may end an expression
        can_end <- c("SYMBOL", "NUM_CONST", "STR_CONST", "NULL_CONST", "SYMBOL_FUNCTION_CALL",
            "')'", "']'", "'}'", "NEXT", "BREAK")
        out <- character(0)
        brackets <- character(0)
        closed_header <- FALSE  # Did the last ) close e.g. if (...), so it can't end an expression?
        for (i in seq_len(nrow(pd))) {
            tok <- pd$token[[i]]
            if (i > 1) {
                if (pd$line1[[i]] != pd$line1[[i - 1]]) {
                    # Newline: separates expressions unless within ( or [, or continuing
                    in_braces <- length(brackets) == 0 || brackets[[length(brackets)]] == "{"
                    ends <- pd$token[[i - 1]] %in% can_end && !(pd$token[[i - 1]] == "')'" && closed_header)
                    sep <- if (in_braces && ends && !(tok %in% c("ELSE", "'}'"))) "; " else " "
                } else {
                    sep <- if (pd$col1[[i]] == pd$col2[[i - 1]] + 1) "" else " "
                }
                out <- c(out, sep)
            }
            out <- c(out, pd$text[[i]])

            if (tok == "'('") {
                is_header <- i > 1 && pd$token[[i - 1]] %in% c("FUNCTION", "IF", "FOR", "WHILE", "'\\\\'")
                brackets <- c(brackets, if (is_header) "(h" else "(")
            }
            if (tok %in% c("'['", "'{'")) brackets <- c(brackets, gsub("'", "", tok))
            if (tok == "LBB") brackets <- c(brackets, "[[")
            if (tok == "')'") closed_header <- identical(utils::tail(brackets, 1), "(h")
            if (tok %in% c("')'", "'}'")) brackets <- utils::head(brackets, -1)
            if (tok == "']'") {
                # First ] of ]] turns [[ into [, second closes it
                if (utils::tail(brackets, 1) == "[[") brackets[[length(brackets)]] <- "[" else brackets <- utils::head(brackets, -1)
            }
        }
        out <- paste(c(paste(out, collapse = ""), comments), collapse = " ")

        # Make sure we haven't changed the meaning
        same <- tryCatch(identical(
            parse(text = out, keep.source = FALSE),
            parse(text = txt, keep.source = FALSE)), error = function (e) FALSE)
        if (same) out else NULL
    }

    out <- character(0)
    i <- 1L
    while (i <= length(lines)) {
        # Find the end of the command starting at i
        status <- parse_status(lines[[i]])
        j <- i
        while (status == "incomplete" && j < length(lines) && j - i < max_group) {
            j <- j + 1L
            status <- parse_status(lines[i:j])
        }

        joined <- if (status == "ok" && j > i) join_lines(lines[i:j]) else NULL
        if (is.null(joined)) {
            # Not a mergeable group, leave first line as-is and carry on from the next
            out <- c(out, lines[[i]])
            i <- i + 1L
        } else {
            out <- c(out, joined)
            i <- j + 1L
        }
    }
    out
}
if (FALSE) {
    hist_merge(c(
        "x <- c(1,",
        "  2)",
        "f <- function (a) {",
        "    b <- a + 1  # inc",
        "    if (b > 2) b",
        "    else a",
        "}",
        "y <- 3"))
    # "x <- c(1, 2)"  "f <- function (a) { b <- a + 1; if (b > 2) b else a } # inc"  "y <- 3"
    hist_merge(c("g <- function (x)", "  x + 1"))  # "g <- function (x) x + 1"
}
