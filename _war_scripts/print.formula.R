print.formula <- function (x, ...) {
    print.default(x);
    if (!identical(environment(x), globalenv())) str(
        as.list(environment(x)),
        list.len = 5,
        deparse.lines = 3 )
}
