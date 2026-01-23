#' @export
ukey_colnames_else_null <- function(x) UseMethod("ukey_colnames_else_null")

# vs. "maybe_", "_maybe", "_option"? would want to change other "maybe_" uses...

#' @export
ukey_colnames_else_null.default <- function(x) NULL

#' A set of colnames that act as a *u*nique identifier for rows
#'
#' @export
ukey_colnames <- function(x, x_arg = rlang::caller_arg(x)) {
  result <- ukey_colnames_else_null(x)
  if (is.null(result)) {
    cli::cli_abort(c(
      "`{x_arg}` must have a known ukey",
      "i" = 'Its class was {clz <- class(x); paste(collapse = "", deparse(clz))}',
      ">" = "Convert to a class where we know the ukey, e.g., keyed_df4"
    ))
  } else {
    result
  }
}

chop_extract <- function(x, is, j) {
  UseMethod("chop_extract")
}

#' @export
chop_extract.default <- function(x, is, j) {
  # XXX should this be list_of?
  lapply(is, function(i) {
    x[i, j]
  })
}
