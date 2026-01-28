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
  # TODO match dplyr or at least use ?dplyr_extending in this default,
  # and write other methods for the ukeyed things?
  #
  # XXX plus this may also have a problem with extensibility... we're
  # redispatching a bunch... Should either be a non-S3 function with a
  # bunch of dispatching, or should be stripping to "self" class
  # before redispatching... but if try latter, since this is default,
  # what would be self class?  Plus seems like this would require
  # dplyr_reconstruct calls or all subclasses to impl this "3rd-party"
  # generic...
  #
  # XXX should this be list_of?
  lapply(is, function(i) {
    x[i, j]
  })
}
