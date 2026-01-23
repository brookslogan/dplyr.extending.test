
#' @export
reconstruct_as_is <- function(df) {
  old_class <- class(df)
  if ("reconstruct_as_is" %in% old_class) {
    # XXX do we need to support appearing twice in class vector?
    # (maybe by having an attr with a vector of "manager class" names
    # or perhaps freshly-invented IDs?) or just require each class
    # that uses to add late enough and remove early enough to not
    # conflict with any other?
    cli_abort("`df` is already a `reconstruct_as_is`")
  }
  class(df) <- c("reconstruct_as_is", old_class)
  df
}

#' @export
as_not_reconstruct_as_is <- function(df) {
  old_class <- class(df)
  i <- match("reconstruct_as_is", old_class)
  if (is.na(i)) {
    cli_abort("`df` was not a `reconstruct_as_is`")
  }
  class(df) <- old_class(-i)
  df
}

#' @export
dplyr_reconstruct.reconstruct_as_is <- function(data, template) {
  data
}

#' Variant of `dplyr_reconstruct` that simply calls the method impl, rather than unclassing `data`
partial_reconstruct <- function(data, template) {
  # TODO notes on which we should use for which attrs
  UseMethod("dplyr_reconstruct", template)
}
# XXX might have problems if a `dplyr_reconstruct` impl manually forwards to `dplyr_reconstruct`?
