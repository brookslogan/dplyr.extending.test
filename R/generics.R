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

ukeyed_destructure <- function(x) UseMethod("ukeyed_destructure")
ukeyed_restructure <- function(super, self_info) UseMethod("ukeyed_restructure")

#' @export
ukeyed_destructure.default <- function(x) {
  list(x, reconstruct_as_is(x))
}

# ukeyed_destructure_impl_ukeyed

# destructure:
# - subclass
# - self
# - superclass

# restore checked:
# - we have superclass's result
# - reconstruct (subclass, self, reconstruct_as_is, parent_original)

# restore unchecked:
# - we have superclass's result
# - reconstruct (subclass, reconstruct_as_is, self, parent_original)

# reconstruct checked

# reconstruct unchecked??

#' @export
ukeyed_destructure.keyed_df4 <- function(x) {
  x_class <- class(x)
  x_self_ind <- match(.Method, x_class)
  x_subclass <- x_class[seq_len(x_self_ind - 1L)]
  parent <- x
  class(parent) <- x_class[(x_self_ind + 1L):length(x_class)]
  parent_destructured <- ukeyed_destructure(parent)
  template <- new_keyed_df4(parent_destructured[[2L]], ....)
  list(parent_destructured[[1L]], x)
  parent
}

#' @export
ukeyed_restore.keyed_df4 <- function(super, template) {




  template <- new_keyed_df4(template, result_ukey_nms_else_null)
  result <- new_keyed_df4(result, result_ukey_nms_else_null)
  template <- reconstruct_as_is(template)
  class(template) <- c(x_subclass, class(template))
  result <- partial_reconstruct(result, template)
}

# extract 0-row?

ukeyed_proxy.keyed_df4 <- function(x) {
  x_class <- class(x)
  x_self_ind <- match(.Method, x_class)
  parent <- x
  class(parent) <- x_class[(x_self_ind + 1L):length(x_class)]
  parent_proxy <- ukeyed_proxy(parent)
  parent_proxy
}

ukeyed_restore.keyed_df4 <- function(x, to) {

}


use <- function() {
  y_out_template <- y[0,][-by]
  y <- ukeyed_proxy(y)
  NextMethod()
  proxy_ptype <- attr(result[[name]], "ptype")
  lapply(result[[name]], ukeyed_restore, .....)
}


ukeyed_destructure <- function(x) {
  x_class <- class(x)
  x_self_ind <- match(.Method, x_class)
  x_subclass <- x_class[seq_len(x_self_ind - 1L)]
  parent <- x
  class(parent) <- x_class[(x_self_ind + 1L):length(x_class)]
  parent_destructured <- ukeyed_destructure(parent)
  template <- new_keyed_df4(parent_destructured[[2L]], ....)
  list(parent_destructured[[1L]], x, )
  parent
}

# XXX what if given ukey not supported and self decays or transforms?

# approach:
# 1. to parent
# 2. orig[0,][-by]
# 3. special reconstruct
# issues matching 0slice class vector with orig class vector


# placeholder approach
# sub<ukeyed_placeholder<sup>>
#
# dplyr_reconstruct.ukeyed_placeholder <- function(data, template) {
#   data <- NextMethod()
#   data <- dplyr_reconstruct(data, template self as_is parent %>% set_ukey_or_decay() WRONG)
#   data <-
#   data <-
# }


# result <- inner_result
# result <- dplyr_reconstruct(inner_result, zero slice last ukeyed as_is parents)
# result <- dplyr_reconstruct(inner_result, unsliced after second-last ukeyed ... as_is last ukeyed ...)
# result <- dplyr_reconstruct(inner_result, zero slice second-last ukeyed as_is parents)
# result <- dplyr_reconstruct(inner_result, unsliced subclass ... as_is last ukeyed ...)

# but slicing may not delegate the same way...


# parent_chop_extraction_restore <- function(chop_extraction, origin, j) UseMethod("parent_chop_extraction_restore", origin)

# #' @export
# parent_chop_extraction_restore.keyed_df4 <- function(chop_extraction, origin, j) {
#   result_elt_template <- dplyr_row_slice(origin, integer())[j]
#   # FIXME reconstruct_as_is in appropriate place...
#   lapply(
#     chop_extraction, dplyr_reconstruct, result_elt_template
#   )
#   # FIXME nonsliced handling...
# }


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
