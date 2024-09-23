
#' Low-level constructor for nodupe_tibble; use as_nodupe_tibble or use carefully
#'
#' This does not validate that there are no duplicate rows in `x`; use this only
#' if you have already verified that. For a constructor/converter that validates
#' that for you, use `as_nodupe_tibble` instead.
#'
#' @param my_attr dummy attr, just here to test how it moves around
#'
#' @export
new_nodupe_tibble <- function(x, my_attr = 1) {
  if (inherits(x, "decay_nodupe_tibble")) {
    stop("x must not already be a dedupe_tibble")
  }
  if (!inherits(x, "data.frame")) {
    stop("x must be a data.frame")
  }
  class(x) <- c("nodupe_tibble", class(x))
  attr(x, "my_attr") <- my_attr
  x
}

#' Is / why isn't data.frame/subclass `x` compatible with nodupe_tibble invariants
#'
#' @param x data.frame or subclass
#' @return TRUE or str
check_df_nodupe_tibble_compatible <- function(x, my_attr) {
  # TODO proper caller_arg passing
  if (anyDuplicated(x) != 0L || nrow(x) > 1L && ncol(x) == 0L) {
    "contained duplicates"
  } else {
    TRUE
  }
}

#' @export
validate_nodupe_tibble <- function(x) {
  # TODO proper caller_arg passing
  checkmate::assert(check_df_nodupe_tibble_compatible(x, attr(x, "my_attr")))
}

#' Remove nodupe_tibble class & attrs:
#'
#' @keywords internal
decay_nodupe_tibble <- function(x, ...) {
  oldclass <- class(x)
  class(x) <- oldclass[oldclass != "nodupe_tibble"]
  attr(x, "my_attr") <- NULL
  x
}

#' Validate whether x obeys invariants of nodupe_tibble; if not, decay to non-nodupe_tibble:
#'
#' @keywords internal
maybe_decay_nodupe_tibble <- function(x) {
  if (isTRUE(check_df_nodupe_tibble_compatible(x, attr(x, "my_attr")))) {
    decay_nodupe_tibble(x)
  } else {
    x
  }
}

## #' Attach nodupe_tibble class & attrs from template without checking validity
## #'
## #' This is like an inverse operation of [`decay_nodupe_tibble`]. It's similar to
## #' `dplyr_reconstruct` but it performs no validation on `data` and performs no
## #' validation or recalculations of `my_attr`.
## #'
## #' @keywords internal
## reclass_nodupe_tibble <- function(data, my_attr) {
##   reclassed <- if (inherits(data, "nodupe_tibble")) {
##     # (or should this move nodupe_tibble to the head of the class vector?)
##     data
##   } else {
##     `class<-`(x, c("nodupe_tibble", class(x)))
##   }
##   `attr<-`(reclassed, "my_attr", my_attr)
## }

#' Convert `x` to a `nodupe_tibble`
#'
#' @param my_attr dummy attr, just here to test how it moves around
#' @export
as_nodupe_tibble <- function(x, ...) UseMethod("as_nodupe_tibble")

#' @export
as_nodupe_tibble.nodupe_tibble <- function(x, ...) {
  x
}

#' @export
as_nodupe_tibble.tbl_df <- function(x, my_attr = 1, ...) {
  result <- new_nodupe_tibble(x, my_attr)
  checkmate::assert(check_df_nodupe_tibble_compatible(x, my_attr))
  result
}

# TODO proper pillar/vctrs interop instead of custom print

#' @export
print.nodupe_tibble <- function(x, ...) {
  cat("# Actually a nodupe_tibble\n")
  cat("# my_attr:", attr(x, "my_attr"), "\n")
  NextMethod()
}

#' @importFrom dplyr dplyr_row_slice
#' @export
dplyr_row_slice.nodupe_tibble <- function(data, i, ...) {
  if (is.numeric(i) && anyDuplicated(i) != 0L) {
    # We will have duplicates iff here; decay & re-dispatch. This should be more
    # efficient than maybe_decay_nodupe_tibble-ing.
    dplyr_row_slice(decay_nodupe_tibble(data), i, ...)
  } else {
    NextMethod()
  }
}

#' @importFrom dplyr dplyr_col_modify
#' @export
dplyr_col_modify.nodupe_tibble <- function(data, cols) {
  result <- NextMethod()
  if (any(vapply(cols, is.null, logical(1L)))) {
    # Removing cols may have introduced duplicates; re-verify:
    maybe_decay_nodupe_tibble(result)
  } else {
    result
  }
}

#' @importFrom dplyr dplyr_reconstruct
#' @export
dplyr_reconstruct.nodupe_tibble <- function(data, template) {
  maybe_decay_nodupe_tibble(NextMethod())
}

#' @export
`[.nodupe_tibble` <- function(x, i, j, ...) {
  maybe_decay_nodupe_tibble(NextMethod())
}

#' @export
`names<-.nodupe_tibble` <- function(x, value) {
  NextMethod()
}

# FIXME TODO groupedness interactions... do we need a reclass?

# XXX this approach to grouping, putting dedupe_tibble in front of grouped_df,
# seems suboptimal, but it's closest to what we have in epi_df currently and I
# don't want to mess around with this part of it now.

#' @importFrom dplyr group_by
#' @export
group_by.nodupe_tibble <- function(...) {
  new_nodupe_tibble(NextMethod())
}

#' @importFrom dplyr ungroup
#' @export
ungroup.nodupe_tibble <- function(...) {
  new_nodupe_tibble(NextMethod())
}
