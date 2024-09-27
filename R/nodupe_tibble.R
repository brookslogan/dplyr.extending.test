
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
  if (inherits(x, "nodupe_tibble")) {
    stop("x must not already be a nodupe_tibble")
  }
  if (!inherits(x, "data.frame")) {
    # XXX probably should make this tibble....
    stop("x must be a data.frame")
  }
  class(x) <- c("nodupe_tibble", class(x))
  attr(x, "my_attr") <- my_attr
  x
}

#' Ensure nodupe_tibble is head class & attrs as given
#'
#' @keywords internal
ensure_new_nodupe_tibble <- function(x, my_attr = 1) {
  class_x <- class(x)
  class(x) <- class_x[class_x != "nodupe_tibble"]
  new_nodupe_tibble(x, my_attr)
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
    x
  } else {
    decay_nodupe_tibble(x)
  }
}

#' Ensure nodupe is head class & attrs set, if compatible with nodupe
#'
#' @keywords internal
maybe_new_nodupe_tibble <- function(x, my_attr = 1) {
  if (is.data.frame(x)) {
    maybe_decay_nodupe_tibble(ensure_new_nodupe_tibble(x, my_attr))
  } else {
    x
  }
}

#' maybe_decay_nodupe_tibble if we know x is.data.frame
#'
#' @keywords internal
maybe_new_nodupe_tibble0 <- function(x, my_attr = 1) {
  maybe_decay_nodupe_tibble(ensure_new_nodupe_tibble(x, my_attr))
}


# #' Attach nodupe_tibble class & attrs from template without checking validity
# #'
# #' This is like an inverse operation of [`decay_nodupe_tibble`]. It's similar to
# #' `dplyr_reconstruct` but it performs no validation on `data` and performs no
# #' validation or recalculations of `my_attr`.
# #'
# #' @keywords internal
# reclass_nodupe_tibble <- function(data, my_attr) {
#   reclassed <- if (inherits(data, "nodupe_tibble")) {
#     # (or should this move nodupe_tibble to the head of the class vector?)
#     data
#   } else {
#     `class<-`(x, c("nodupe_tibble", class(x)))
#   }
#   `attr<-`(reclassed, "my_attr", my_attr)
# }

#' Convert `x` to a `nodupe_tibble`
#'
#' @param my_attr dummy attr, just here to test how it moves around
#' @export
as_nodupe_tibble <- function(x, my_attr = 1, ...) UseMethod("as_nodupe_tibble")

#' @export
as_nodupe_tibble.nodupe_tibble <- function(x, my_attr = 1, ...) {
  attr(x, "my_attr") <- my_attr
  # If my_attr actually influenced duplicate check, we might need to re-validate
  # here if the my_attr actually changed.
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

# #' @importFrom dplyr dplyr_row_slice
# #' @export
# dplyr_row_slice.nodupe_tibble <- function(data, i, ...) {
#   if (is.numeric(i) && anyDuplicated(i) != 0L) {
#     # We will have duplicates iff here; decay & re-dispatch. This should be more
#     # efficient than maybe_decay_nodupe_tibble-ing.
#     dplyr_row_slice(decay_nodupe_tibble(data), i, ...)
#   } else {
#     NextMethod()
#   }
# }

#' @importFrom dplyr dplyr_row_slice
#' @export
dplyr_row_slice.nodupe_tibble <- function(data, i, ...) {
  data[i,]
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
  res <- NextMethod()
  # # res <- new_nodupe_tibble(res, attr(template, "my_attr"))
  # res <- maybe_decay_nodupe_tibble(res)
  #
  # FIXME it's unclear that just using template attrs is right. E.g. we may have
  # additional key columns added and should preserve them...
  maybe_new_my_attr <- attr(template, "my_attr")
  res <- maybe_new_nodupe_tibble0(res, maybe_new_my_attr)
  res
}

# nodupe_tibble_col_select <- function(x, iloc) {
#   # decay so we can access
#   res <- decay_nodupe_tibble(x)[iloc]
#   # columns may have been removed; 
#   res <- new_nodupe_tibble(res, attr(x, "my_attr"))
#   res <- maybe_decay_nodupe_tibble(res)
# }

#' @export
# `[.nodupe_tibble` <- function(x, i, j, ...) {
#   # Row selections are performed with x[i,j] and x[i,] but not x[i]; counting
#   # args provides a way of detecting this properly while `missing` can't
#   # distinguish between the 2nd and 3rd cases. (Counting args includes blank
#   # args.)
#   i_is_optional_row_selection <- nargs() - rlang::dots_n(...) == 3L
#   if (i_is_optional_row_selection) {
#     # XXX probably should do col selection first if possible, as this seems more
#     # efficient indexing-wise; however, not sure if that become more complicated
#     # dupe-checking wise
#     #
#     # `i` is an optional row selection; for our purposes we can pretend like
#     # this happens before the col selection rather than simultaneously.
#     #
#     # dplyr_row_slice will take care of typechecking on i for the types we will
#     # support
#     #
#     if (missing(i)) {
#       # Re-dispatch rather than NextMethod() as it's not clear how to alter
#       # nargs():
#       #
#       # FIXME properly delegate ...
#       x[j]
#     } else {
#       # Re-dispatch rather than NextMethod() due to the above reason + because
#       # row slice might have a different class:
#       dplyr_row_slice(x, i)[j]

#       # TODO consider a bare col select that tries to correctly set attribute
#       # but does not post-decay? Or dispatch row slice etc. to `[` as the core
#       # impl?  Don't check i if already have to revalidate based on j?
#     }
#   } else {
#     # We're doing column selection or everything-selection.
#     res <- NextMethod()
#     # FIXME drop = TRUE not being forwarded?
#     #
#     # We might have selected away columns, and that might have introduced
#     # duplicates. And NextMethod() might have dropped our class. Reclass &
#     # revalidate:
#     #
#     # TODO consider a `maybe_as_nodupe_tibble` method
#     #
#     # TODO consider optimizations
#     if (is.data.frame(res)) {
#       maybe_new_my_attr <- attr(x, "my_attr")
#       res <- maybe_new_nodupe_tibble(res, maybe_new_my_attr)
#     }
#     res
#   }
# }

# Alternative approach that could be used to back some dplyr_ stuff:

#' @export
`[.nodupe_tibble` <- function(x, i, j, ..., drop = FALSE) {
  i_is_special <- nargs() - rlang::dots_n(...) == 2L && !missing(i)
  if (i_is_special) {
    # We were called along the lines of x[cols/lmat/imat] (or x[i =
    # cols/lmat/imat]); handle or standardize.
    if (is.matrix(i)) {
      maybe_new_my_attr <- attr(x, "my_attr")
      return(maybe_new_nodupe_tibble(NextMethod(), maybe_new_my_attr))
    } else {
      # Standardize; NextMethod() appears to have issues when we change
      # missingness patterns, so re-dispatch:
      return(x[, i, ..., drop = drop])
    }
  }

  # If we reached here, we were called along the lines of the following: x[i,j],
  # x[i,], x[,j], x[,], x[j = j], or x[].

  # XXX perf: we could also standardize away no-op row and/or col selections to
  # try to improve performance, though identifying those could be complex and
  # might actually be slower.
  if (missing(i)) {
    if (missing(j)) {
      # i missing, j missing:
      #
      # "Everything" selection; let's assume no (data.table-like) special
      # behavior of "subclasses" in this case, and skip delegation.
      return(x)
    } else {
      # i missing, j present:
      #
      # Col selection might mess up nodupe invariant or yield non-data.frame, so
      # validate.
      maybe_new_my_attr <- attr(x, "my_attr")
      return(maybe_new_nodupe_tibble(NextMethod(), maybe_new_my_attr))
    }
  } else {
    if (missing(j)) {
      # i present, j missing:
      if (is.numeric(i) && anyDuplicated(i) != 0L) {
        # We will have duplicates; decay & re-dispatch. This should be more
        # efficient than maybe_new_nodupe_tibble-ing the result.
        return(decay_nodupe_tibble(data)[i, j, ..., drop = drop])
      } else {
        # FIXME ensure char row indices not allowed
        #
        # We shouldn't have duplicates, just enforce right class&attr:
        new_my_attr <- attr(x, "my_attr")
        return(ensure_new_nodupe_tibble(NextMethod(), new_my_attr))
      }
    } else {
      # i present, j present:
      #
      # Col selection might mess up nodupe invariant or yield non-data.frame, so
      # validate. Since we're already validating, no need to check for integer i
      # duplications.
      maybe_new_my_attr <- attr(x, "my_attr")
      return(maybe_new_nodupe_tibble(NextMethod(), maybe_new_my_attr))
    }
    # FIXME refactor
    res
  }
}

#' @export
`names<-.nodupe_tibble` <- function(x, value) {
  res <- NextMethod()
  if (!inherits(res, "nodupe_tibble")) {
    res <- new_nodupe_tibble(res, attr(x, "my_attr"))
  }
}

# XXX this approach to grouping, putting nodupe_tibble in front of grouped_df,
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
