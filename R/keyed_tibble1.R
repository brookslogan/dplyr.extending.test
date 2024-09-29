#' Low-level constructor for keyed_tibble1; use as_keyed_tibble1 or use carefully
#'
#' This does not validate that there are no duplicate rows in `x`; use this only
#' if you have already verified that. For a constructor/converter that validates
#' that for you, use `as_keyed_tibble1` instead.
#'
#' @param key_colnames chr
#'
#' @export
new_keyed_tibble1 <- function(x, key_colnames) {
  if (inherits(x, "keyed_tibble1")) {
    stop("x must not already be a keyed_tibble1")
  }
  if (!tibble::is_tibble(x)) {
    stop("x must be a tibble")
  }
  if (!inherits(key_colnames, "character")) {
    stop("key_colnames must be character vector")
  }
  class(x) <- c("keyed_tibble1", class(x))
  attr(x, "dplyr.extending.test::key_colnames") <- key_colnames
  x
}

#' Ensure keyed_tibble1 is head class & attrs as given
#'
#' @keywords internal
ensure_new_keyed_tibble1 <- function(x, key_colnames) {
  class_x <- class(x)
  class(x) <- class_x[class_x != "keyed_tibble1"]
  new_keyed_tibble1(x, key_colnames)
}

#' Is / why isn't data.frame/subclass `x` compatible with keyed_tibble1 invariants
#'
#' @param x data.frame or subclass
#' @return TRUE or str
check_df_keyed_tibble1_compatible <- function(x, key_colnames) {
  # TODO proper caller_arg passing
  if (!all(key_colnames %in% names(x))) {
    "didn't have one of the `key_colnames`"
  } else if (anyDuplicated(ensure_decayed_keyed_tibble1(x)[key_colnames]) != 0L ||
               nrow(x) > 1L && length(key_colnames) == 0L) {
    "contained duplicates"
  } else {
    TRUE
  }
}

#' Is / isn't data.frame/subclass `x` compatible with keyed_tibble1 invariants
#'
#' @param x data.frame or subclass
#' @return TRUE or FALSE
test_df_keyed_tibble1_compatible <- function(x, key_colnames) {
  # TODO perf: if using better check_* feedback, might want to optimize this
  isTRUE(check_df_keyed_tibble1_compatible(x, key_colnames))
}

#' @export
validate_keyed_tibble1 <- function(x) {
  # TODO proper caller_arg passing
  checkmate::assert(check_df_keyed_tibble1_compatible(x, attr(x, "dplyr.extending.test::key_colnames")))
}

#' Remove keyed_tibble1 class & attrs if present:
#'
#' @keywords internal
ensure_decayed_keyed_tibble1 <- function(x, ...) {
  oldclass <- class(x)
  class(x) <- oldclass[oldclass != "keyed_tibble1"]
  attr(x, "dplyr.extending.test::key_colnames") <- NULL
  x
}

#' Validate whether x obeys invariants of keyed_tibble1; if not, decay to non-keyed_tibble1:
#'
#' @keywords internal
maybe_ensure_decayed_keyed_tibble1 <- function(x) {
  if (test_df_keyed_tibble1_compatible(x, attr(x, "dplyr.extending.test::key_colnames"))) {
    x
  } else {
    ensure_decayed_keyed_tibble1(x)
  }
}

#' Ensure nodupe is head class & attrs set, if compatible with nodupe
#'
#' @keywords internal
maybe_new_keyed_tibble1 <- function(x, key_colnames) {
  if (is.data.frame(x)) {
    maybe_ensure_decayed_keyed_tibble1(ensure_new_keyed_tibble1(x, key_colnames))
  } else {
    x
  }
}

#' maybe_ensure_decayed_keyed_tibble1 if we know x is.data.frame
#'
#' @keywords internal
maybe_new_keyed_tibble1_0 <- function(x, key_colnames) {
  maybe_ensure_decayed_keyed_tibble1(ensure_new_keyed_tibble1(x, key_colnames))
}


# #' Attach keyed_tibble1 class & attrs from template without checking validity
# #'
# #' This is like an inverse operation of [`ensure_decayed_keyed_tibble1`]. It's similar to
# #' `dplyr_reconstruct` but it performs no validation on `data` and performs no
# #' validation or recalculations of `key_colnames`.
# #'
# #' @keywords internal
# reclass_keyed_tibble1 <- function(data, key_colnames) {
#   reclassed <- if (inherits(data, "keyed_tibble1")) {
#     # (or should this move keyed_tibble1 to the head of the class vector?)
#     data
#   } else {
#     `class<-`(x, c("keyed_tibble1", class(x)))
#   }
#   `attr<-`(reclassed, "dplyr.extending.test::key_colnames", key_colnames)
# }

#' Convert `x` to a `keyed_tibble1`
#'
#' @param key_colnames dummy attr, just here to test how it moves around
#' @export
as_keyed_tibble1 <- function(x, key_colnames = 1, ...) UseMethod("as_keyed_tibble1")

#' @export
as_keyed_tibble1.keyed_tibble1 <- function(x, key_colnames = 1, ...) {
  attr(x, "dplyr.extending.test::key_colnames") <- key_colnames
  # If key_colnames actually influenced duplicate check, we might need to re-validate
  # here if the key_colnames actually changed.
  x
}

#' @export
as_keyed_tibble1.tbl_df <- function(x, key_colnames = 1, ...) {
  result <- new_keyed_tibble1(x, key_colnames)
  checkmate::assert(check_df_keyed_tibble1_compatible(x, key_colnames))
  result
}

# TODO proper pillar/vctrs interop instead of custom print

#' @export
print.keyed_tibble1 <- function(x, ...) {
  cat("# Actually a keyed_tibble1\n")
  cat("# key_colnames:", attr(x, "dplyr.extending.test::key_colnames"), "\n")
  NextMethod()
}

# #' @importFrom dplyr dplyr_row_slice
# #' @export
# dplyr_row_slice.keyed_tibble1 <- function(data, i, ...) {
#   if (is.numeric(i) && anyDuplicated(i) != 0L) {
#     # We will have duplicates iff here; decay & re-dispatch. This should be more
#     # efficient than maybe_ensure_decayed_keyed_tibble1-ing.
#     dplyr_row_slice(ensure_decayed_keyed_tibble1(data), i, ...)
#   } else {
#     NextMethod()
#   }
# }

#' @importFrom dplyr dplyr_row_slice
#' @export
dplyr_row_slice.keyed_tibble1 <- function(data, i, ...) {
  data[i,]
}

#' @importFrom dplyr dplyr_col_modify
#' @export
dplyr_col_modify.keyed_tibble1 <- function(data, cols) {
  result <- NextMethod()
  if (any(vapply(cols, is.null, logical(1L)))) {
    # Removing cols may have introduced duplicates; re-verify:
    maybe_ensure_decayed_keyed_tibble1(result)
  } else {
    result
  }
}

#' @importFrom dplyr dplyr_reconstruct
#' @export
dplyr_reconstruct.keyed_tibble1 <- function(data, template) {
  res <- NextMethod()
  # # res <- new_keyed_tibble1(res, attr(template, "dplyr.extending.test::key_colnames"))
  # res <- maybe_ensure_decayed_keyed_tibble1(res)
  #
  # FIXME it's unclear that just using template attrs is right. E.g. we may have
  # additional key columns added and should preserve them...
  maybe_new_key_colnames <- attr(template, "dplyr.extending.test::key_colnames")
  res <- maybe_new_keyed_tibble1_0(res, maybe_new_key_colnames)
  res
}

#' @export
`[.keyed_tibble1` <- function(x, i, j, ..., drop = FALSE) {
  i_is_special <- nargs() - rlang::dots_n(...) == 2L && !missing(i)
  if (i_is_special) {
    # We were called along the lines of x[cols/lmat/imat] (or x[i =
    # cols/lmat/imat]); handle or standardize.
    if (is.matrix(i)) {
      # Logical or integer matrix indexing:
      result <- NextMethod()
      if (is.data.frame(result)) {
        stop("internal error: expected matrix indexing to yield a non-data.frame")
      }
      return(result)
    } else {
      # Standardize. NextMethod() appears to have issues when we change
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
      old_key_colnames <- attr(x, "dplyr.extending.test::key_colnames")
      result <- NextMethod()
      maybe_new_key_colnames <- old_key_colnames[old_key_colnames %in% names(result)]
      result <- maybe_new_keyed_tibble1(result, maybe_new_key_colnames)
      return(result)
    }
  } else {
    if (missing(j)) {
      # i present, j missing:
      if (is.numeric(i) && anyDuplicated(i) != 0L) {
        # We will have duplicates; decay & re-dispatch. This should be more
        # efficient than maybe_new_keyed_tibble1-ing the result.
        return(ensure_decayed_keyed_tibble1(x)[i, j, ..., drop = drop])
      } else if (is.character(i)) {
        stop("character row indexing not allowed")
      } else {
        # We shouldn't have duplicates, just enforce right class&attr:
        new_key_colnames <- attr(x, "dplyr.extending.test::key_colnames")
        return(ensure_new_keyed_tibble1(NextMethod(), new_key_colnames))
      }
    } else {
      # i present, j present:
      #
      # Col selection might mess up nodupe invariant or yield non-data.frame, so
      # validate. Since we're already validating, no need to check for integer i
      # duplications.
      old_key_colnames <- attr(x, "dplyr.extending.test::key_colnames")
      result <- NextMethod()
      maybe_new_key_colnames <- old_key_colnames[old_key_colnames %in% names(result)]
      result <- maybe_new_keyed_tibble1(result, maybe_new_key_colnames)
      return(result)
    }
    res
  }
}

#' @export
`names<-.keyed_tibble1` <- function(x, value) {
  result <- NextMethod()
  old_names <- names(x)
  old_key_colnames <- attr(x, "dplyr.extending.test::key_colnames")
  new_key_colnames <- value[match(old_key_colnames, old_names)]
  ensure_new_keyed_tibble1(x, new_key_colnames)
}

# XXX this approach to grouping, putting keyed_tibble1 in front of grouped_df,
# seems suboptimal, but it's closest to what we have in epi_df currently and I
# don't want to mess around with this part of it now.

#' @importFrom dplyr group_by
#' @export
group_by.keyed_tibble1 <- function(.data, ...) {
  new_keyed_tibble1(NextMethod(), attr(.data, "dplyr.extending.test::key_colnames"))
}

#' @importFrom dplyr ungroup
#' @export
ungroup.keyed_tibble1 <- function(x, ...) {
  new_keyed_tibble1(NextMethod(), attr(x, "dplyr.extending.test::key_colnames"))
}

# TODO rowwise?
