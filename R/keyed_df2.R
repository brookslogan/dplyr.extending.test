#' Low-level constructor for keyed_df2; use as_keyed_df2 or use carefully
#'
#' This does not validate that there are no duplicate rows in `df`; use this only
#' if you have already verified that. For a constructor/converter that validates
#' that for you, use `as_keyed_df2` instead.
#'
#' @param df data frame (possibly subclassed)
#' @param df_ukey_colnames chr
#'
#' @return a (nominal) keyed_df2
#'
#' @export
new_keyed_df2 <- function(df, df_ukey_colnames) {
  if (inherits(df, "keyed_df2")) {
    cli::cli_abort("`df` must not already be a keyed_df2")
  }
  if (!tibble::is_tibble(df)) {
    cli::cli_abort("`df` must be a tibble")
  }
  if (!inherits(df_ukey_colnames, "character")) {
    cli::cli_abort("df_ukey_colnames must be character vector")
  }
  class(df) <- c("keyed_df2", class(df))
  attr(df, "dplyr.extending.test::ukey_colnames") <- df_ukey_colnames
  if (dplyr::is_grouped_df(df)) {
    attr(df, "groups") <- new_keyed_df2(attr(df, "groups"), dplyr::group_vars(df))
  }
  # TODO rowwise
  df
}

#' Convert a nominal kdf2 (or subclass) into a not-kdf2
#'
#' @keywords internal
nominal_kdf2_decay <- function(nominal_kdf2) {
  if (!inherits(nominal_kdf2, "keyed_df2")) {
    cli::cli_abort("`nominal_kdf2` was not marked a keyed_df2 to begin with")
  }
  result <- nominal_kdf2
  if (is_grouped_df(result)) {
    attr(result, "groups") <- nominal_kdf2_decay(attr(result, "groups"))
  }
  attr(result, "dplyr.extending.test::ukey_colnames") <- NULL
  class(result) <- class(result)[class(result) != "keyed_df2"]
  result
}

#' Is / why isn't data.frame/subclass `x` compatible with keyed_df2 invariants
#'
#' @param x data.frame (possibly subclassed)
#' @return TRUE or str
df_check_kdf2_compatible <- function(x, ukey_colnames) {
  # TODO proper caller_arg passing
  if (!all(ukey_colnames %in% names(x))) {
    "didn't have one of the `ukey_colnames`"
  } else if (vctrs::vec_duplicate_any(nominal_kdf2_decay(x)[ukey_colnames]) != 0L ||
               nrow(x) > 1L && length(ukey_colnames) == 0L) {
    "contained duplicates"
  } else {
    TRUE
  }
}

#' @export
is_keyed_df2 <- function(obj) {
  inherits(obj, "keyed_df2")
}

#' @export
validate_keyed_df2 <- function(nominal_kdf2) {
  if (!inherits(nominal_kdf2, "keyed_df2")) {
    cli::cli_abort("`nominal_kdf2` was not marked a keyed_df2 to begin with")
  }
  nominal_kdf2_class <- class(nominal_kdf2_class)
  if (sum(nominal_kdf2_class  == "keyed_df2") > 1L) {
    cli::cli_abort('`class(nominal_kdf2)` contained multiple appearances of "keyed_df2"')
  }
  if (is_grouped_df(nominal_kdf2)) {
    if (vctrs::vec_match("keyed_df2", nominal_kdf2_class) > vctrs::vec_match("grouped_df", nominal_kdf2_class)) {
      cli::cli_abort('`class(nominal_kdf2)` had "keyed_df2" appear after "grouped_df"')
    }
    groups <- attr(nominal_kdf2, "groups")
    if (!is_keyed_df2(groups)) {
      cli::cli_abort("nominal_kdf2 was grouped, but its groups were not a (nominal) kdf2")
    }
    validate_keyed_df2(groups) # TODO caller_arg passing really matters here.  though this check seems unlikely to fail
  }
  # TODO rowwise
  #
  # TODO proper caller_arg passing
  checkmate::assert(df_check_kdf2_compatible(x, attr(x, "dplyr.extending.test::ukey_colnames")))
}

# #' Convert a nominal kdf2 (or subclass) into its parent class
# #'
# #' @keywords internal
# nominal_kdf2_parent <- function(nominal_kdf2) {
#   if (!inherits(nominal_kdf2, "keyed_df2")) {
#     cli::cli_abort("`nominal_kdf2` was not marked a keyed_df2 to begin with")
#   }
#   result <- nominal_kdf2
#   if (is_grouped_df(result)) {
#     attr(result, "groups") <- nominal_kdf2_decay(attr(result, "groups"))
#   }
#   attr(result, "dplyr.extending.test::ukey_colnames") <- NULL
#   old_class <- class(result)
#   class(result) <- old_class[(match("keyed_df2", old_class) + 1L):length(old_class)]
#   result
# }

#' Convert a nominal kdf2 (or subclass) so kdf2 is the head class
#'
#' A crucial tool for subclass-friendly S3 method implementation.  Our
#' S3 method implementations should either:
#'
#' * Never call an S3 method on the S3 dispatch argument; possibly call `NextMethod()`.
#'
#' * Strip subclasses of the S3 argument before calling any S3 methods on them; never call `NextMethod()`.
#'
#' Used to prevent invalid results, hard errors, and redundant
#' processing from re-dispatching to subclasses when calling S3
#' methods from within an S3 method implementation. (We want
#' SML/Haskell/Rust-style parametric polymorphism / parent dependency
#' injection, not Java/Python-style virtual/dynamic dispatch.)
#'
#' ```
#' specific_op1.keyed_df2 <- function(x, args1) {
#'   x <- nominal_kdf2_strip_subclasses(x)
#'   g(x, transform_args1(args1))
#' }
#' specific_op2.keyed_df2 <- function(x, args2) {
#'   x <- nominal_kdf2_strip_subclasses(x)
#'   g(x, transform_args2(args2))
#' }
#' general_op.keyed_df2 <- function(x, g_args) {
#'   postprocess(NextMethod())
#' }
#' ```
#'
#' @keywords internal
nominal_kdf2_strip_subclasses <- function(nominal_kdf2) {
  if (!inherits(nominal_kdf2, "keyed_df2")) {
    cli::cli_abort("`nominal_kdf2` was not marked a keyed_df2 to begin with")
  }
  result <- nominal_kdf2
  old_class <- class(result)
  class(result) <- old_class[match("keyed_df2", old_class):length(old_class)]
  result
}

# TODO other converters, helpers


#' @export
dplyr_row_slice.keyed_df2 <- function(data, i, ...) {
  # if (vctrs::vec_duplicate_any(i)) {
  #   dplyr_row_slice(nominal_kdf2_decay(data), i, ...)
  # } else {
  #   new_keyed_df2(NextMethod(), attr(data, "dplyr.extending.test::ukey_colnames"))
  # }
  nominal_kdf2_strip_subclasses(data)[i,]
}

#' @export
dplyr_col_modify.keyed_df2 <- function(data, cols) {
  # result <- NextMethod()
  # data_key_colnames <- attr(data, "dplyr.extending.test::ukey_colnames")
  # if (any(names(cols) %in% data_key_colnames)) {
  #   result_key_colnames <- vctrs::vec_set_intersect(data_key_colnames, names(result))
  #   result <- maybe_new_keyed_tibble1_0(result, result_key_colnames)
  #   # FIXME if we end up a unkeyed grouped df, we need to make sure
  #   # that groups attr is de-keyed; awkward in current scheme because
  #   # we have a sort of half-keyed NextMethod result.
  # } else {
  #   result <- new_keyed_df2(result, data_key_colnames)
  # }
  # result
  data <- nominal_kdf2_strip_subclasses(data)
  data[names(cols)] <- cols
  data
}

#' @export
dplyr_reconstruct.keyed_df2 <- function(data, template) {
  stop("TODO")
}

#' @export
`names<-.keyed_df2` <- function(x, value) {
  stop("TODO")
}

#' @export
`[.keyed_df2` <- function(x, i, j, ..., drop = FALSE) {
  call_was_1d <- nargs() - rlang::dots_n(...) == 2L && !missing(i)
  if (call_was_1d) {
    # We were called along the lines of x[cols/lmat/imat] (or x[i =
    # cols/lmat/imat]); handle or standardize.
    if (is.matrix(i)) {
      # Logical or integer matrix indexing.  Output class is based on
      # element classes; do not adjust.
      result <- NextMethod()
      return(result)
    } else {
      # Standardize. NextMethod() appears to have issues when we change
      # missingness patterns, so re-dispatch:
      return(nominal_kdf2_strip_subclasses(x)[, i, ..., drop = drop])
    }
  }

  # If we reached here, we were called along the lines of the following: x[i,j],
  # x[i,], x[,j], x[,], x[j = j], or x[].

  # XXX perf: we could also standardize away no-op row and/or col selections to
  # try to improve performance, though identifying those could be complex and
  # might actually be slower.
  if (missing(i)) {
    if (missing(j)) {
      # i missing, j missing ("everything" selection):
      #
      # Still delegate in case parent class assigns special behaviors:
      old_key_colnames <- attr(x, "dplyr.extending.test::ukey_colnames")
      # TODO refactor to common if possible?:
      parent_result <- NextMethod()
      result <- new_keyed_df2(parent_result, old_key_colnames)
      # TODO refactor to common or...
      return(result)
    } else {
      # i missing, j present:
      #
      # Col selection might mess up nodupe invariant or yield non-data.frame, so
      # validate.
      old_key_colnames <- attr(x, "dplyr.extending.test::ukey_colnames")
      parent_result <- NextMethod()
      # TODO refactor to helper?:
      if (!is.data.frame(parent_result)) {
        result <- parent_result
      } else {
        maybe_new_key_colnames <- old_key_colnames[old_key_colnames %in% names(result)]
        # FIXME TODO conditional validation here, plus finish rewriting the rest.
        result <- maybe_new_keyed_tibble2(result, maybe_new_key_colnames)
      }
      return(result)
    }
  } else {
    if (missing(j)) {
      # i present, j missing:
      if (is.numeric(i) && anyDuplicated(i) != 0L) {
        # We will have duplicates; decay & re-dispatch. This should be more
        # efficient than maybe_new_keyed_df2-ing the result.
        return(ensure_decayed_keyed_df2(x)[i, j, ..., drop = drop])
      } else if (is.character(i)) {
        stop("character row indexing not allowed")
      } else {
        # We shouldn't have duplicates, just enforce right class&attr:
        new_key_colnames <- attr(x, "dplyr.extending.test::ukey_colnames")
        return(ensure_new_keyed_df2(NextMethod(), new_key_colnames))
      }
    } else {
      # i present, j present:
      #
      # Col selection might mess up nodupe invariant or yield non-data.frame, so
      # validate. Since we're already validating, no need to check for integer i
      # duplications.
      old_key_colnames <- attr(x, "dplyr.extending.test::ukey_colnames")
      result <- NextMethod()
      maybe_new_key_colnames <- old_key_colnames[old_key_colnames %in% names(result)]
      result <- maybe_new_keyed_df2(result, maybe_new_key_colnames)
      return(result)
    }
  }
}

#' @export
`[<-.keyed_df2` <- function(x, i, j, ..., drop = FALSE) {
  stop("TODO")
}

# TODO other base methods


# kdf2_decay
# kdf2_decay_if_bad
# df_decay_if_kdf2
# df_decay_if_bad_kdf2
# obj_decay_if_bad_kdf2

# VS

# kdf2_decay
# kdf2_if_bad_decay
# df_if_kdf2_decay
# df_if_bad_kdf2_decay
# obj_if_bad_kdf2_decay

# VS

# kdf2_decay
# kdf2_or_bad_kdf2_if_bad_kdf2_decay # or nominal_kdf2_if_bad_kdf2_decay
# df_if_kdf2_or_bad_kdf2_decay
# df_if_bad_kdf2_decay
# obj_if_bad_kdf2_decay

# (decay vs. to_{not,non}_[nominal_]kdf2)

# VS

# ...




#' @importFrom dplyr dplyr_row_slice
#' @export
dplyr_row_slice.keyed_df2 <- function(data, i, ...) {
  data[i,]
}


#' @export
print.keyed_df2 <- function(x, ...) {
  # TODO pillar stuff, cli toString alternative
  print(glue::glue('# keyed_df2[{toString(attr(x, "dplyr.extending.test::ukey_colnames"))}] of:\n'))
  NextMethod()
}

#' @export
group_by.keyed_df2 <- function(.data, ...) {
  new_keyed_df2(NextMethod(), attr(.data, "dplyr.extending.test::ukey_colnames"))
}
