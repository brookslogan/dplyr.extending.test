#' Low-level constructor for keyed_df4; use as_keyed_df4 or use carefully
#'
#' This does not validate that there are no duplicate rows in `df`; use this only
#' if you have already verified that. For a constructor/converter that validates
#' that for you, use `as_keyed_df4` instead.
#'
#' @param df data frame (possibly subclassed)
#' @param df_ukey_colnames chr
#'
#' @return a (nominal) keyed_df4
#'
#' @export
new_keyed_df4 <- function(df, ukey_colnames) {
  if (!inherits(df, "data.frame")) {
    # TODO go back to requiring tibble rather than df?
    cli::cli_abort("`df` must be a `data.frame`")
  }
  if (inherits(df, "keyed_df4")) {
    cli::cli_abort("`df` must not already be a keyed_df4")
  }
  # TODO is.character?
  if (!inherits(ukey_colnames, "character")) {
    cli::cli_abort("`ukey_colnames` must be a character vector")
  }
  # TODO subset verification
  class(df) <- c("keyed_df4", class(df))
  attr(df, "dplyr.extending.test::ukey_colnames") <- ukey_colnames
  # TODO rowwise
  df
}
# TODO check that not data.table?

#' Convert a nominal kdf4 (or subclass) into a not-kdf4
#'
#' @keywords internal
nominal_kdf4_decay <- function(nominal_kdf4) {
  if (!inherits(nominal_kdf4, "keyed_df4")) {
    cli::cli_abort("`nominal_kdf4` was not marked a keyed_df4 to begin with")
  }
  result <- nominal_kdf4
  attr(result, "dplyr.extending.test::ukey_colnames") <- NULL
  class(result) <- class(result)[class(result) != "keyed_df4"]
  result
}

df_ensure_not_kdf4 <- function(df) {
  if (inherits(df, "keyed_df4")) {
    nominal_kdf4_decay(df)
  } else {
    df
  }
}

#' @export
ukey_colnames.keyed_df4 <- function(x) {
  attr(x, "dplyr.extending.test::ukey_colnames")
}

kdf4_self <- function(x) {
  if (!inherits(nominal_kdf4, "keyed_df4")) {
    cli::cli_abort("`nominal_kdf4` was not marked a keyed_df4 to begin with")
  }
  result <- nominal_kdf4
  old_class <- class(result)
  # TODO note only nominally stripping subclasses; attrs may remain.
  class(result) <- old_class[match("keyed_df4", old_class):length(old_class)]
  result
}

# TODO any good helpers to re-tag with subclasses?

kdf4_super <- function(x) {
  old_class <- class(x)
  result <- nominal_kdf4_decay(x)
  # TODO note only nominally stripping subclasses; attrs may remain.
  class(result) <- old_class[(match("keyed_df4", old_class) + 1L):length(old_class)]
  result
}

#' Is / why isn't data.frame/subclass `x` compatible with keyed_df4 invariants
#'
#' @param x data.frame (possibly subclassed)
#' @return TRUE or str
df_check_kdf4_compatible <- function(x, ukey_colnames) {
  # TODO proper caller_arg passing
  if (!all(ukey_colnames %in% names(x))) {
    "didn't have one of the `ukey_colnames`"
  } else if (vctrs::vec_duplicate_any(kdf4_super(x)[ukey_colnames]) ||
               nrow(x) > 1L && length(ukey_colnames) == 0L) {
    "contained duplicate ukey values"
  } else {
    TRUE
  }
}

# TODO validate

df_ensure_structural_keyed_df4 <- function(df, ukey_colnames) {
  # TODO subset verification
  if (! "keyed_df4" %in% class(df)) {
    class(df) <- c("keyed_df4", class(df))
  }
  attr(df, "dplyr.extending.test::ukey_colnames") <- ukey_colnames
  # TODO rowwise
  df
}

df_as_keyed_df4_if_compatible <- function(df, ukey_colnames) {
  if (isTRUE(df_check_kdf4_compatible(df, ukey_colnames))) {
    df_ensure_structural_keyed_df4(df, ukey_colnames)
  } else {
    df
  }
}

#' @export
print.keyed_df4 <- function(x, ...) {
  # TODO pillar stuff, cli toString alternative
  print(glue::glue('# keyed_df4[{toString(ukey_colnames(x))}] of:\n'))
  NextMethod()
}

#' @export
`names<-.keyed_df4` <- function(x, value) {
  result <- NextMethod()
  old_names <- names(x)
  old_key_colnames <- ukey_colnames(x)
  new_key_colnames <- value[match(old_key_colnames, old_names)]
  df_ensure_structural_keyed_df4(x, new_key_colnames)
}

kdf4_extraction_restore_kdf4_if_possible <- function(extraction, original, i = NULL, j = NULL) {
  if (anyNA(i)) {
    return(df_ensure_not_kdf4(extraction))
  }
  if (is.numeric(i) && length(i) >= 1L && i[[1L]] >= 1L && (max(i) > nrow(original) || vctrs::vec_duplicate_any(i))) {
    # TODO or make > nrow a hard error? maybe through a non-restore interface (maybe using vec_slice)?
    return(df_ensure_not_kdf4(extraction))
  }
  if (is.null(j)) {
    return(df_ensure_structural_keyed_df4(extraction, ukey_colnames(original)))
  }
  if (!is.character(j)) {
    j <- names(original)[j]
  }
  dropped_ukey_colnames <- vctrs::vec_set_difference(ukey_colnames(original), j)
  if (length(dropped_ukey_colnames) == 0L) {
    return(df_ensure_structural_keyed_df4(extraction, ukey_colnames(original)))
  }
  if (is.null(i)) {
    dropped_ukey_col_values <- kdf4_super(original)[dropped_ukey_colnames]
  } else {
    dropped_ukey_col_values <- kdf4_super(original)[i, dropped_ukey_colnames]
  }
  # TODO vs. vctrs::vec_unique_count:
  if (vctrs::vec_size(dropped_ukey_col_values) != 0L &&
        !all(vctrs::vec_equal(dropped_ukey_col_values, dropped_ukey_col_values[1,]))) {
    # technically we could still have unique new-ukey values here,
    # but it seems like a violation anyway
    df_ensure_not_kdf4(extraction)
  } else {
    df_ensure_structural_keyed_df4(extraction, vctrs::vec_set_difference(ukey_colnames(original), dropped_ukey_colnames))
  }
}

#' @export
`[.keyed_df4` <- function(x, i, j, ..., drop = FALSE) {
  rlang::check_dots_empty0(...)

  result <- NextMethod()

  call_was_1d <- nargs() - rlang::dots_n(...) == 2L && !missing(i)
  if (call_was_1d) {
    # We were called along the lines of x[cols/lmat/imat] (or x[i =
    # cols/lmat/imat]); handle or standardize.
    if (is.matrix(i)) {
      # Logical or integer matrix indexing.  Output class is based on
      # element classes; do not adjust.
      return(result)
    } else {
      # Standardize.
      j <- i
      i <- NULL
    }
  } else {
    # We were called along the lines of the following: x[i,j],
    # x[i,], x[,j], x[,], x[j = j], or x[].
    if (missing(i)) i <- NULL
    if (missing(j)) j <- NULL
  }
  result <- kdf4_extraction_restore_kdf4_if_possible(result, x, i, j)
  if (drop && length(result) == 1L) {
    result <- result[[1L]]
  }
  result
}

# TODO seems like joins will each require a method impl that uses a
# basic(???) role-indicator-wrapper to preserve things for
# reconstruction.  Vs. just allow the reconstruct-based-on-first
# approach?  No, just have a method for each that performs the logic
# to determine the appropriate output key, and convert at the end.
# Don't want the based-on-first approach, so have to have these
# methods.  It's bind_* that has less controllable behavior.
# bind_rows may just be a less efficient vec_rbind once that's
# properly adjusted; or not: what ptype2 logic could say that we might
# decay depending on keys?  Perhaps something with attrs or stripping
# val cols that makes the (sorted?.......) ukey set part of the ptype,
# and ptype2 having to do combination logic... except incompatible
# with what we'd do with vec_cbind... so vec_*bind would have to be
# more demanding and degrade very quickly to tibble... except
# self-self needs to return self for other purposes, plus it's still a
# rbind vs cbind mismatch (drop vs. keep)... solution may be
# conditional hard errors.  bind_cols and vec_cbind might be true
# problem cases... and the ones that might actually require column
# wrappers ... except also they probably would need to be the
# complicated integrated wrappers (ukey_col_varprefix work).

# ptype2 logic... may want to consider decorator interchangeability
# and canonical ordering... but don't have to; can just require
# matching order


# TODO pivot functions

# TODO separate unit & time ukeys & aggregation mechanisms...
# * index_by approach?
# * auto-mark detectably-derived cols purely from unit or purely from time + group_by & .by doing something similar?
