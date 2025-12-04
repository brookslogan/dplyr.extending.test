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
`names<-.keyed_df4` <- function(x, value) {
  result <- NextMethod()
  old_names <- names(x)
  old_key_colnames <- ukey_colnames(x)
  new_key_colnames <- value[match(old_key_colnames, old_names)]
  df_ensure_structural_keyed_df4(x, new_key_colnames)
}

kdf4_extraction_restore_kdf4_if_possible <- function(extraction, original, i = NULL, j = NULL) {
  if (i has duplicates) {
    no
  }
  # if (j drops ukey cols) {
  if (i not all same dropped ukey col vals) {
    no
  } else {
    yes, with altered ukey cols
  }
  # } else {
  #   yes
  # }
}

#' @export
`[.keyed_df2` <- function(x, i, j, ..., drop = FALSE) {
  rlang::check_dots_empty0(...)

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
      # Standardize.
      j <- i
      i <- NULL
    }
  } else {
    # We were called along the lines of the following: x[i,j],
    # x[i,], x[,j], x[,], x[j = j], or x[].
    if (missing(i)) i <- NULL
    if (missing(j)) j <- NULL
    # TODO vs. forward to helper method?
  }

  if (is.null(j)) {
    dropped_ukey_colnames <- character()
  } else {
    # ... don't want to have to double-munge j format to figure out dropped_ukey_colnames... try to use delegate+restore approach? though it will likely have more trouble with drop = TRUE... unless delegate with drop = FALSE.
  }

  # TODO deal with selection

  # TODO deal with drop = TRUE
}
