

new_keyed_df4_selection <- function(selected_df, unselected_ukey_cols_df) {
  if (!inherits(selected_df, "data.frame")) {
    cli_abort("`selected_df` must be a data frame.")
  }
  if (inherits(selected_df, "keyed_df4_selection")) {
    cli_abort("`selected_df` is already a keyed_df4_selection; extra logic may be required.")
  }
  if (!inherits(unselected_ukey_cols_df, "data.frame")) {
    cli_abort("`unselected_ukey_cols_df` must be a data frame.")
  }
  if (nrow(selected_df) != nrow(unselected_ukey_cols_df)) {
    cli_abort("`selected_df` and `unselected_ukey_cols_df` must have matching `nrow`.")
  }
  class(selected_df) <- c("keyed_df4_selection", class(selected_df))
  attr(selected_df, "dplyr.extending.test:::unselected_ukey_cols_df") <- unselected_ukey_cols_df
  selected_df
}

# keyed_df4_selection <- function(kdf4, j) {
#   # TODO update `[` for kdf4 to drop its class if drops ukey col, not
#   # perform fancy checks, have those handled in this class.
#   selected_df <- kdf4[j]

#   # XXX or should this specifically be the kdf4 ukey colnames?
#   unselected_ukey_cols_df <- kdf4[vctrs::vec_set_difference(ukey_colnames(kdf4), names(selected_df))]

#   # XXX as helper method, should this only conditionally add the selection class if we need it?
#   new_keyed_df4_selection(selected_df, unselected_ukey_cols_df)
# }

nominal_kdf4s_decay <- function(nominal_kdf4s) {
  if (!inherits(nominal_kdf4, "keyed_df4_selection")) {
    cli::cli_abort("`nominal_kdf4s` was not marked a keyed_df4_selection to begin with")
  }
  result <- nominal_kdf4s
  attr(result, "dplyr.extending.test:::unselected_ukey_cols_df") <- NULL
  class(result) <- class(result)[class(result) != "keyed_df4_selection"]
  result
}

df_ensure_structural_keyed_df4_selection <- function(df, unselected_ukey_cols_df) {
  # TODO just call this from constructor?
  if (! "keyed_df4_selection" %in% class(df)) {
    class(df) <- c("keyed_df4_selection", class(df))
  }
  attr(df, "dplyr.extending.test:::unselected_ukey_cols_df") <- unselected_ukey_cols_df
  df
}

df_ensure_not_kdf4s <- function(df) {
  if (inherits(df, "keyed_df4")) {
    nominal_kdf4_decay(df)
  } else {
    df
  }
}

#' @export
print.keyed_df4_selection <- function(x, ...) {
  # TODO pillar stuff, cli toString alternative
  unselected_ukey_colnames <- names(attr(x, "dplyr.extending.test:::unselected_ukey_cols_df"))
  print(glue::glue('# keyed_df4_selection[without {toString(unselected_ukey_colnames)}] of:\n'))
  NextMethod()
}

#' @export
`names<-.keyed_df4_selection` <- function(x, value) {
  unselected_ukey_cols_df <- attr(x, "dplyr.extending.test:::unselected_ukey_cols_df")
  unselected_ukey_colnames <- names(unselected_ukey_cols_df)
  if (any(unselected_ukey_colnames %in% value)) {
    x <- df_ensure_not_kdf4s(x)
    NextMethod()
  } else {
    result <- NextMethod()
    old_names <- names(x)
    df_ensure_structural_keyed_df4_selection(x, unselected_ukey_cols_df)
  }
}

#' @export
`[.keyed_df4_selection` <- function(x, i, j, ..., drop = FALSE) {
  rlang::check_dots_empty0(...)

  x_unselected_ukey_cols_df <- attr(x, "dplyr.extending.test:::unselected_ukey_cols_df")

  # x_class <- class(x)
  # x_self_ind <- match("keyed_df4_selection", x_class)
  # class(x) <- class(x)[(x_self_ind+1):length(x_class)]
  # attr(x, "dplyr.extending.test:::unselected_ukey_cols_df") <- NULL
  # # ^ XXX or do we trust things to not redispatch without doing all
  # # but the attr stuff for us?

  result <- NextMethod()

  call_was_1d <- nargs() == 2L && !missing(i)
  if (call_was_1d) {
    # We were called along the lines of x[cols/lmat/imat] (or x[i =
    # cols/lmat/imat]); handle or standardize.
    if (is.matrix(i)) {
      # Logical or integer matrix indexing.  Output class is based on
      # element classes; do not adjust.
      return(result)
    } else {
      # `i` is col selection, not row selection
      result_unselected_ukey_cols_df <- x_unselected_ukey_cols_df
    }
  } else {
    # We were called along the lines of the following: x[i,j],
    # x[i,], x[,j], x[,], x[j = j], or x[].
    result_unselected_ukey_cols_df <- x_unselected_ukey_cols_df[i,]
  }
  if (inherits(result, "keyed_df4_selection")) {
    new_or_repeated_unselections <- attr(result, "dplyr.extending.test::unselected_ukey_cols_df")
    result_unselected_ukey_cols_df[names(new_or_repeated_unselections)] <- new_or_repeated_unselections
  }

  # TODO simplify away if possible
  result <- df_ensure_structural_keyed_df4_selection(result, result_unselected_ukey_cols_df)

  result
}
