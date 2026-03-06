#' Low-level constructor for keyed_df5; use as_keyed_df5 or use carefully
#'
#' This does not validate that there are no duplicate rows in `df`; use this only
#' if you have already verified that. For a constructor/converter that validates
#' that for you, use `as_keyed_df5` instead.
#'
#' @param df data frame (possibly subclassed)
#' @param df_ukey_colnames chr
#'
#' @return a (nominal) keyed_df5
#'
#' @export
new_keyed_df5 <- function(df, full_ukey_colnames, unselected_ukey_cols_df, context_row) {
  checkmate::assert(check_structural_keyed_df5(df, full_ukey_colnames, unselected_ukey_cols_df, context_row))
  new_key_keyed_df5_0(df, full_ukey_colnames, unselected_ukey_cols_df, context_row)
}

new_key_keyed_df5_0 <- function(df, full_ukey_colnames, unselected_ukey_cols_df, context_row) {
  class(df) <- c("keyed_df5", class(df))
  attr(df, "dplyr.extending.test::full_ukey_colnames") <- full_ukey_colnames
  attr(df, "dplyr.extending.test::unselected_ukey_cols_df") <- unselected_ukey_cols_df
  attr(df, "dplyr.extending.test::context_row") <- context_row
  df
}

check_structural_keyed_df5 <- function(df, full_ukey_colnames, unselected_ukey_cols_df, context_row) {
  if (!inherits(df, "data.frame")) {
    # TODO go back to requiring tibble rather than df?
    #
    # XXX or try to allow even more? requires more than just
    # dplyr_extending, but having to do significant work beyond anyway
    return("`df` must be a `data.frame`")
  }
  if (inherits(df, "keyed_df5")) {
    return("`df` must not already be a keyed_df5")
  }
  # TODO is.character?
  if (!inherits(full_ukey_colnames, "character")) {
    return("`full_ukey_colnames` must be a character vector")
  }
  if (!inherits(unselected_ukey_cols_df, "data.frame")) {
    return("`unselected_ukey_cols_df` must be a data.frame")
  }
  if (nrow(unselected_ukey_cols_df) != nrow(df)) {
    return("`unselected_ukey_cols_df` must have the same `nrow` as `df`")
  }
  if (!all(names(unselected_ukey_cols_df) %in% full_ukey_colnames)) {
    return("`unselected_ukey_cols_df` must have all its colnames included in `full_ukey_colnames`")
  }
  if (!all(full_ukey_colnames %in% names(df) | full_ukey_colnames %in% names(unselected_ukey_cols_df))) {
    return("`full_ukey_colnames` must all appear in either `names(df)` or `names(unselected_ukey_cols_df)`")
  }
  if (!inherits(context_row, "data.frame")) {
    return("`context_row` must be a data.frame")
  }
  if (nrow(context_row) != 1L) {
    return("`context_row` must have exactly one row")
  }
  return(TRUE)
}
# TODO check that not data.table? perhaps might not have to, if we're data.table-unaware? but then vctrs stuff might need to consider data.table.

#' Convert a nominal kdf5 (or subclass) into a not-kdf5
#'
#' @keywords internal
nominal_kdf5_decay <- function(nominal_kdf5) {
  if (!inherits(nominal_kdf5, "keyed_df5")) {
    cli::cli_abort("`nominal_kdf5` was not marked a keyed_df5 to begin with")
  }
  result <- nominal_kdf5
  attr(result, "dplyr.extending.test::full_ukey_colnames") <- NULL
  attr(result, "dplyr.extending.test::unselected_ukey_cols_df") <- NULL
  attr(result, "dplyr.extending.test::context_row") <- NULL
  class(result) <- class(result)[class(result) != "keyed_df5"]
  result
}

df_ensure_not_kdf5 <- function(df) {
  if (inherits(df, "keyed_df5")) {
    # XXX redundant check... better organization somehow?
    nominal_kdf5_decay(df)
  } else {
    df
  }
}

#' @export
ukey_colnames_else_null.keyed_df5 <- function(x) {
  # TODO if we have no unselected cols then we have ukey; else not.
  # Unless we missed unselected -> context simplifications...
  attr(x, "dplyr.extending.test::ukey_colnames")
}

kdf5_self <- function(x) {
  if (!inherits(x, "keyed_df5")) {
    cli::cli_abort("`x` was not marked a keyed_df5 to begin with")
  }
  result <- x
  old_class <- class(result)
  # TODO note only nominally stripping subclasses; attrs will remain.
  class(result) <- old_class[match("keyed_df5", old_class):length(old_class)]
  result
}

# TODO any good helpers to re-tag with subclasses?

kdf5_super <- function(x) {
  if (!inherits(x, "keyed_df5")) {
    cli::cli_abort("`x` was not marked a keyed_df5 to begin with")
  }
  old_class <- class(x)
  result <- x
  # TODO note only nominally stripping self and subclasses; attrs will remain.
  class(result) <- old_class[(match("keyed_df5", old_class) + 1L):length(old_class)]
  result
}

#' Is / why isn't data.frame/subclass `x` compatible with keyed_df5 invariants
#'
#' @param x data.frame (possibly subclassed)
#' @return TRUE or str
df_check_kdf5_compatible <- function(x, full_ukey_colnames, unselected_ukey_cols_df, context_row) {
  # TODO omit check if possible
  #
  # TODO structural checks
  #
  # TODO proper caller_arg passing
  if (!all(full_ukey_colnames %in% names(x))) {
    "didn't have one of the `ukey_colnames`"
  } else {
    if (inherits(x, "keyed_df5")) {
      maybe_super <- kdf5_super(x)
    } else {
      maybe_super <- x
    }
    if (vctrs::vec_duplicate_any(maybe_super[full_ukey_colnames])) {
      "contained duplicate ukey values"
    } else {
      TRUE
    }
  }
}

# TODO better doc
#' Convert to kdf5
#' @export
as_keyed_df5 <- function(x, ukey_colnames, context_row = tibble::new_tibble(list(), nrow = 1L)) {
  unselected_ukey_cols_df <- x[,integer()]
  as_keyed_df5_selection(x, ukey_colnames, unselected_ukey_cols_df, context_row)
}

as_keyed_df5_selection <- function(x, full_ukey_colnames, unselected_ukey_cols_df, context_row) {
  check <- df_check_kdf5_compatible(x, full_ukey_colnames, unselected_ukey_cols_df, context_row)
  if (isTRUE(check)) {
    df_ensure_structural_keyed_df5(x, full_ukey_colnames, unselected_ukey_cols_df, context_row)
  } else {
    # TODO use validator function instead.
    cli::cli_abort("`x` {check}")
  }
}

# TODO validate

df_ensure_structural_keyed_df5 <- function(x, full_ukey_colnames, unselected_ukey_cols_df, context_row) {
  not_kdf5 <- df_ensure_not_kdf5(x)
  new_keyed_df5(not_kdf5, full_ukey_colnames, unselected_ukey_cols_df, context_row)
}
# XXX should we be allowing kdf5 in the middle of the class list?

df_kdf5_incompatible_clean <- function(df) {
  df_class <- class(df)
  class(df) <- df_class[df_class != "keyed_df5"]
  attr(result, "dplyr.extending.test::full_ukey_colnames") <- NULL
  attr(result, "dplyr.extending.test::unselected_ukey_cols_df") <- NULL
  attr(result, "dplyr.extending.test::context_row") <- NULL
  df
}

# df_as_keyed_df5_if_compatible -->
df_as_keyed_df5_else_clean <- function(df, full_ukey_colnames, unselected_ukey_cols_df, context_row) {
  if (isTRUE(df_check_kdf5_compatible(df, full_ukey_colnames, unselected_ukey_cols_df, context_row))) {
    df_ensure_structural_keyed_df5(df, full_ukey_colnames, unselected_ukey_cols_df, context_row)
  } else {
    df_kdf5_incompatible_clean(df)
  }
}
# TODO where should simplification of unique ukey cols go?

#' @export
print.keyed_df5 <- function(x, ...) {
  # TODO pillar stuff, cli toString alternative
  full_ukey_colnames <- attr(x, "dplyr.extending.test::full_ukey_colnames")
  unselected_ukey_colnames <- names(attr(x, "dplyr.extending.test::unselected_ukey_cols_df"))
  context_row <- attr(x, "dplyr.extending.test::context_row")
  if (length(unselected_ukey_colnames) == 0L) {
    header <- glue::glue('# keyed_df5[{toString(full_ukey_colnames)}]')
  } else {
    selected_ukey_colnames <- vctrs::vec_set_difference(full_ukey_colnames, unselected_ukey_colnames)
    header <- glue::glue('# keyed_df5_view[{toString(selected_ukey_colnames)} without {toString(unselected_ukey_colnames)}]')
  }
  if (length(context_row) != 0L) {
    # TODO name=value, not just value
    header <- header + glue::glue(' for {toString(paste0(names(context_row), " = ", context_row))}')
  }
  header <- header + " backed by:"
  print(header)
  NextMethod()
}

# FIXME TODO FINISH

#' @export
`names<-.keyed_df5` <- function(x, value) {
  result <- NextMethod()
  old_names <- names(x)
  old_key_colnames <- ukey_colnames(x)
  new_key_colnames <- value[match(old_key_colnames, old_names)]
  df_ensure_structural_keyed_df5(x, new_key_colnames)
}

kdf5_extraction_restore_kdf5_if_possible <- function(extraction, original, i = NULL, j = NULL) {
  if (anyNA(i)) {
    return(df_ensure_not_kdf5(extraction))
  }
  if (is.numeric(i) && length(i) >= 1L && i[[1L]] >= 1L && (max(i) > nrow(original) || vctrs::vec_duplicate_any(i))) {
    # TODO or make > nrow a hard error? maybe through a non-restore interface (maybe using vec_slice)?
    return(df_ensure_not_kdf5(extraction))
  }
  if (is.null(j)) {
    return(df_ensure_structural_keyed_df5(extraction, ukey_colnames(original)))
  }
  if (!is.character(j)) {
    j <- names(original)[j]
  }
  original_ukey_colnames <- ukey_colnames(original)
  ukey_col_included <- original_ukey_colnames %in% j
  selected_ukey_colnames <- original_ukey_colnames[ukey_col_included]
  dropped_ukey_colnames <- original_ukey_colnames[!ukey_col_included]
  if (length(dropped_ukey_colnames) == 0L) {
    return(df_ensure_structural_keyed_df5(extraction, ukey_colnames(original)))
  }
  if (is.null(i)) {
    dropped_ukey_col_values <- kdf5_super(original)[dropped_ukey_colnames]
  } else {
    dropped_ukey_col_values <- kdf5_super(original)[i, dropped_ukey_colnames]
  }
  # TODO vs. vctrs::vec_unique_count:
  if (vctrs::vec_size(dropped_ukey_col_values) != 0L &&
        !all(vctrs::vec_equal(dropped_ukey_col_values, dropped_ukey_col_values[1,]))) {
    # technically we could still have unique new-ukey values here,
    # but it seems like a violation anyway
    df_ensure_not_kdf5(extraction)
  } else {
    df_ensure_structural_keyed_df5(extraction, vctrs::vec_set_difference(ukey_colnames(original), dropped_ukey_colnames))
  }
}
# TODO vs. inline
#
# TODO vs. known-2D extractor function
#
# TODO vs. 3 functions
#
# TODO consider whether just to turn row&col slice into row-slice
# followed by col-slice; slices some unnecessary columns but might
# actually be faster

#' @export
`[.keyed_df5` <- function(x, i, j, ..., drop = FALSE) {
  rlang::check_dots_empty0(...)

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
  if (drop && length(result) == 1L) {
    # We want to drop.  NextMethod() may give us an invalid keyed_df5
    # or subclass; ensure these are gone before re-dispatching.
    if (inherits(result, "keyed_df5")) {
      result <- kdf5_super(result)
    }
    return(result[[1L]])
  }
  if (drop) {
    stop("TODO")
  }
    x_full_ukey_colnames <- attr(x, "dplyr.extending.test::full_ukey_colnames")
  if (!is.null(j)) {
    # TODO detect ukey col duplication?
    x_unselected_ukey_cols_df <- attr(x, "dplyr.extending.test::unselected_ukey_cols_df")
    freshly_unselected_colnames <- vctrs::vec_set_difference(names(x), names(result))
    freshly_unselected_ukey_colnames <- vctrs::vec_set_intersect(freshly_unselected_colnames, x_full_ukey_colnames)
    result_unselected_ukey_cols_df <- dplyr_col_modify(x_unselected_ukey_cols_df, kdf5_super(x)[freshly_unselected_colnames])
  } else {
    result_unselected_ukey_cols_df <- attr(x, "dplyr.extending.test::unselected_ukey_cols_df")
  }
  if (!is.null(i)) {
    result_unselected_ukey_cols_df <- result_unselected_ukey_cols_df[i,]
    is_constant <- vapply(result_unselected_ukey_cols_df, vctrs::vec_unique_count, integer(1L)) == 1L
    fresh_context <- result_unselected_ukey_cols_df[1L, is_constant]
    result_unselected_ukey_cols_df <- result_unselected_ukey_cols_df[!is_constant]
    result_context_row <- attr(x, "dplyr.extending.test::context_row")
    result_context_row[, names(fresh_context)] <- fresh_context
    result_full_ukey_colnames <- vctrs::vec_set_difference(x_full_ukey_colnames, names(fresh_context))
  } else {
    result_context_row <- attr(x, "dplyr.extending.test::context_row")
    result_full_ukey_colnames <- x_full_ukey_colnames
  }
  result <- df_ensure_structural_keyed_df5(result, result_full_ukey_colnames, result_unselected_ukey_cols_df, result_context_row)
  # ^ TODO skip structural checks?
  result
}

#' @export
`[<-.keyed_df5` <- function(x, i, j, ..., value) {
  rlang::check_dots_empty0(...)

  result <- NextMethod()

  x_ukey_colnames <- ukey_colnames(x)
  call_was_1d <- nargs() == 3L && !missing(i)
  if (call_was_1d) {
    if (is.matrix(i)) {
      maybe_new_ukey_colnames <- vctrs::vec_set_intersect(names(result), x_ukey_colnames)
      result <- df_as_keyed_df5_else_clean(df_ensure_not_kdf5(result), maybe_new_ukey_colnames)
      return(result)
    } else {
      j <- i
      i <- NULL
    }
  }
  if (!is.character(j)) {
    j <- names(x)[j]
  }
  if (any(j %in% x_ukey_colnames)) {
    # XXX S3 dispatch on names probably a decorator dispatch
    # violation... temporarily turn into kdf5_super, re-dispatch,
    # conditionally kdf5, then tack back subclasses?
    maybe_new_ukey_colnames <- vctrs::vec_set_intersect(names(result), x_ukey_colnames)
    result <- df_as_keyed_df5_else_clean(df_ensure_not_kdf5(result), maybe_new_ukey_colnames)
  }
  result
}

#' @export
`$<-.keyed_df5` <- function(x, name, value) {
  x[name] <- list(value)
  x
}

#' @export
`[[<-.keyed_df5` <- function(x, i, value) {
  # XXX documentation for args of `[[<-` being limited to (x, i, value)
  # doesn't match what we can put into data.frame `[[<-` methods...
  x[i] <- list(value)
  x
}


#' @export
dplyr_row_slice.keyed_df5 <- function(data, i, ...) {
  kdf5_self(data)[i,]
  # XXX may have old subclass attrs sticking around, but maybe not
  # guaranteed... do we need to guarantee or does subclass need to
  # guarantee correct post-processing?
}

#' @export
dplyr_col_modify.keyed_df5 <- function(data, cols) {
  data <- kdf5_self(data)
  # XXX may have old subclass attrs sticking around, but maybe not
  # guaranteed... do we need to guarantee or does subclass need to
  # guarantee correct post-processing?
  data[names(cols)] <- cols
  data
}

#' @export
dplyr_reconstruct.keyed_df5 <- function(data, template) {
  maybe_result_ukey_colnames <- vctrs::vec_set_intersect(names(data), ukey_colnames(template))
  df_as_keyed_df5_else_clean(df_ensure_not_kdf5(NextMethod()), maybe_result_ukey_colnames)
  # XXX may have old subclass attrs sticking around, but maybe not
  # guaranteed... do we need to guarantee or does subclass need to
  # guarantee correct post-processing?
}

#' @importFrom vctrs vec_ptype2
#' @importFrom rlang caller_arg caller_env
#' @export
vec_ptype2.keyed_df5.keyed_df5 <- function(x, y, ..., x_arg = caller_arg(x), y_arg = caller_arg(y), call = caller_env()) {
  # XXX there's also the matter of ukey ordering... we might use this
  # to determine convenience sorts, so we may not necessarily be able
  # to apply a fixed-(C-)locale alphabetization to get a canonical
  # ptype.  Let's just require strict matching.
  #
  # XXX vs. just favoring x's ordering...
  #
  # TODO what about non-identical? if (sxy, ox) is unique in x and
  # (sxy, oy) is unique in y, do we attempt (sxy, ox, oy)? if (sxy,
  # ax) and (sxy), do we attempt (sxy, ax)?  Maybe not automatically;
  # seems automatic is tied to rbinding and "implicit" rbinding... we
  # could allow vec_cast to add more ukey cols though.
  x_ukey_colnames <- ukey_colnames(x)
  y_ukey_colnames <- ukey_colnames(y)
  if (identical(x_ukey_colnames, y_ukey_colnames)) {
    new_keyed_df5(
      vec_ptype2(
        kdf5_super(x),
        kdf5_super(y),
        ...,
        x_arg = glue::glue("kdf5_super({x_arg})"),
        y_arg = glue::glue("kdf5_super({y_arg})"),
        call = call
      ),
      x_ukey_colnames,
      vec_ptype2(
      ),
    )
  } else {
    # cli::cli_abort("`{x_arg}` and `{y_arg}` have incompatible `ukey_colnames`", call = call)
    vctrs::stop_incompatible_type(
      x, y, x_arg = x_arg, y_arg = y_arg, call = call,
      details = cli::format_message(c(
        "x" = "`{x_arg}` and `{y_arg}` had incompatible `ukey_colnames`",
        "i" = "`ukey_colnames({x_arg})`: {ukey_colnames(x)}",
        "i" = "`ukey_colnames({y_arg})`: {ukey_colnames(y)}"
        # TODO port format functions
      ))
    )
  }
}

#' @export
vec_ptype2.keyed_df5.data.frame <- function(x, y, ..., x_arg = caller_arg(x), y_arg = caller_arg(y), call = caller_env()) {
  vec_ptype2(
    kdf5_super(x), y, ...,
    x_arg = glue::glue("kdf5_super({x_arg})"),
    y_arg = y_arg,
    call = call
  )
}

#' @export
vec_ptype2.keyed_df5.tbl_df <- vec_ptype2.keyed_df5.data.frame

# if not actually potentially part of a decorator stack, could just rely on vec_default_{cast,ptype2}...

# #' @export
# vec_ptype2.keyed_df5.default <- function(x, y, ..., x_arg = caller_arg(x), y_arg = caller_arg(y), call = caller_env()) {
#   cat("MADE IT HERE!\n")
#   stop("TODO")
# }

#' @export
vec_ptype2.data.frame.keyed_df5 <- function(x, y, ..., x_arg = caller_arg(x), y_arg = caller_arg(y), call = caller_env()) {
  vec_ptype2(
    x, kdf5_super(y), ...,
    x_arg = x_arg,
    y_arg = glue::glue("kdf5_super({y_arg})"),
    call = call
  )
}

#' @export
vec_ptype2.tbl_df.keyed_df5 <- vec_ptype2.data.frame.keyed_df5

# XXX no(?) way to make this work with decorators that don't know about each other... unless we have a decorator_df as the head class always and have it handle dispatch, which might be doable... or maybe we can have a registry of df-lookalikes plus vctrs ptypes, and auto-register a whole bunch of stuff on new_* as well as hook on every installed package...

# TODO consider at least vec_cast to/from tsibble (to only for time-key-supporting...)

# TODO other vec_ptype2 impls

# TODO vec_cast

#' @export
vec_cast.keyed_df5.keyed_df5 <- function(x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  x_ukey_colnames <- ukey_colnames(x)
  to_ukey_colnames <- ukey_colnames(to)
  if (identical(x_ukey_colnames, to_ukey_colnames)) {
    x
  } else if (all(x_ukey_colnames %in% to_ukey_colnames)) {
    attr(x, "dplyr.extending.test::ukey_colnames") <- to_ukey_colnames
    x
  } else {
    vctrs::stop_incompatible_cast(
      x, to, ..., x_arg = x_arg, to_arg = to_arg,
      details = cli::format_message(c(
        "x" = "`{to_arg}`'s ukey colnames were not a superset of `{x_arg}`'s",
        "i" = "`ukey_colnames({x_arg})`: {ukey_colnames(x)}",
        "i" = "`ukey_colnames({to_arg})`: {ukey_colnames(to)}"
        # TODO port format functions
      ))
    )
  }
}

#' @export
vec_cast.keyed_df5.data.frame <- function(x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  # # vctrs native dispatch -> we are head class; we can re-dispatch cleanly
  # dplyr_reconstruct(x, to)
  as_keyed_df5(
    vec_cast(x, kdf5_super(to), ..., x_arg = x_arg, to_arg = "kdf5_super({to_arg})", call = call),
    ukey_colnames(to)
  )
}

#' @export
vec_cast.keyed_df5.tbl_df <- vec_cast.keyed_df5.data.frame

#' @export
vec_cast.data.frame.keyed_df5 <- function(x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env()) {
  vec_cast(kdf5_super(x), to, ..., x_arg = glue::glue("kdf5_super({x_arg})"), to_arg = to_arg, call = call)
}

#' @export
vec_cast.tbl_df.keyed_df5 <- vec_cast.data.frame.keyed_df5

#' @method as.data.frame keyed_df5
#' @export
as.data.frame.keyed_df5 <- function(x, ...) {
  result <- NextMethod()
  # TODO refactor this into a function? df_ensure_not_kdf5 doesn't clean attrs of non-kdf5-classed things
  attr(result, "dplyr.extending.test::ukey_colnames") <- NULL
  result
}

#' @importFrom tibble as_tibble
#' @method as_tibble keyed_df5
#' @export
as_tibble.keyed_df5 <- function(x, ...) {
  result <- NextMethod()
  attr(result, "dplyr.extending.test::ukey_colnames") <- NULL
  result
}

# #' @importFrom vctrs vec_proxy
# #' @export
# vec_proxy.keyed_df5 <- function(x, ...) {
#   x
# }

#' @importFrom vctrs vec_restore
#' @export
vec_restore.keyed_df5 <- function(x, to, ...) {
  as_keyed_df5(vec_restore(kdf5_super(x), kdf5_super(to)), ukey_colnames(to))
}

#' @importFrom dplyr group_data
#' @export
group_by.keyed_df5 <- function(.data, ...) {
  df_ensure_structural_keyed_df5(NextMethod(), ukey_colnames(.data))
}

#' @importFrom dplyr group_data
#' @export
group_data.keyed_df5 <- function(.data) {
  result <- NextMethod()
  .data_group_vars <- vctrs::vec_set_difference(names(result), ".rows")
  new_keyed_df5(result, .data_group_vars)
}

#' @importFrom dplyr inner_join
#' @export
inner_join.keyed_df5 <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., multiple = "all", relationship = NULL) {
  if (is.null(by)) {
    by <- vctrs::vec_set_intersect(names(x), names(y))
    cli_inform('Joining with `by = {paste(collapse = "", deparse(by))}`')
  }
  if (inherits(by, "dplyr_join_by")) {
    x_by <- by$x
    y_by <- by$y
  } else {
    x_by <- names(by) %||% by
    y_by <- unname(by)
  }

  x_inp_ukey_nms <- ukey_colnames(x)
  x_out_ukey_nms <- x_inp_ukey_nms
  x_out_ukey_nm_needs_suffix <- (! x_out_ukey_nms %in% x_by) & x_out_ukey_nms %in% names(y)
  x_out_ukey_nms[x_out_ukey_nm_needs_suffix] <- paste0(x_out_ukey_nms[x_out_ukey_nm_needs_suffix], suffix[[1L]])
  x_out_nonby_ukey_nms <- vctrs::vec_set_difference(x_out_ukey_nms, x_by)

  if (!is.null(relationship) && relationship %in% c("one-to-one", "many-to-one") ||
        multiple %in% c("first", "any", "last")) {
    # We already knew that each x ukey value maps to a single `by`
    # value, and now `dplyr` will check that each "by value" does not
    # map to multiple rows in `y`.  So ukeys from `x` will be ukeys in
    # the result.
    result_ukey_nms_else_null <- x_out_ukey_nms
    y_out_nonby_ukey_nms <- character()
  } else {
    y_inp_ukey_nms_else_null <- ukey_colnames_else_null(y)
    if (is.null(y_inp_ukey_nms_else_null)) {
      result_ukey_nms_else_null <- NULL
    } else {
      y_inp_nonby_ukey_nms <- vctrs::vec_set_difference(y_inp_ukey_nms_else_null, y_by)
      y_out_nonby_ukey_nms <- y_inp_nonby_ukey_nms
      y_out_nonby_ukey_nm_needs_suffix <- y_out_nonby_ukey_nms %in% names(x)
      y_out_nonby_ukey_nms[y_out_nonby_ukey_nm_needs_suffix] <- paste0(y_out_nonby_ukey_nms[y_out_nonby_ukey_nm_needs_suffix], suffix[[2L]])
      result_ukey_nms_else_null <- c(x_out_ukey_nms, y_out_nonby_ukey_nms)
    }
  }

  # Avoid unnecessary ukey validation from NextMethod()'s
  # dplyr_reconstruct by converting to superclass:
  orig_x <- x
  x_class <- class(x)
  x_self_ind <- match("keyed_df5", x_class)
  x_subclass <- x_class[seq_len(x_self_ind - 1L)]
  class(x) <- x_class[(x_self_ind + 1L):length(x_class)]
  result <- NextMethod(by = by) # must manually pass optional arg "override"
  template <- x
  if (!is.null(result_ukey_nms_else_null)) {
    if (.Generic %in% c("left_join", "full_join") && length(y_out_nonby_ukey_nms) != 0L) {
      if (vctrs::vec_any_missing(result[y_out_nonby_ukey_nms])) {
        cli_abort(c(
          "Join may have introduced missing values for {?this/these} ukey column{?s}:
           {y_out_nonby_ukey_nms}",
          ">" = "Check that you didn't mean to include
                 {cli::qty(y_out_nonby_ukey_nms)} {?this/these} column{?s} in `by`",
          "i" = "Otherwise, either
                 (a) the missing ukey value was already in `y` and the join simply propagated it, or
                 (b) there was a `by` value in `x` that was not present in `y`.",
          ">" = 'To make case (a) work while still checking for (b), move to
                 {switch(.Generic, "left_join" = "inner_join(unmatched = c(<x setting>, \\"error\\"))",
                                   "full_join" = "right_join(unmatched = \\"error\\"")}.'
        ))
      }
    }
    if (.Generic %in% c("right_join", "full_join") && length(y_out_nonby_ukey_nms) != 0L) {
      if (vctrs::vec_any_missing(result[y_out_nonby_ukey_nms])) {
        cli_abort(c(
          "Join may have introduced missing values for {?this/these} ukey column{?s}:
           {x_out_nonby_ukey_nms}",
          ">" = "Check that you didn't mean to include
                 {cli::qty(x_out_nonby_ukey_nms)} {?this/these} column{?s} in `by`",
          "i" = "Otherwise, either
                 (a) the missing ukey value was already in `x` and the join simply propagated it, or
                 (b) there was a `by` value in `y` that was not present in `x`.",
          ">" = 'To make case (a) work while still checking for (b), move to
                 {switch(.Generic, "right_join" = "inner_join(unmatched = c(\\"error\\", <y setting>))",
                                   "full_join" = "right_join(unmatched = \\"error\\"")}.'
        ))
      }
    }
    template <- new_keyed_df5(template, result_ukey_nms_else_null)
    result <- new_keyed_df5(result, result_ukey_nms_else_null)
  }
  template <- reconstruct_as_is(template)
  class(template) <- c(x_subclass, class(template))
  result <- partial_reconstruct(result, template)
  result
}

#' @importFrom dplyr left_join
#' @export
left_join.keyed_df5 <- inner_join.keyed_df5

#' @importFrom dplyr right_join
#' @export
right_join.keyed_df5 <- inner_join.keyed_df5

#' @importFrom dplyr full_join
#' @export
full_join.keyed_df5 <- inner_join.keyed_df5

#' @importFrom dplyr cross_join
#' @export
cross_join.keyed_df5 <- function(x, y, ..., copy = FALSE, suffix = c(".x", ".y")) {
  orig_x <- x
  x_class <- class(x)
  x_self_ind <- match("keyed_df5", x_class)
  x_subclass <- x_class[seq_len(x_self_ind - 1L)]
  class(x) <- x_class[(x_self_ind + 1L):length(x_class)]
  result <- NextMethod()
  y_ukey_nms_else_null <- ukey_colnames_else_null(y)
  template <- x
  if (!is.null(y_ukey_nms_else_null)) {
    x_out_ukey_nms <- ukey_colnames(orig_x)
    x_out_ukey_nm_needs_suffix <- x_out_ukey_nms %in% names(y)
    x_out_ukey_nms[x_out_ukey_nm_needs_suffix] <- paste0(x_out_ukey_nms[x_out_ukey_nm_needs_suffix], suffix[[1L]])
    y_out_ukey_nms <- ukey_colnames(y)
    y_out_ukey_nm_needs_suffix <- y_out_ukey_nms %in% names(orig_x)
    y_out_ukey_nms[y_out_ukey_nm_needs_suffix] <- paste0(y_out_ukey_nms[y_out_ukey_nm_needs_suffix], suffix[[2L]])
    result_ukey_nms <- c(x_out_ukey_nms, y_out_ukey_nms)
    template <- new_keyed_df5(template, result_ukey_nms)
    result <- new_keyed_df5(result, result_ukey_nms)
  }
  template <- reconstruct_as_is(template)
  class(template) <- c(x_subclass, class(template))
  result <- partial_reconstruct(result, template)
  result
}

#' @importFrom dplyr nest_join
#' @export
nest_join.keyed_df5 <- function(x, y, by = NULL, copy = FALSE, keep = NULL, name = NULL, ...) {
  if (is.null(name)) {
    # quickly apply this default, before anything potentially forces
    # `y` and mess up `enexpr` result
    name <- rlang::as_label(rlang::enexpr(y))
  }
  if (is.null(by)) {
    by <- vctrs::vec_set_intersect(names(x), names(y))
    cli_inform('Joining with `by = {paste(collapse = "", deparse(by))}`')
  }
  if (is.null(keep)) {
    keep <- FALSE
  } else {
    if (!rlang::is_bool(keep)) {
      cli_abort("`keep` must be `TRUE`, `FALSE`, or `NULL`, not {rlang::obj_type_friendly(keep)}")
    }
  }
  if (keep) {
    y_out_elt_nms <- names(y)
  } else {
    if (inherits(by, "dplyr_join_by")) {
      y_by <- by$y
    } else {
      y_by <- unname(by)
    }
    y_out_elt_nms <- vctrs::vec_set_difference(names(y), y_by)
  }
  # TODO see/migrate notes from keyed_df4
  y_out_elt_template <- dplyr_row_slice(y, integer())[y_out_elt_nms]
  y <- as_tibble(y)
  result <- NextMethod(by = by, keep = keep, name = name) # must manually pass optional arg "override", esp. since `y` forcing breaks `name` default
  result[[name]] <- lapply(result[[name]], dplyr_reconstruct, y_out_elt_template)
  result
}

# TODO nest and unnest, ...

# TODO finish

# TODO review https://vctrs.r-lib.org/reference/howto-faq-coercion-data-frame.html



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

# group_data extension rather than attr "groups" modifications may
# make inheritance make more sense as structure is only added rather
# than changed.  though duplicate processing on re-dispatch still
# applies.  however, decorator approach still seems like it would be
# more flexible if the structure assumptions were ever removed.







# TODO removed-ukey-col tracking? or chop_extract in group_split
#
# TODO nest, unnest if removed-ukey-col doesn't auto
#
# TODO context keys

# TODO pivot functions

# TODO complete function, separate & unite functions, ...

# TODO .by=, by= args...

# TODO time key mgmt
#
# TODO ephemeral role-specifying wrappers for inset operations?
#
# TODO separate unit & time ukeys & aggregation mechanisms...
# * index_by approach?
# * auto-mark detectably-derived cols purely from unit or purely from time + group_by & .by doing something similar?

# TODO check `[` does not introduce duplicate colnames, at least for ukey cols

# TODO check for ukey_colnames redispatching all over the place... should not redispatch without converting to self first.

# TODO pillar/tbl/whatever methods needed to get a better header

# TODO could a more direct Result type help simplify some checks and/or restructuring operations?
