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
  if (!inherits(x, "keyed_df4")) {
    cli::cli_abort("`x` was not marked a keyed_df4 to begin with")
  }
  result <- x
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
  } else {
    if (inherits(x, "keyed_df4")) {
      maybe_super <- kdf4_super(x)
    } else {
      maybe_super <- x
    }
    if (vctrs::vec_duplicate_any(maybe_super[ukey_colnames])) {
      "contained duplicate ukey values"
    } else {
      TRUE
    }
  }
}

# TODO better doc
#' Convert to kdf4
#' @export
as_keyed_df4 <- function(x, ukey_colnames) {
  check <- df_check_kdf4_compatible(x, ukey_colnames)
  if (isTRUE(check)) {
    df_ensure_structural_keyed_df4(x, ukey_colnames)
  } else {
    # TODO use validator function instead.
    cli::cli_abort("`x` {check}")
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
    # XXX if was nominal or structural or structural-minus-nominal
    # kdf4 that needs to decay, we have to ensure not... not sure this
    # function as-is is that helpful
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
`[.keyed_df4` <- function(x, i, j, ..., drop = FALSE) {
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
  result <- kdf4_extraction_restore_kdf4_if_possible(result, x, i, j)
  if (drop && length(result) == 1L) {
    result <- result[[1L]]
  }
  result
}

#' @export
`[<-.keyed_df4` <- function(x, i, j, ..., value) {
  rlang::check_dots_empty0(...)

  result <- NextMethod()

  x_ukey_colnames <- ukey_colnames(x)
  call_was_1d <- nargs() == 3L && !missing(i)
  if (call_was_1d) {
    if (is.matrix(i)) {
      maybe_new_ukey_colnames <- vctrs::vec_set_intersect(names(result), x_ukey_colnames)
      result <- df_as_keyed_df4_if_compatible(df_ensure_not_kdf4(result), maybe_new_ukey_colnames)
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
    # violation... temporarily turn into kdf4_super, re-dispatch,
    # conditionally kdf4, then tack back subclasses?
    maybe_new_ukey_colnames <- vctrs::vec_set_intersect(names(result), x_ukey_colnames)
    result <- df_as_keyed_df4_if_compatible(df_ensure_not_kdf4(result), maybe_new_ukey_colnames)
  }
  result
}

#' @export
`$<-.keyed_df4` <- function(x, name, value) {
  x[name] <- list(value)
}

#' @export
`[[<-.keyed_df4` <- function(x, i, value) {
  # XXX documentation for args of `[[<-` being limited to (x, i, value)
  # doesn't match what we can put into data.frame `[[<-` methods...
  x[i] <- list(value)
}


#' @export
dplyr_row_slice.keyed_df4 <- function(data, i, ...) {
  kdf4_self(data)[i,]
  # XXX may have old subclass attrs sticking around, but maybe not
  # guaranteed... do we need to guarantee or does subclass need to
  # guarantee correct post-processing?
}

#' @export
dplyr_col_modify.keyed_df4 <- function(data, cols) {
  data <- kdf4_self(data)
  # XXX may have old subclass attrs sticking around, but maybe not
  # guaranteed... do we need to guarantee or does subclass need to
  # guarantee correct post-processing?
  data[names(cols)] <- cols
  data
}

#' @export
dplyr_reconstruct.keyed_df4 <- function(data, template) {
  df_if_kdf4_compatible_as_kdf4(df_ensure_not_kdf2(NextMethod()), ukey_colnames(template))
  # XXX may have old subclass attrs sticking around, but maybe not
  # guaranteed... do we need to guarantee or does subclass need to
  # guarantee correct post-processing?
}

#' @importFrom vctrs vec_ptype2
#' @importFrom rlang caller_arg caller_env
#' @export
vec_ptype2.keyed_df4.keyed_df4 <- function(x, y, ..., x_arg = caller_arg(x), y_arg = caller_arg(y), call = caller_env()) {
  # XXX there's also the matter of ukey ordering... we might use this
  # to determine convenience sorts, so we may not necessarily be able
  # to apply a fixed-(C-)locale alphabetization to get a canonical
  # ptype.  Let's just require strict matching.
  #
  # XXX vs. just favoring x's ordering...
  #
  # TODO what about non-identical? if (sxy, ox) is unique in x and
  # (sxy, oy) is unique in y, do we attempt (sxy, ox, oy)? if (sxy,
  # ax) and (sxy), do we attempt (sxy, ax)?
  x_ukey_colnames <- ukey_colnames(x)
  y_ukey_colnames <- ukey_colnames(y)
  if (identical(x_ukey_colnames, y_ukey_colnames)) {
    new_keyed_df4(vec_ptype2(
      kdf4_super(x),
      kdf4_super(y),
      ...,
      x_arg = glue::glue('kdf4_super({x_arg})'),
      y_arg = glue::glue('kdf4_super({y_arg})'),
      call = call
    ), x_ukey_colnames)
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

# TODO other vec_ptype2 impls

# #' @importFrom vctrs vec_proxy
# #' @export
# vec_proxy.keyed_df4 <- function(x, ...) {
#   x
# }

#' @importFrom vctrs vec_restore
#' @export
vec_restore.keyed_df4 <- function(x, to, ...) {
  as_keyed_df4(vec_restore(kdf4_super(x), kdf4_super(to)), ukey_colnames(to))
}

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

# TODO group_by

# TODO pivot functions

# TODO separate unit & time ukeys & aggregation mechanisms...
# * index_by approach?
# * auto-mark detectably-derived cols purely from unit or purely from time + group_by & .by doing something similar?

# TODO ephemeral role-specifying wrappers for inset operations?

# group_data extension rather than attr "groups" modifications may
# make inheritance make more sense as structure is only added rather
# than changed.  though duplicate processing on re-dispatch still
# applies.  however, decorator approach still seems like it would be
# more flexible if the structure assumptions were ever removed.
