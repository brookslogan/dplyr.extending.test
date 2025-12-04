# Actual decorator approach via packed tibbles.

new_keyed_tbl3 <- function(x, ukey_colnames) {
  if (!is.data.frame(x)) {
    cli::cli_abort("`x` must be a data.frame")
  }
  if (!is.character(ukey_colnames)) {
    cli::cli_abort("`ukey_colnames` must be a character vector")
  }
  if (!all(ukey_colnames %in% names(x))) {
    cli::cli_abort("`ukey_colnames` must be a subset of `names(x)`")
  }
  result <- new_tibble(list(x = x), class = "keyed_tbl3")
  attr(result, "dplyr.extending.test::ukey_colnames") <- ukey_colnames
  class(result) <- c("keyed_tbl3", class(result))
  result
}

#' @export
ukey_colnames.keyed_tbl3 <- function(x) {
  attr(x, "dplyr.extending.test::ukey_colnames")
}

#' @export
print.keyed_tbl3 <- function(x, ...) {
  print(glue::glue('# keyed_tbl3[{toString(ukey_colnames(x))}] of:\n'))
  print(unclass(x)$x)
  invisible(x)
}

#' @export
group_by.keyed_tbl3 <- function(.data, ...) {
  stop("TODO via grouping decorator? or forwarding op to $x?")
  # XXX decorator stack makes is_grouped_df unreliable unless every
  # decorator ensures the grouping decorator is outermost
  #
  # XXX also points to keyed_df2-type approaches directly manipulating
  # "groups" attr rather than manipulating group_data being
  # potentially problematic.
}

#' @export
group_data.keyed_tbl3 <- function(.data) {
  # TODO rename $x to something that won't cause confusion if our parameter name was x
  .data_x_group_data <- group_data(unclass(.data)$x)
  .data_x_group_vars <- vctrs::vec_set_difference(names(.data_x_group_data), ".rows")
  new_keyed_tbl3(.data_x_group_data, .data_x_group_vars)
}


#' @export
`names.keyed_tbl3` <- function(x) {
  names(unclass(x)$x)
}

#' @export
`names<-.keyed_tbl3` <- function(x, value) {
  # FIXME actual logic
  old_class <- class(x)
  class(x) <- vctrs::vec_set_difference(class(x), "keyed_tbl3")
  names(x$x) <- value
  class(x) <- old_class
  x
}

#' @export
`$.keyed_tbl3` <- function(x, name) {
  unclass(x)$x[[name]]
}

#' @export
`[[.keyed_tbl3` <- function(x, ...) {
  unclass(x)$x[[...]]
}

#' @export
`$<-.keyed_tbl3` <- function(x, name, value) {
  # FIXME actual logic
  old_class <- class(x)
  class(x) <- vctrs::vec_set_difference(x, "keyed_tbl3")
  x$x[[name]] <- value
  class(x) <- old_class
  x
}

#' @export
`[[<-.keyed_tbl3` <- function(x, i, value) {
  # FIXME actual logic
  old_class <- class(x)
  class(x) <- vctrs::vec_set_difference(class(x), "keyed_tbl3")
  x$x[[i]] <- value
  class(x) <- old_class
  x
}

#' @export
`[.keyed_tbl3` <- function(x, ...) {
  # FIXME actual logic
  new_keyed_tbl3(unclass(x)$x[...], ukey_colnames(x))
}

#' @export
`[<-.keyed_tbl3` <- function(x, ..., value) {
  # FIXME actual logic
  old_class <- class(x)
  class(x) <- vctrs::vec_set_difference(class(x), "keyed_tbl3")
  # XXX ^ in more places and instead of unclass?
  x$x[...] <- value
  class(x) <- old_class
  x
}

#' @title keyed_tbl3_examples
#' @name keyed_tbl3_examples
#'
#' @examples
#'
#' toy1tbl <- tibble(k = c(1, 1, 1, 2, 2), t = c(1:3, 1:2), v = 1:5)
#'
#' toy1tbl %>%
#'   group_by(k) %>%
#'   new_keyed_tbl3(c("k", "t")) %>%
#'   {}
#'
#' toy1tbl %>%
#'   new_keyed_tbl3(c("k", "t")) %>%
#'   `[[<-`("v2", .$v) %>%
#'   {}
#'
#'
#'
#'
#'
#' toy1tbl %>%
#'   new_keyed_tbl3(c("k", "t")) %>%
#'   mutate(v2 = v) %>%
#'   {}
#' # XXX ^ this failure might be fatal for this approach; at least at
#' # first glance, this looks like dplyr's mutate is requiring no
#' # "lying" about names:
#'
#' toy1tbl %>%
#'   group_by(k) %>%
#'   new_keyed_tbl3(c("k", "t")) %>%
#'   mutate(v2 = v) %>%
#'   {}
#'
NULL
