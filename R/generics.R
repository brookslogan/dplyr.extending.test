#' A set of colnames that act as a *u*nique identifier for rows
#'
#' @export
ukey_colnames <- function(x) UseMethod("ukey_colnames")

chop_extract <- function(x, is, j) {
  UseMethod("chop_extract")
}

#' @export
chop_extract.default <- function(x, is, j) {
  lapply(is, function(i) {
    x[i, j]
  })
}
