
#' @export
test_generic <- function(x,y) UseMethod("test_generic")

#' @export
test_generic.asdf <- function(x,y) {
  w <- x
  y <- x
  x <- rlang::missing_arg()
  # y <- 858
  res <- NextMethod()
  # browser()
  4321
}

#' @export
test_generic.qwer <- function(x,y) {
  z <- 42
  print(missing(x))
  print(missing(y))
  1234
}

# test_generic(structure(list(),class=c("asdf","qwer")), 87)
# test_generic(structure(list(),class=c("asdf","qwer")), )
# test_generic(structure(list(),class=c("asdf","qwer")))
