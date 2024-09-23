
#' @export
as_trace_tibble <- function(x, ...) UseMethod("as_trace_tibble")

#' @export
as_trace_tibble.trace_tibble <- function(x, ...) {
  x
}

#' @export
as_trace_tibble.tbl_df <- function(x, my_attr = 1, ...) {
  `attr<-`(`class<-`(x, c("trace_tibble", class(x))), "my_attr", my_attr)
}

# TODO proper pillar/vctrs interop instead of custom print

#' @export
print.trace_tibble <- function(x, ...) {
  cat("# Actually a trace_tibble\n")
  cat("# my_attr:", attr(x, "my_attr"), "\n")
  NextMethod()
}

#' @importFrom dplyr dplyr_row_slice
#' @export
dplyr_row_slice.trace_tibble <- function(data, i, ...) {
  cat("dplyr_row_slice.trace_tibble\n")
  print(class(data))
  ## print(data)
  NextMethod()
}

#' @importFrom dplyr dplyr_col_modify
#' @export
dplyr_col_modify.trace_tibble <- function(data, cols) {
  cat("dplyr_col_modify.trace_tibble\n")
  print(class(data))
  ## print(data)
  NextMethod()
}

#' @importFrom dplyr dplyr_reconstruct
#' @export
dplyr_reconstruct.trace_tibble <- function(data, template) {
  cat("dplyr_reconstruct.trace_tibble\n")
  print(class(data))
  ## print(attributes(data))
  print(attr(data, "my_attr"))
  ## print(data)
  NextMethod()
}

## #' @export
## `[.trace_tibble` <- function(x, j) {
##   cat("[.trace_tibble\n")
##   print(rlang::call_match())
##   NextMethod()
## }

#' @export
`[.trace_tibble` <- function(x, i, j, ..., drop = FALSE) {
  cat("[.trace_tibble\n")
  print(rlang::call_match())
  print(class(x))
  ## print(x)
  if (drop) {
    stop('drop = TRUE is unsupported')
  }
  NextMethod()
}

#' @export
`names<-.trace_tibble` <- function(x, value) {
  cat("names<-.trace_tibble\n")
  print(class(x))
  ## print(x)
  NextMethod()
}
