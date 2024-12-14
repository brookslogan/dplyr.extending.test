#' Low-level constructor for keyed_tibble2; use as_keyed_tibble2 or use carefully
#'
#' This does not validate that there are no duplicate rows in `x`; use this only
#' if you have already verified that. For a constructor/converter that validates
#' that for you, use `as_keyed_tibble2` instead.
#'
#' @param key_colnames chr
#'
#' @export
new_keyed_tibble2 <- function(x, key_colnames) {
  if (inherits(x, "keyed_tibble2")) {
    stop("x must not already be a keyed_tibble2")
  }
  if (!tibble::is_tibble(x)) {
    stop("x must be a tibble")
  }
  if (!inherits(key_colnames, "character")) {
    stop("key_colnames must be character vector")
  }
  class(x) <- c("keyed_tibble2", class(x))
  attr(x, "dplyr.extending.test::key_colnames") <- key_colnames
  x
}



# ktbl2_decay
# ktbl2_decay_if_bad
# df_decay_if_ktbl2
# df_decay_if_bad_ktbl2
# obj_decay_if_bad_ktbl2

# VS

# ktbl2_decay
# ktbl2_if_bad_decay
# df_if_ktbl2_decay
# df_if_bad_ktbl2_decay
# obj_if_bad_ktbl2_decay

# VS

# ...




#' @importFrom dplyr dplyr_row_slice
#' @export
dplyr_row_slice.keyed_tibble2 <- function(data, i, ...) {
  data[i,]
}
