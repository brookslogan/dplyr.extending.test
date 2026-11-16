
new_suspended <- function(obj, templates) {
  if (inherits(obj, "suspended")) {
    rlang::abort('`obj` is already a `"suspended"`; please double check usage or consider `suspend_class`')
  }
  if (!is.list(templates)) {
    rlang::abort("`templates` must be a list")
  }
  class(obj) <- c("suspended", class(obj))
  attr(obj, "dplyr.extending.test::suspended::templates") <- templates
  obj
}



# suspend_class <-
# suspend_classes <-
# suspend_ukeyed <-

# ?.Class
