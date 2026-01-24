
# #' @import dplyr
# #' @import tidyr
# NULL

# We want to be head class to handle vctrs ops.  (Or alternatively,
# perhaps we could have a specific decorator for this purpose?)

# Two possible approaches to the class here:
#
# * Keep subclassing with a long class vector.  Bad for nested
#   decorators of same type, if ever desired (multiple ukeys via class
#   chaining?).  May make some operations more awkward if they need a
#   default behavior to trigger on every wrapper rather than passing
#   through to first one with method.  Does not allow structural
#   modifications, only additions via attrs.  Makes class vector long.
#
# * Structurally wrap.  Needs every S3 method to have a abstract
#   decorator impl to manually forward in case there is not a specific
#   decorator impl.  But how do we get to that impl?  That impl cannot
#   be in front of abstract decorator class, because we need
#   abstract-decorator class in front for vctrs stuff.  Do we need
#   `before_decorator` and `after_decorator` class?  So
#   `before_decorator` can handle vctrs, and `after_decorator` can
#   provide decorator default behavior?
#
# * (Sort of intermediate between the two is the attr-wrap approach if
#   we want to use ?dplyr_extending, where we keep the abstract
#   decorator, outermost concrete decorator, and data.frame classes,
#   and have an attr tracking the inner concrete decorators.  But this
#   doesn't let us wrap abstract tbls.)

# #' @export
# arrange.tbl_decorator <- function(.data, ..., .by_group = FALSE) {
# }
