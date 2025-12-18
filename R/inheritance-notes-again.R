#' Thoughts on inheritance after initial draft of keyed_df4
#' @name inheritance_notes_again
#'
#' S3 methods provide one way to obtain (downstream-extensible)
#' polymorphism, allowing analogous operations for different classes
#' to be called using the same function name (and for downstream
#' packages to add to the set of supported classes).  Sometimes, users
#' call methods directly (e.g., `mutate`). Sometimes, we want build on
#' top of a set of core methods to provide a broader set of
#' polymorphic functionality.  One example of the latter is
#' `dplyr_extending`: here, the upstream package requires downstream
#' packages to provide a `data.frame` subclass and implement methods
#' for a few generics (e.g., `dplyr_row_slice`), and provides a
#' broader set of functionality (e.g., `mutate`, via
#' `mutate.data.frame`).  Two common issues when using inheritance
#' systems like S3 for this sort of composition are:
#'
#' * The base class potentially-needlessly requiring a certain data
#'   structure underneath (e.g., `dplyr_extending` requires a
#'   `data.frame`), so cannot freely organize or reorganize data in
#'   subclasses.  They can add structure via attributes, though as we
#'   see, this requires care.
#'
#' * S3 methods for one class that call other S3 generics will
#'   potentially re-dispatch to its subclasses.
#'
#'   * E.g., `mutate.data.frame` calls `dplyr_col_modify`.  But we
#'     might also think about `[` implementations that call
#'     `dplyr_row_slice`, or vice versa.
#'
#'   * If the subclass methods are "compliant" and don't destroy
#'     structure that's essential to the original class, then the
#'     original class' methods will likely be fine, though they may
#'     have extra classes or structure attached to them which they may
#'     or may not invalidate or destroy, so subclass methods have to
#'     make sure to perform appropriate clean-up.
#'
#'   * Re-dispatching to other S3 generics may involve performing
#'     duplicate work to check or preserve added structure and
#'     invariants, since these checks may be needed in methods for
#'     each callee generic, as well as in a method for the original
#'     generic (since the parent class may have performed additional
#'     manipulations that invalidated them, unless we know it very
#'     rigidly sticks to using only the subclass methods it says that
#'     it depends on, and no subclass-ignoring base operations, FFI,
#'     `unclass`, `class<-`, etc. --- `dplyr_extending` on ungrouped
#'     data.frame subclasses might be one such case).
#'
#' If we have an upstream generic that outputs something of the same
#' class as its dispatch/polymorphic argument, plus or minus some
#' upstream class entries upstream class entries (e.g., `mutate`,
#' `group_by`, `ungroup`), and are implementing a method for a
#' subclass, what should we expect out of `NextMethod()`?
#'
#' * Our class entry might remain, or it might be stripped.
#'
#' * Our attrs may be held over from the input argument, or they may
#'   be outputted from helper S3 methods called by the parent's main
#'   method, or they may be stripped.  Said another way, we may have
#'   valid, invalid, or none of our subclass' attrs.  We may have our
#'   attrs but lack our class, or vice versa.
#'
#' * If our input had subsubclasses, the NextMethod output may include
#'   or exclude their class entries.
#'
#' * If our input had subsubclasses, the NextMethod output may have
#'   valid/invalid/absent attrs.
#'
#'
#' If we're writing df -> df method and potentially going to be called
#' by a subclass' NextMethod(), then should we guarantee that we'll
#' preserve its input class and attrs?  (Or whatever the base upstream
#' ancestor class does, so our interjection doesn't change base
#' expectations?)  Or should we maintain freedom and make it the
#' subclass' job to worry about it?  Latter would not be very kind if
#' we're overriding default verbs provided by `?dplyr_extending`,
#' since they'd then require methods for all subclasses as well. For
#' core df methods such as `?dplyr_extending` requirements and base
#' operations, it seems fine, except `?dplyr_extending` says some are
#' sometimes optional, and so then subclasses might still expect
#' things to be preserved.  If we use a packing wrapper (e.g.,
#' keyed_tibble2) with manual wrappers of all methods to replace
#' NextMethod dispatch with wrapper dispatch, then we'd not have to
#' worry about wrapped class and attrs within wrappers at all, but
#' mutate impl may rule out easy structural wrappers.  If we use a
#' decorated_df with overrides for all verbs that change to
#' class&attrs stored in an attr and redispatch, then decorators may
#' not need to worry about other decorators' attrs; the decorated_df
#' wrapper could handle un- & re-wrapping.
NULL
