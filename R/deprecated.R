#' Deprecated functions
#'
#' These functions were renamed after fcutils 0.1.2.1. The old names still work,
#' but they warn and will be removed in a later version. Replace them with the
#' new names, which take the same arguments:
#'
#' | Old | New |
#' |---|---|
#' | `multi_chain()` | [chain()] |
#' | `aggr()` | [agg()] |
#' | `faggr()` | [fagg()] |
#' | `ytd_cum()` | [ytd_agg()] |
#' | `fytd_cum()` | [fytd_agg()] |
#' | `mtd_cum()` | [mtd_agg()] |
#' | `ptd_cum()` | [ptd_agg()] |
#'
#' @param ... arguments passed to the new function
#'
#' @return the result of the new function
#' @name fcutils-deprecated
#' @keywords internal
NULL

#' @rdname fcutils-deprecated
#' @export
multi_chain <- function(...) {
  .Deprecated("chain", package = "fcutils")
  chain(...)
}

#' @rdname fcutils-deprecated
#' @export
aggr <- function(...) {
  .Deprecated("agg", package = "fcutils")
  agg(...)
}

#' @rdname fcutils-deprecated
#' @export
ytd_cum <- function(...) {
  .Deprecated("ytd_agg", package = "fcutils")
  ytd_agg(...)
}

#' @rdname fcutils-deprecated
#' @export
fytd_cum <- function(...) {
  .Deprecated("fytd_agg", package = "fcutils")
  fytd_agg(...)
}

#' @rdname fcutils-deprecated
#' @export
mtd_cum <- function(...) {
  .Deprecated("mtd_agg", package = "fcutils")
  mtd_agg(...)
}

#' @rdname fcutils-deprecated
#' @export
ptd_cum <- function(...) {
  .Deprecated("ptd_agg", package = "fcutils")
  ptd_agg(...)
}

#' @rdname fcutils-deprecated
#' @export
faggr <- function(...) {
  .Deprecated("fagg", package = "fcutils")
  fagg(...)
}
