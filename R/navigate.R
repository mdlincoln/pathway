#' Limit which points are included in the nearest neighbor search
#'
#' When seeking the next point in a space, search only among those points that
#' fulfill a given function based on the previously-selected points.
#'
#' This can be useful for drawing a path that, for example, only moves forward
#' in time, or which introduces a selection probability based on attributes of
#' the point supplied in another table.
#'
#' @param .p A function taking the following arguments
#'   * `x` The original matrix
#'   * `pi` Indices of already-selected-nodes
#'   * `p1`, `p2` Start and end points used in [pathway].
#'
#'   Must return an integer list of indices within `m` forming the search space
#'   for the next step of the nearest neighbor search.
#' @param ... Additional arguments passed on to `.p`
#'
#' @export
navigate <- function(.p, ...) {
  function(x, pi, p1, p2) {
    .p(x, pi, p1, p2, ...)
  }
}

#' @describeIn navigate Select only points with indices following the ones
#'   already selected.
#' @export
navigate_ordered <- function(x, pi, p1, p2) {
  if (p2 < p1) stop("Cannot use navigate_ordered when p2 < p1")
  start <- max(max(pi) + 1, p1 + 1)
  end <- min(p2, nrow(x))
  if (start < end) {
    following_points <- seq(from = start, to = end, by = 1)
    res <- setdiff(following_points, c(p1, p2))
    if (length(res > 0)) {
      return(res)
    }
  }
  warning("There are no remaining candidate points in this path. Returning p2")
  p2
}

#' @describeIn navigate Select only points that have not yet been visited.
#' @export
navigate_unique <- function(x, pi, p1, p2) {
  seq_len(nrow(x))[-c(pi, p1, p2)]
}

#' @describeIn navigate Select any point, even ones that have already been
#'   visited.
#' @export
navigate_any <- function(x, pi, p1, p2) {
  seq_len(nrow(x))[-c(p1, p2)]
}