#' Limit which points are included in the nearest neighbor search
#'
#' When seeking the next point in a space, search only among those points that
#' fulfill a given function based on the previously-selected points.
#'
#' The supplied functions [navigate_ordered], [navigate_unique], and
#' [navigate_any] can be supplied directly to [pathway].
#'
#' [navigate] is a generator that takes an arbitrary user-supplied predicate
#' function that will generate a vector of search indices when given `x`, `pi`,
#' `p1`, and `p2`. This can be useful for drawing a path that, for example, only
#' moves forward in time, or which introduces a selection probability based on
#' attributes of the point supplied in another table.
#'
#' @param x The original matrix
#' @param pi Indices of already-selected-nodes. During the first step of a
#'   pathway, `pi` will be `NULL`
#' @param p1 Start and end points used in [pathway]
#' @param p2 Start and end points used in [pathway]
#' @param n Number of indices to be found
#' @param .p A function that returns a vector of indices
#' @param ... Additional arguments passed on to `.p`
#'
#' @return Integer. An integer vector of indices within `x` forming the search
#'   space for the next step of the nearest neighbor search.
#'
#' @export
#'
#' @examples
#'
#' m <- matrix(runif(1000), nrow = 500, ncol = 2)
#' obs_types <- sample(c("setosa", "versicolor", "virginica"), 500, replace = TRUE)
#'
#' # A custom predicate function must take the original matrix, the list of
#' # previously-selected pathway points, along with p1 and p2.
#' different_species <- function(x, pi, p1, p2, obs_types) {
#'   if (is.null(pi)) {
#'     search_space <- 1:nrow(x)
#'   } else {
#'     # Only search observations that do not have the same species as the immediately previous one.
#'     prev_type <- obs_types[tail(pi, 1)]
#'     search_space <- which(obs_types != prev_type)
#'   }
#'
#'   # Don't forget to exclude p1 and p2
#'   setdiff(search_space, c(p1, p2))
#' }
#'
#' p_species <- pathway(m, 2, 11, n = 8, navigator = navigate(different_species, obs_types))
navigate <- function(.p, ...) {
  function(x, pi, p1, p2, n) {
    .p(x, pi, p1, p2, ...)
  }
}

#' @describeIn navigate Select only points with indices following the ones
#'   already selected.
#' @export
navigate_ordered <- function(x, pi, p1, p2, n) {
  if (p2 < p1) stop("Cannot use navigate_ordered when p2 < p1")
  start <- max(max(pi, -Inf) + 1, p1 + 1)
  end <- p2 - 1 - (n - length(pi))
  if (start <= end) {
    following_points <- seq(from = start, to = end, by = 1)
    res <- as.integer(setdiff(following_points, c(p1, p2)))
    if (length(res > 0)) {
      return(res)
    }
  }
  warning("There are no remaining candidate points in this path. Returning p2")
  as.integer(p2)
}

#' @describeIn navigate Select only points with indices preceding the ones
#'   already selected.
#' @export
navigate_ordered_desc <- function(x, pi, p1, p2, n) {
  if (p2 > p1) stop("Cannot use navigate_ordered when p2 > p1")
  start <- min(min(pi, Inf) - 1, p1 - 1)
  end <- p2 + 1 + (n - length(pi))
  if (start >= end) {
    following_points <- seq(from = start, to = end, by = -1)
    res <- as.integer(setdiff(following_points, c(p1, p2)))
    if (length(res > 0)) {
      return(res)
    }
  }
  warning("There are no remaining candidate points in this path. Returning p2")
  as.integer(p2)
}

#' @describeIn navigate Select only points that have not yet been visited.
#' @export
navigate_unique <- structure(function(x, pi, p1, p2, n) {
  # This is a dummy function that isn't actually run by pathway()
  dummy_msg()
}, nav_class = "navigate_unique")

#' @describeIn navigate Select any point, even ones that have already been
#'   visited.
#' @export
navigate_any <- structure(function(x, pi, p1, p2, n) {
  # This is a dummy function that isn't actually run by pathway()
  dummy_msg()
}, nav_class = "navigate_any")

dummy_msg <- function() stop("This is a dummy function that shouldn't actually be evaluated. If you are seeing this, make sure you specify it in pathway() by name only, without using parentheses.")

