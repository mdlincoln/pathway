#' Find a pathway of nearest neighbors between two points in a multidimensional
#' matrix
#'
#' Given two points in a matrix `p1` and `p2`, find the points nearest to an
#' ideal path between them.
#'
#' By default `k` is 1, and will return the closest possible points to the ideal
#' line between `p1` and `p2`. Setting k to a higher number will mean points are
#' sampled from a larger range.
#'
#' @param x A two-dimensional numeric matrix.
#' @param p1 Integer. Row index of point at the start of the path.
#' @param p2 Integer. Row index of point at the end of the path.
#' @param n Integer. Number of intervening points to find.
#' @param navigator Function for constraining nearest neighbor search based on
#'   previously-selected steps.. See [navigate] for more details.
#' @param ... Additional arguments passed to [distances::distances].
#' @param verbose Display progress messages.
#'
#' @return A list with the following values
#' * `line` `n` points along a line between `p1` and `p2`.
#' * `i` Row indices of `x` indicating matched points.
#' * `p1`, `p2` Inidices of points origially passed in.
#'
#' @import assertthat
#'
#' @export
#' @examples
#' set.seed(10)
#' m <- matrix(runif(1000), nrow = 500, ncol = 2)
#' m[2,]
#' m[5,]
#' pathway(m, 2, 11)
pathway <- function(x, p1, p2, n = 4L, navigator = navigate_unique, ..., verbose = FALSE) {
  assert_that(is.matrix(x))
  assert_that(is.numeric(x))
  assert_that(is.count(p1))
  assert_that(is.count(p2))
  assert_that(is.count(n))
  assert_that(length(dim(x)) == 2)
  assert_that(!anyNA(x))
  assert_that(p1 != p2, msg = "p1 and p2 must be different points")
  assert_that(p1 <= nrow(x))
  assert_that(p2 <= nrow(x))
  assert_that(n + 2 < nrow(x), msg = "n must be larger than 2 + the number of matrix rows")

  # Generate an "ideal" list of n points between p1 and p2
  if (verbose) message("- Adding ideal points into original matrix")
  artificial_vector <- ideal_points(a = x[p1,], b = x[p2,], n = n)
  artificial_indices <- seq(nrow(x) + 1, nrow(x) + n)

  # Add this ideal vector into the original matrix
  merged_x <- rbind(x, artificial_vector)

  # Locate k nearest neighbors to the points in the ideal vector
  if (verbose) message("- Nearest neighbor search")
  x_distances <- distances::distances(merged_x)
  nn_results <- accumulate_neighbors(x, x_distances, p1, p2, artificial_indices, navigator, verbose)

  list(
    line = unname(artificial_vector),
    i = unname(nn_results),
    p1 = p1,
    p2 = p2
  )
}

# This wrapper checks for special cases of naviagator functions and dispatches
# the proper handlers
accumulate_neighbors <- function(x, x_distances, p1, p2, artificial_indices, navigator, verbose) {
  if (attr(navigator, "nav_class", exact = TRUE) == "navigate_any") {
    accumulate_neighbors_any(x, x_distances, p1, p2, artificial_indices, verbose)
  } else if (attr(navigator, "nav_class", exact = TRUE) == "navigate_unique") {
    accumulate_neighbors_unique(x, x_distances, p1, p2, artificial_indices, verbose)
  } else {
    accumulate_neighbors_custom(x, x_distances, p1, p2, artificial_indices, navigator, verbose)
  }
}

# Loop along ideal points defined in artificial_indices and build a set of
# nearest neighbor search results. The navigator function establishes limits on
# the search indices passed to [distances::nearest_neighbor_search]
accumulate_neighbors_custom <- function(x, x_distances, p1, p2, artificial_indices, navigator, verbose) {
  n <- length(artificial_indices)
  # Construct an empty container to hold results
  container <- NULL
  for (i in seq_len(n)) {
    search_space <- navigator(x, pi = container, p1, p2, n)
    if (!length(search_space) > 0)
      stop("Search space must have at least one possible number in it.")
    candidate <- distances::nearest_neighbor_search(
      x_distances, k = 1L,
      query_indices = artificial_indices[i],
      search_indices = search_space)
    if (verbose) message(candidate)
    container <- c(container, candidate[1,1])
  }
  container
}

# Directly calls distances::nearest_neighbor_search, excluding p1 and p2 from
# the search indices
accumulate_neighbors_any <- function(x, x_distances, p1, p2, artificial_indices, ...) {
  candidates <- distances::nearest_neighbor_search(
    x_distances, k = 1L,
    query_indices = artificial_indices,
    search_indices = seq_len(nrow(x))[-c(p1, p2)])

  unname(candidates[1,])
}

# Special case of accumulating only unique neighbors
accumulate_neighbors_unique <- function(x, x_distances, p1, p2, artificial_indices, ...) {
  # For each ideal point, find as many nearest neighbors as total requested
  # points
  candidates <- distances::nearest_neighbor_search(
    x_distances, k = length(artificial_indices),
    query_indices = artificial_indices,
    search_indices = seq_len(nrow(x))[-c(p1, p2)])

  # Keep the closes results for each point that is not yet used in the results
  results <- integer(length(artificial_indices))
  for (i in seq_along(results)) {
    results[i] <- setdiff(candidates[,i], results)[1]
  }
  results
}

# Helper function to find points along a line between two points
ideal_points <- function(a, b, n) {
  stopifnot(length(a) == length(b))
  as.matrix(mapply(seq, a, b, MoreArgs = list(length.out = n + 2))[2:(n + 1),])
}
