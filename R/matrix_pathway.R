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
#' @param k Integer. Number of nearest neighbors to locate for each point on the
#'   ideal path, passed to [distances::nearest_neighbor_search] If `k > 1` then
#'   one real point per `n` will be randomly sampled from the `k` points
#'   returned by the search.
#' @param ... Additional arguments passed to [distances::distances].
#'
#' @return A list with the following values
#' * `line` `n` points along a line between `p1` and `p2`.
#' * `i` Row indices of `x` indicating matched points.
#' * `ni` Indices of nearest neighbors in `x` found for for each point in `line`.
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
#' matrix_pathway(m, 2L, 11L, k = 1L)
matrix_pathway <- function(x, p1, p2, n = 4L, k = 1L, ...) {
  assert_that(is.matrix(x))
  assert_that(is.numeric(x))
  assert_that(is.count(p1))
  assert_that(is.count(p2))
  assert_that(is.count(n))
  assert_that(is.count(k))
  assert_that(length(dim(x)) == 2)
  assert_that(!anyNA(x))
  assert_that(p1 != p2, msg = "p1 and p2 must be different points")
  assert_that(p1 <= nrow(x))
  assert_that(p2 <= nrow(x))
  assert_that(n + 2 < nrow(x), msg = "n must be larger than 2 + the number of matrix rows")

  # Generate an "ideal" vector of n points between p1 and p2
  artificial_vector <- ideal_points(a = x[p1,], b = x[p2,], n = n)
  artificial_indices <- seq(nrow(x) + 1, nrow(x) + n)
  unsearchable_indices <- c(p1, p2, artificial_indices)

  # Add this ideal vector into the original matrix
  merged_x <- rbind(x, artificial_vector)
  searchable_indices <- seq_len(nrow(merged_x))[-unsearchable_indices]

  # Locate k nearest neighbors to the points in the ideal vector
  dd <- distances::distances(merged_x)
  dm <- distances::nearest_neighbor_search(dd, k = k, query_indices = artificial_indices, search_indices = searchable_indices)

  # Sample one nearest neighbor for each point of the ideal vector
  if (k > 1) {
    match_points <- apply(dm, 2, sample, size = 1)
  } else {
    match_points <- dm[1,]
  }

  n_unique <- length(unique(match_points))
  if (n_unique < n) warning("Only ", n_unique, " unique intermediate points located. Try increasing k or decreasing n.")

  list(
    line = artificial_vector,
    i = unname(match_points),
    ni = dm,
    p1 = p1,
    p2 = p2
  )
}

# Helper function to find points along a line between two points
ideal_points <- function(a, b, n) {
  stopifnot(length(a) == length(b))
  mapply(seq, a, b, MoreArgs = list(length.out = n + 2))[2:(n + 1),]
}
