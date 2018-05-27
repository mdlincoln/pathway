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
#' @param p1 Integer. Row index of point at the start of the path
#' @param p2 Integer. Row index of point at the end of the path
#' @param n Integer. Number of intervening points to find
#' @param k Integer. Number of nearest neighbors to locate for each point on the
#'   ideal path, passed to [distances::nearest_neighbor_search] If `k > 1` then
#'   one real point per `n` will be randomly sampled from the `k` points
#'   returned by the search.
#'
#' @return A list with the following values
#' * `ax` Generated matrix of `n` rows with the same number of columns as `x`
#' * `match_indices`
#'
#' @export
matrix_pathway <- function(x, p1, p2, n = 4L, k = 1L, ...) {
  stopifnot(is.matrix(x))
  stopifnot(is.numeric(x))
  stopifnot(is.integer(p1))
  stopifnot(is.integer(p2))
  stopifnot(is.integer(n))
  stopifnot(is.integer(k))
  stopifnot(length(dim(x)) == 2)
  stopifnot(!anyNA(x))
  stopifnot(p1 != p2)
  stopifnot(p1 <= nrow(x))
  stopifnot(p2 <= nrow(x))
  stopifnot(n >= 1)
  stopifnot(n + 2 < nrow(x))

  # Generate an "ideal" vector of n points between p1 and p2
  artificial_vector <- mapply(seq, x[p1,], x[p2,], MoreArgs = list(length.out = n + 2))[2:(n + 1),]
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
    match_points <- unname(apply(dm, 2, sample, size = 1))
  } else {
    match_points <- dm[1,]
  }

  n_unique <- length(unique(match_points))
  if (n_unique < n) warning("Only ", n_unique, " points located.")

  list(
    ax = artificial_vector,
    match_indices = match_points
  )
}
