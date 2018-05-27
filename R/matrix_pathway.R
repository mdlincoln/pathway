#' Find a pathway of nearest neighbors between two points in a multidimensional
#' matrix
#'
#' Given two points in a matrix `p1` and `p2`, find the points nearest to an ideal path between them.
#'
#' @param x A two-dimensional numeric matrix.
#' @param p1 Integer. Row index of point at the start of the path
#' @param p2 Integer. Row index of point at the end of the path
#' @param n Integer. Number of intervening
#'
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
  stopifnot(n + 2 < nrow(x))

  artificial_vector <- mapply(seq, x[p1,], x[p2,], MoreArgs = list(length.out = n + 2))
  artificial_indices <- seq(nrow(x) + 1, nrow(x) + n)
  unsearchable_indices <- c(p1, p2, artificial_indices)
  merged_x <- rbind(x, artificial_vector[seq(2, n + 1), ])
  searchable_indices <- seq_len(nrow(merged_x))[-unsearchable_indices]

  dd <- distances::distances(merged_x, ...)
  dm <- distances::nearest_neighbor_search(dd, k = k, query_indices = artificial_indices, search_indices = searchable_indices)
  match_points <- apply(dm, 2, function(x) sample(x, 1))

  list(
    x = merged_x,
    match_points = match_points,
    p1 = p1,
    p2 = p2,
    a = artificial_indices
  )
}