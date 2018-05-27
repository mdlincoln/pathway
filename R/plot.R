#' Plot calculated pathway over original data
#'
#' @param m Original matrix.
#' @param p The results of [matrix_pathway].
#'
#' @export
plot_pathway <- function(m, p) {
  if (ncol(m) > 2) warning("Only the first two data dimensions will be plotted")

  plot(m, col = "gray", pch = 20)
  if (nrow(p$line) > 1)
    points(p$line, pch = 4)
  points(m[p$ni,], col = "pink", pch = 15)
  points(m[p$i,], col = "red", pch = 16)
  points(m[c(p$p1, p$p2),], col = "blue", pch = 15)
  segments(x0 = m[p$p1,1], y0 = m[p$p1,2], x1 = m[p$p2,1], y1 = m[p$p2,2])
}
