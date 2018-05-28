#' Plot calculated pathway over original data
#'
#' @param m Original matrix.
#' @param p The results of [matrix_pathway].
#' @param pca Logical. Run PCA transform before plotting?
#'
#' @export
plot_pathway <- function(m, p, pca = FALSE) {

  if (pca) {
    message("- Running PCA")
    prcm <- prcomp(rbind(m, p$line))
    plot_m <- prcm$x[1:nrow(m), 1:2]
    plot_line <- prcm$x[-(1:nrow(m)), 1:2]
  } else {
    if (ncol(m) > 2) warning("Only the first two data dimensions will be plotted. Suggest setting pca = TRUE")
    plot_m <- m[,1:2]
    plot_line <- p$line[,1:2]
  }

  plot_i <- p$i
  p1 <- p$p1
  p2 <- p$p2

  plot(plot_m, col = "gray", pch = 20)
  if (nrow(plot_line) > 1)
    points(plot_line, pch = 4)
  points(plot_m[c(p1, p2),], col = "blue", pch = 15)
  points(plot_m[plot_i,], col = "red", pch = 16)
  lines(plot_m[c(p1, plot_i, p2),])
}
