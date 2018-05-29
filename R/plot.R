#' Plot calculated pathway over original data
#'
#' @param m Original matrix.
#' @param p The results of [pathway].
#' @param pca Logical. Run PCA transform before plotting?
#'
#' @import graphics
#'
#' @export
plot_pathway <- function(m, p, pca = FALSE) {

  if (pca) {
    message("- Running PCA")
    prcm <- stats::prcomp(rbind(m, p$line))
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

  if (is.null(rownames(plot_m))) {
    plot_labels <- plot_i
  } else {
    plot_labels <- rownames(plot_m)[plot_i]
  }

  graphics::plot(plot_m, col = "gray", pch = 20)
  if (nrow(plot_line) > 1)
    graphics::points(plot_line, pch = 4)
  graphics::points(plot_m[c(p1, p2),], col = "blue", pch = 15)
  graphics::points(plot_m[plot_i,], col = "red", pch = 16)
  graphics::text(plot_m[plot_i,], labels = plot_labels, pos = 3)
  graphics::lines(plot_m[c(p1, plot_i, p2),])
}
