% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot_pathway}
\alias{plot_pathway}
\title{Plot calculated pathway over original data}
\usage{
plot_pathway(m, p, pca = FALSE, name_all_points = FALSE,
  name_end_points = TRUE)
}
\arguments{
\item{m}{Original matrix.}

\item{p}{The results of \link{pathway}.}

\item{pca}{Logical. Run PCA transform before plotting?}

\item{name_all_points}{Logical. Add names of intermediate points to plot?}

\item{name_end_points}{Logical. Add names of end poits to plot?}
}
\description{
Plot calculated pathway over original data
}
