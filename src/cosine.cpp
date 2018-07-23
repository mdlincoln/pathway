#include <Rcpp.h>
using namespace Rcpp;

IntegerVector cwhich(int x, LogicalVector p) {
  IntegerVector indices = seq_len(x);
  IntegerVector out = indices[p];
  return out;
}

// Measure the cosine similarity of two numeric vectors of equal length
// [[Rcpp::export]]
double cosine_distance(NumericVector x, NumericVector y) {
  int n = x.size();
  double dot = 0.0, a = 0.0, b = 0.0;
  for (int i = 0; i < n; ++i) {
    dot += x[i] * y[i];
    a += pow(x[i], 2.0);
    b += pow(y[i], 2.0);
  }
  return 1 - (dot / (sqrt(a) * sqrt(b)));
}

// Given a numeric matrix x and a row index a, find the cosine similarity of
// every other row in x to row a.
// [[Rcpp::export]]
NumericVector rowwise_cosine_dist(NumericMatrix x, int a) {
  int nrowx = x.nrow();
  // Output length matches the numer of rows in x - a
  NumericVector cos_dist(nrowx);
  NumericVector av = x(a, _);

  for (int i = 0; i < nrowx; i++) {
    // check for interrupt every 1000 iterations
    if (i % 1000 == 0)
      checkUserInterrupt();
    NumericVector iv = x(i, _);
    cos_dist[i] = cosine_distance(av, iv);
  }

  return cos_dist;
}

List ideal_midpoint(NumericVector p1_dist, NumericVector p2_dist, int b) {
  // Pull the specific distance between points a and b
  double base_dist = p1_dist[b];
  double half_dist = base_dist / 2;

  LogicalVector candidatesl = p1_dist < half_dist & p2_dist < half_dist;
  IntegerVector candidates = cwhich(candidatesl.size(), candidatesl);

  // The ideal point is whichever one has the smallest total distance between
  // both p1 and p2
  NumericVector p1_candidates = p1_dist[candidates];
  NumericVector p2_candidates = p2_dist[candidates];

  NumericVector adist_ideal = abs(p1_candidates - half_dist);
  NumericVector bdist_ideal = abs(p2_candidates - half_dist);
  int ideal_min = candidates[which_min(adist_ideal + bdist_ideal)];

  List res;
  res["mid"] = ideal_min;
  res["candidates"] = candidates;
  return res;
}

// Given a matrix x and two row indices a and b, return the ideal midpoint
// [[Rcpp::export]]
int cos_midpoint(NumericMatrix x, int a, int b) {
  // For every other point in the matrix, calculate the distances to a and b
  NumericVector p1_dist = rowwise_cosine_dist(x, a);
  NumericVector p2_dist = rowwise_cosine_dist(x, b);

  List mid = ideal_midpoint(p1_dist, p2_dist, b);
  return mid["mid"];
}

// [[Rcpp::export]]
int midpoint(NumericMatrix x, int a, int b) {
  return cos_midpoint(x, a - 1, b - 1);
}

// [[Rcpp::export]]
IntegerVector cpath(NumericMatrix x, int a, int b) {
  // Add absolute names to the matrix
  x.names() = seq_len(x.nrow());

  IntegerVector out(3);
  // Shift indices to 0-start
  a -= 1, b -= 1;

  int midpoint = cos_midpoint(x, a, b)["mid"];

  out[0] = a;
  out[1] = midpoint;
  out[2] = b;

  // Adjust indices for R-style 1-based indexing
  out = out + 1;

  return out;
}

// NumericMatrix namedex(NumericMatrix x, CharacterVector i) {
//   IntegerVector subi = cwhich(rownames(x) %in% i);
//   NumericMatrix out = x(subi, Range(1, x.ncol()));
//   return out;
// }

/*** R
set.seed(1)
x <- 2
y <- 100000
#baz <- embeddings
#pbaz <- prcomp(baz)$x[,1:2]

a <- sample.int(nrow(baz), 1)
b <- sample.int(nrow(baz), 1)

mid1 <- midpoint(baz, a, b)
mid2 <- midpoint(baz, a, mid1)
mid3 <- midpoint(baz, b, mid1)
mid4 <- midpoint(baz, a, mid2)
mid5 <- midpoint(baz, mid1, mid2)
mid6 <- midpoint(baz, b, mid3)
mid7 <- midpoint(baz, mid1, mid3)

mids <- c(mid1, mid2, mid3, mid4, mid5, mid6, mid7) + 1


plot(pbaz, col = grDevices::rgb(red = 0.75, green = 0.75, blue = 0.75, alpha = 0.1), pch = 20)
points(pbaz[c(a, b), , drop = FALSE], col = "blue", pch = 15)
points(pbaz[mids, , drop = FALSE], col = "red", pch = 15)

segments(x0 = 0, y0 = 0, x1 = pbaz[c(a, b, mids), 1], y1 = pbaz[c(a, b, mids), 2])
*/
