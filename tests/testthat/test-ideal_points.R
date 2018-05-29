context("ideal_points")

describe("ideal_points", {
  l <- 10
  l1 <- 1
  p1 <- rnorm(l)
  p1l1 <- rnorm(l1)
  p2 <- rnorm(l)
  p2l1 <- rnorm(l1)
  n <- 5
  n1 <- 1
  res <- ideal_points(p1, p2, n)
  res1 <- ideal_points(p1l1, p2l1, n1)

  it("returns matrix", {
    expect_is(res, "matrix")
    expect_true(is.numeric(res))
    expect_equal(nrow(res), n)
    expect_equal(ncol(res), l)
    expect_is(res1, "matrix")
    expect_true(is.numeric(res1))
    expect_equal(nrow(res1), n1)
    expect_equal(ncol(res1), l1)
  })

  it("rejects mismatched inputs", {
    expect_error(ideal_points(rnorm(2), rnorm(5), n))
    expect_error(ideal_points(rnorm(2), rnorm(5), n1))
  })

  it("only returns values bounded within the original points", {
    between <- function(x, a, b) {
      x >= min(a, b) & x <= max(a, b)
    }
    expect_true(all(apply(res, 1, function(x) mapply(between, x, p1, p2))))
    expect_true(all(apply(res1, 1, function(x) mapply(between, x, p1l1, p2l1))))
  })
})