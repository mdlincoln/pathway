context("ideal_points")

describe("ideal_points", {
  l <- 10
  p1 <- rnorm(l)
  p2 <- rnorm(l)
  n <- 5
  res <- ideal_points(p1, p2, n)

  it("returns matrix", {
    expect_is(res, "matrix")
    expect_equal(nrow(res), n)
    expect_equal(ncol(res), l)
  })

  it("rejects mismatched inputs", {
    expect_error(ideal_points(rnorm(2), rnorm(5), n))
  })

})