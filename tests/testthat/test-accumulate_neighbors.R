context("accumulate_neighbors")

m <- matrix(rnorm(100), nrow = 25, ncol = 4)
p1 <- 1
p2 <- 20
n <- nrow(m) - 2
ip <- ideal_points(m[p1, ], m[p2, ], n)
ai <- seq(nrow(m) + 1, nrow(m) + n)
bm <- rbind(m, ip)
mdist <- distances::distances(bm)

describe("accumulate_neighbors_unique", {
  res <- accumulate_neighbors_unique(m, mdist, p1, p2, ai)

  it("returns only unique points", {
    expect_equivalent(anyDuplicated(res), 0)
  })

  it("returns correct number of points", {
    expect_length(res, n)
  })

  it("does not return p1 or p2", {
    expect_false(p1 %in% res)
    expect_false(p2 %in% res)
  })
})

describe("accumulate_neighbors_any", {
  res <- accumulate_neighbors_any(m, mdist, p1, p2, ai)

  it("returns correct number of points", {
    expect_length(res, n)
  })

  it("does not return p1 or p2", {
    expect_false(p1 %in% res)
    expect_false(p2 %in% res)
  })
})
