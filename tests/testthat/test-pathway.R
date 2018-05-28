context("pathway")

set.seed(105)
rmat <- matrix(rnorm(100), 50, 2)
na_rmat <- rmat
na_rmat[c(1, 5, 7)] <- NA

describe("pathway", {
  it("rejects mismatched_inputs", {
    expect_error(pathway(1:10, 1, 2))
    expect_error(pathway(matrix(letters), 1, 2))
    expect_error(pathway(rmat, 1:2, 3))
    expect_error(pathway(rmat, 3, 1:2))
    expect_error(pathway(rmat, 1, 2, n = 2:5))
    expect_error(pathway(na_rmat, 1, 2))
    expect_error(pathway(rmat, 2, 2))
    expect_error(pathway(rmat, 101, 2))
    expect_error(pathway(rmat, 2, 101))
    expect_error(pathway(rmat, 1, 2, n = 102))
  })

  it("returns expected list", {
    n = 4
    res <- pathway(rmat, 1, 3, n = n)
    expect_is(res$line, "matrix")
    expect_equal(dim(res$line), c(n, ncol(rmat)))
    expect_equal(length(res$i), n)
    expect_equal(length(unique(res$i)), n)
    expect_equal(res$p1, 1)
    expect_equal(res$p2, 3)
  })
})
