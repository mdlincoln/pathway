context("matrix_pathway")

set.seed(105)
rmat <- matrix(rnorm(100), 50, 2)
na_rmat <- rmat
na_rmat[c(1, 5, 7)] <- NA

describe("matrix_pathway", {
  it("rejects mismatched_inputs", {
    expect_error(matrix_pathway(1:10, 1, 2))
    expect_error(matrix_pathway(matrix(letters), 1, 2))
    expect_error(matrix_pathway(rmat, 1:2, 3))
    expect_error(matrix_pathway(rmat, 3, 1:2))
    expect_error(matrix_pathway(rmat, 1, 2, n = 2:5))
    expect_error(matrix_pathway(rmat, 1, 2, n = 1, k = 2:5))
    expect_error(matrix_pathway(na_rmat, 1, 2))
    expect_error(matrix_pathway(rmat, 2, 2))
    expect_error(matrix_pathway(rmat, 101, 2))
    expect_error(matrix_pathway(rmat, 2, 101))
    expect_error(matrix_pathway(rmat, 1, 2, n = 102))
  })

  it("returns expected list", {
    n = 4
    k = 2
    res <- matrix_pathway(rmat, 1, 3, n = n, k = k)
    expect_warning(res2 <- matrix_pathway(rmat, 1, 2, n = n, k = k))
    expect_is(res$line, "matrix")
    expect_equal(dim(res$line), c(n, ncol(rmat)))
    expect_equal(length(res$i), n)
    expect_equal(length(unique(res$i)), n)
    expect_equal(dim(res$ni), c(k, n))
    expect_equal(res$p1, 1)
    expect_equal(res$p2, 3)
    expect_equal(length(res2$i), n)
    expect_equal(length(unique(res2$i)), 2)
  })
})
