context("navigate")

set.seed(10)
nr <- 20
nc <- 5
x <- matrix(rnorm(n * k), nrow = nr, ncol = nc)
p1 <- 5
p2 <- 12
pi <- numeric(4)
pi2 <- c(5, 10, 0, 0)

navigate_any_res <- navigate_any(x, pi, p1, p2)
navigate_any_res2 <- navigate_any(x, p2, p1, p2)

describe("navigate_any", {
  it("Returns positive non-zero integer vector", {
    expect_gt(length(navigate_any_res), 0)
    expect_is(navigate_any_res, "integer")
    expect_true(all(navigate_any_res > 0))
    expect_gt(length(navigate_any_res2), 0)
    expect_is(navigate_any_res2, "integer")
    expect_true(all(navigate_any_res2 > 0))
  })

  it("Won't return points outside of matrix scope", {
    expect_true(all(navigate_any_res %in% 1:nrow(x)))
    expect_true(all(navigate_any_res2 %in% 1:nrow(x)))
  })

  it("Won't return p1 or p2 as options", {
    expect_false(p1 %in% navigate_any_res)
    expect_false(p1 %in% navigate_any_res)
    expect_false(p1 %in% navigate_any_res2)
    expect_false(p1 %in% navigate_any_res2)
  })

  it("Won't return any duplicate indices", {
    expect_equal(anyDuplicated(navigate_any_res), 0)
    expect_equal(anyDuplicated(navigate_any_res2), 0)
  })
})

context("navigate_unique")

navigate_unique_res <- navigate_unique(x, pi, p1, p2)
navigate_unique_res2 <- navigate_unique(x, pi2, p1, p2)

describe("navigate_any", {
  it("Returns positive non-zero integer vector", {
    expect_gt(length(navigate_unique_res), 0)
    expect_is(navigate_unique_res, "integer")
    expect_true(all(navigate_unique_res > 0))
    expect_gt(length(navigate_unique_res2), 0)
    expect_is(navigate_unique_res2, "integer")
    expect_true(all(navigate_unique_res2 > 0))
  })

  it("Won't return points outside of matrix scope", {
    expect_true(all(navigate_unique_res %in% 1:nrow(x)))
    expect_true(all(navigate_unique_res2 %in% 1:nrow(x)))
  })

  it("Won't return p1 or p2 as options", {
    expect_false(p1 %in% navigate_unique_res)
    expect_false(p1 %in% navigate_unique_res)
    expect_false(p1 %in% navigate_unique_res2)
    expect_false(p1 %in% navigate_unique_res2)
  })

  it("Won't return any duplicate indices", {
    expect_equal(anyDuplicated(navigate_unique_res), 0)
    expect_equal(anyDuplicated(navigate_unique_res2), 0)
  })

  it("Won't reuse any indices already in pi", {
    expect_false(any(pi %in% navigate_unique_res))
    expect_false(any(pi2 %in% navigate_unique_res2))
  })
})

context("navigate_ordered")

navigate_ordered_res <- navigate_ordered(x, pi, p1, p2)
navigate_ordered_res2 <- navigate_ordered(x, pi2, p1, p2)

describe("navigate_any", {
  it("Returns positive non-zero integer vector", {
    expect_gt(length(navigate_ordered_res), 0)
    expect_is(navigate_ordered_res, "integer")
    expect_true(all(navigate_ordered_res > 0))
    expect_gt(length(navigate_ordered_res2), 0)
    expect_is(navigate_ordered_res2, "integer")
    expect_true(all(navigate_ordered_res2 > 0))
  })

  it("Won't return points outside of matrix scope", {
    expect_true(all(navigate_ordered_res %in% 1:nrow(x)))
    expect_true(all(navigate_ordered_res2 %in% 1:nrow(x)))
  })

  it("Won't return p1 or p2 as options", {
    expect_false(p1 %in% navigate_ordered_res)
    expect_false(p1 %in% navigate_ordered_res)
    expect_false(p1 %in% navigate_ordered_res2)
    expect_false(p1 %in% navigate_ordered_res2)
  })

  it("Won't return any duplicate indices", {
    expect_equal(anyDuplicated(navigate_ordered_res), 0)
    expect_equal(anyDuplicated(navigate_ordered_res2), 0)
  })

  it("Won't reuse any indices already in pi", {
    expect_false(any(pi %in% navigate_ordered_res))
    expect_false(any(pi2 %in% navigate_ordered_res2))
  })

  it("Errors with invalid inputs", {
    expect_error(navigate_ordered(x, pi, 10, 5), "p2 < p1")
    expect_warning(res_w <- navigate_ordered(x, pi = c(6, 8, 12), p1, p2), "no remaining candidate points")
    expect_equal(res_w, p2, info = "When no remaining candidate points are found, return p2 only.")
  })
})
