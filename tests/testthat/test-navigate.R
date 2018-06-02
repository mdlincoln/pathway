set.seed(10)
nr <- 20
nc <- 5
x <- matrix(rnorm(nr * nc), nrow = nr, ncol = nc)
n <- 5
p1 <- 5
p2 <- 12
pi <- numeric(4)
pi2 <- c(5, 10, 0, 0)

context("navigate_any")

describe("navigate_any", {
  it("Returns error if actually called", {
    expect_error(navigate_any(x, pi, p1, p2, n))
  })

  it("Has correct nav_class", {
    expect_equivalent(attr(navigate_any, "nav_class"), "navigate_any")
  })
})

context("navigate_unique")

describe("navigate_unique", {
  it("Returns error if actually called", {
   expect_error(navigate_unique(x, pi, p1, p2, n))
  })

  it("Has correct nav_class", {
    expect_equivalent(attr(navigate_unique, "nav_class"), "navigate_unique")
  })
})

context("navigate_ordered")

navigate_ordered_res <- navigate_ordered(x, pi, p1, p2, n)
expect_warning(navigate_ordered_res2 <- navigate_ordered(x, pi2, p1, p2, n))

describe("navigate_ordered", {
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

  it("Returned indices are all higher than the higest in pi", {
    expect_gt(min(navigate_ordered_res), max(pi))
    expect_gt(min(navigate_ordered_res2), max(pi2))
  })

  it("Errors with invalid inputs", {
    expect_error(navigate_ordered(x, pi, 10, 5, n), "p2 < p1")
    expect_warning(res_w <- navigate_ordered(x, pi = c(6, 8, 12), p1, p2, n), "no remaining candidate points")
    expect_equal(res_w, p2, info = "When no remaining candidate points are found, return p2 only.")
  })
})
