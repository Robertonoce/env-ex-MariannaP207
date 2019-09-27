library(testthat)

context("anywhwere")

e0 <- list2env(list(t = 0))
e1 <- list2env(list(x = 1))
e2 <- list2env(list(t = 2))
t <- 3
parent.env(e0) <- e1
parent.env(e1) <- e2
# so: e0 is child of e1 is child of e2 is child of .GlobalEnv

test_that("anywhere has basic functionality", {
  expect_is(
    test1 <- anywhere("t", env = e0),
    "list"
  )
  expect_identical(
    test1,
    list(e0, e2, globalenv(), baseenv())
  )
  expect_identical(
    anywhere("t"),
    list(globalenv(), baseenv())
  )
})

test_that("anywhere deals with errors & failures", {
  expect_identical(
    anywhere("nothing_found", env = emptyenv()),
    list()
  )
  # these should be error messages you trigger via input checks:
  expect_error(
    anywhere(23, env = e0)
  )
  expect_error(
    anywhere(c("t", "x"), env = e0)
  )
  expect_error(
    anywhere("t", env = "env")
  )
})
