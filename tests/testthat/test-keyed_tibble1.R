library(tibble)

test_that("new_keyed_tibble1 works as expected", {
  # Class output is what was expected:
  expect_equal(class(new_keyed_tibble1(tibble(x = 1:5), "x")),
               c("keyed_tibble1", "tbl_df", "tbl", "data.frame"))

  # Type validation is handled:
  expect_error(new_keyed_tibble1(data.frame(x = 1:5), "x"))
  expect_error(new_keyed_tibble1(new_keyed_tibble1(tibble(x = 1:5), "x"), "x"))
  expect_error(new_keyed_tibble1(tibble(x = 1:5), 12345))

  # Non-type validation isn't handled in this constructor:
  expect_no_error(new_keyed_tibble1(tibble(x = 1:5), "y"))
  expect_no_error(new_keyed_tibble1(tibble(x = c(1,1,2,3)), "x"))
})

test_that("ensure_keyed_tibble1 works as expected", {
  # Successful output is what was expected:
  expect_equal(ensure_new_keyed_tibble1(tibble(x = 1:5), "x"),
               new_keyed_tibble1(tibble(x = 1:5), "x"))

  intermediate_result <- new_keyed_tibble1(tibble(x = 1:5), "x")
  class(intermediate_result) <- c("grouped_df", class(intermediate_result))
  expect_equal(ensure_new_keyed_tibble1(intermediate_result, "x"),
               `class<-`(new_keyed_tibble1(tibble(x = 1:5), "x"),
                         c("keyed_tibble1", "grouped_df", "tbl_df", "tbl", "data.frame")))

  expect_equal(ensure_new_keyed_tibble1(new_keyed_tibble1(tibble(x = 1:5, y = 1:5), "x"), "y"),
               new_keyed_tibble1(tibble(x = 1:5, y = 1:5), "y"))

  # Type validation is handled:
  expect_error(ensure_new_keyed_tibble1(data.frame(x = 1:5), "x"))
  expect_error(ensure_new_keyed_tibble1(tibble(x = 1:5), 12345))

  # Non-type validation isn't handled in this constructor:
  expect_no_error(ensure_new_keyed_tibble1(tibble(x = 1:5), "y"))
  expect_no_error(ensure_new_keyed_tibble1(tibble(x = c(1,1,2,3)), "x"))
})

# TODO error classes and/or snapshots
