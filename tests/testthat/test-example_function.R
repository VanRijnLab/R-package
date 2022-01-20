test_that("example_function() prints Hello world!", {
  expect_equal(example_function(1), "Hello world!")
})

test_that("example_function() prints nothing", {
  expect_equal(example_function(0), NULL)
})
