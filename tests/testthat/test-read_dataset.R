test_that("errors on invalid file path", {
  expect_error(read_dataset(abc))
})
test_that("results is dataset", {
  expect_message(read_dataset(), "Example dataset was used: textcues")
  data <- read_dataset()
  expect_true("userId" %in% colnames(data))
  expect_true("reactionTime" %in% colnames(data))
})
