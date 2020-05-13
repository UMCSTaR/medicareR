test_that("choose not save", {
  expect_message(helper_check_if_save_data(save_or_not = "no"))
})

