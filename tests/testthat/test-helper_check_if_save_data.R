test_that("choose not save", {
  expect_message(helper_check_if_save_data(save_or_not = "no"))
})


# test if data 1. ask to save data 2. data not on drive
test = dplyr::tibble(a = c(1:3),
                     b = rep("test", 3))

test_that("choose not save", {
  expect_message(
    helper_check_if_save_data(
      save_or_not = "yes",
      wd = "tests/testthat/data/",
      csv_name = "not_on_drive_data.csv",
      rdata_name = test
    ),
    "README: saved the"
  )
})

# to do; not sure how to test menu ---
# # test if data 1. ask to save data 2. data on drive
# test = dplyr::tibble(a = c(1:3),
#                      b = rep("test", 3))


# delete created file ----
file.remove("tests/testthat/data/not_on_drive_data.csv")
