test_that("non-exist maize location", {
  your_loc = "/you_loc/George_Surgeon_Projects/"
  expect_error(find_maize_folder(your_BG_folder_loc = your_loc), your_loc)
})

