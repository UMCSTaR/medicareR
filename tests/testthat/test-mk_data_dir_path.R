test_that("wd file exists", {
  if(!file.exists("/Volumes/George_Surgeon_Projects")){
  skip('Not connected to maize /Volumes/George_Surgeon_Projects')}
  expect_error(mk_data_dir_path(data_root = "sampl",
                                medicare_file = "Denom_MBSF",
                                george_file = "/Volumes/George_Surgeon_Projects"),
               "data_root input value has to be: sample or full_data")
})
