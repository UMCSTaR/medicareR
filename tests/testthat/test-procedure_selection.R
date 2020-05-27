# cpt define map
load("data/define_proc_sample.rdata")
test = define_proc_sample %>%
  dplyr::select(1:2)

prof_clm_data = medicareR::prof_clm_data

test_that("Check input cpt define dataset", {
  expect_error(procedure_selection(std_data_root = "data",
                                   professional_clm_data_name = "prof_clm_data.csv",
                                   cpt_map = test))
})
