# import data
import_mapping <- readr::read_csv("data/import_mapping.csv")

test_that("professional claim return dataset", {
  expect_s3_class(prof_clm_code(year = 2016,
                           schema = "prof_clm1",
                           data_file_name_clm = "bcarrier_2016_claims_not_real_data.csv",
                           src_root_clm = "data/",
                           mapping_data = import_mapping),
                  "data.table")
})

test_that("professional claim selected vars", {
  expect_equal(prof_clm_code(year = 2016,
                                schema = "prof_clm1",
                                data_file_name_clm = "bcarrier_2016_claims_not_real_data.csv",
                                src_root_clm = "data/",
                                mapping_data = import_mapping) %>%
                 names() %>%
                 sort(),
                  sort(c("member_id", "claim_id", "seq", "value", "code_type", "icd_version")))
})
