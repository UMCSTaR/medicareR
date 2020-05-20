# import data
import_mapping <- readr::read_csv("data/import_mapping.csv")

test_that("professional claim return dataset", {
  expect_s3_class(prof_clm(year = 2016,
                           schema = "prof_clm1",
                           data_file_name_clm = "bcarrier_2016_claims_not_real_data.csv",
                           data_file_name_ln = "bcarrier_2016_line_not_real_data.csv",
                           src_root_clm = "data/",
                           src_root_ln = "data/",
                           mapping_data = import_mapping),
                  "data.table")
})

