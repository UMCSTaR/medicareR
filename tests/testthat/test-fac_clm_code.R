# import data
import_mapping <- readr::read_csv("data/import_mapping.csv")
import_src <- readr::read_csv("data/import_source.csv")


test_that("facility claim code dataset", {
  expect_s3_class(
    fac_clm_code(
      mapping_data = import_mapping,
      year = 2010,
      src_root = "data/",
      data_file_name = "medpar2010_not_real_data.csv",
      schema = "fac_clm2"
    ),
    "data.table"
  )
})

