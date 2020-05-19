library(tidyverse)
library(data.table)

# import data
import_mapping <- readr::read_csv("data/import_mapping.csv")

test_that("professional dataset", {
  expect_s3_class(
    professionals(
      mapping_data = import_mapping,
      year = 2010,
      src_root = "data/",
      data_file_name = "bcarrier_2007_line_not_real_data.csv",
      schema = "professionals1"
    ),
    "data.table"
  )
})
