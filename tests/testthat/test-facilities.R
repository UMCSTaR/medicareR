library(tidyverse)
library(data.table)

# import data
import_mapping <- readr::read_csv("../data/import_mapping.csv")

test_that("facilities dataset", {
  expect_s3_class(facilities(mapping_data = import_mapping,
                             year = 2010,
                             src_root = "../data/",
                             data_file_name = "medpar2010_not_real_data.csv",
                             schema = "facilities1"),
                  "data.table")
})

