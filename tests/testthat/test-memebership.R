library(tidyverse)
library(data.table)

# import data
import_mapping <- readr::read_csv("../data/import_mapping.csv")

test_that("membership dataset", {
  expect_s3_class(membership(mapping_data = import_mapping,
                             year = 2007,
                             src_root = "../data/",
                             data_file_name = "demon07_not_real_data.csv",
                             schema = "membership"),
                  "data.table")
})
