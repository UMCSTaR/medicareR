library(tidyverse)

# import data
import_mapping <- readr::read_csv("data/import_mapping.csv")
import_src <- readr::read_csv("data/import_source.csv")


test_that("import map return data frame", {
  expect_s3_class(import_map(schema = "facilities",
                             src_data = import_src,
                             mapping_data = import_mapping), "data.frame")
})
