library(tidyverse)
library(data.table)

# import data
import_mapping <- readr::read_csv("data/import_mapping.csv")

test_that("membership dataset rename", {
  expect_equal(membership(mapping_data = import_mapping,
                                        year = 2007,
                                        src_root = "data/",
                                        data_file_name = "demon07_not_real_data.csv",
                                        schema = "membership1") %>%
                    names(),
            import_mapping %>%
              filter(source_schema == "membership1") %>%
              pull(target_column))
})
