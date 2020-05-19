library(tidyverse)
library(data.table)

# import data
import_mapping <- readr::read_csv("data/import_mapping.csv")

test_that("facility claim return data table", {
  expect_s3_class(fac_clm(mapping_data = import_mapping,
                             year = 2010,
                             src_root = "data/",
                             data_file_name = "medpar2010_not_real_data.csv",
                             schema = "fac_clm2"),
                  "data.table")
})


test_that("facility claim only include selected vars", {
  expect_equal(fac_clm(mapping_data = import_mapping,
                          year = 2010,
                          src_root = "data/",
                          data_file_name = "medpar2010_not_real_data.csv",
                          schema = "fac_clm2") %>%
                 names(),
               import_mapping %>%
                 filter(source_schema == "fac_clm2",
                        target_column %in% c(
                          "member_id",
                          "claim_id",
                          "claim_yr",
                          "facility_npi",
                          "facility_prvnumgrp",
                          "svc_from_dt",
                          "svc_end_dt",
                          "dischg_disp_cd",
                          "dischg_status_cd",
                          "admit_type_cd",
                          "claim_type_cd",
                          "primary_pyr_cd",
                          "drg_cd",
                          "admit_dx",
                          "admit_dx_vrsn",
                          "icd_dx_cnt",
                          "icd_pr_cnt",
                          "claim_line_id"
                        )) %>%
                 pull(target_column)
                  )
})

