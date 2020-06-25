# setup ---------
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(haven)
library(data.table)

# config
Brian_George_folder_loc <- medicareR::find_maize_folder()
wd <- medicareR::mk_data_dir_path(
  data_root = "sample",           # "sample", "full_data"
  george_file = Brian_George_folder_loc
)

analytic_phy = fread(paste0(wd$std_analytic_root, "analytic_phy.csv"))

analytic_6mon = comorbidity_6m(original_data = analytic_phy,
                                month = 6)

analytic_4mon = comorbidity_6m(original_data = analytic_phy,
                                month = 4)

min(analytic_4mon$dt_facclm_adm)


test_that("4m comorbidities variable names", {
  expect_equal(comorbidity_6m(original_data = analytic_phy,
                            month = 4) %>%
              select(contains("4m")) %>%
              names() %>%
              length(), 29)
})


test_that("5m comorbidities variable names", {
  expect_gte(comorbidity_6m(original_data = analytic_phy,
                               month = 5) %>%
                 summarize(min(dt_facclm_adm)) %>%
                 pull() %>%
               as_date(), as_date(paste0("2007-",(12-5),"-01")))
})

