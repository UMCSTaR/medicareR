#' Elixhauser flags based on Dx ICD code using ahrq map
#' @description  main function is using ICD Package comorbid(, map = icd9_map_ahrq);
#'     icd9_map_ahrq for icd9; icd10_map_ahrq for icd10
#'     ICD package map reference: https://rdrr.io/cran/icd/man/icd9_map_ahrq.html
#'
#' @param original_data data to add on
#'
#' @return
#' @export
#'
#' @note ICD9/10 transition date is "2015-10-1"; icd10 is service date >= "2015-10-1"
#'
fac_dx_elix <- function(original_data) {
  analytic_elix_vars <- original_data %>%
    lazy_dt() %>%
    select(id, drg, dt_profsvc_start, starts_with("dx"))

  icd10 <- analytic_elix_vars %>%
    filter(dt_profsvc_start >= "2015-10-1") %>%
    as.data.table()

  icd9 <- analytic_elix_vars %>%
    filter(dt_profsvc_start < "2015-10-1") %>%
    as.data.table()

  if(nrow(icd9)>0){
  icd9_elix <- comorbid(icd9, map = icd9_map_ahrq, return_df = TRUE) %>%
    # as_tibble() %>%
    mutate_if(is.logical, as.numeric) %>%
    mutate(HTN_C = ifelse(HTN == 1 | HTNcx == 1, 1, 0)) %>%
    select(-HTN, -HTNcx)
  } else icd9_elix = tibble()

  if(nrow(icd10)>0){
  icd10_elix <- comorbid(icd10, map = icd10_map_ahrq, return_df = TRUE) %>%
    # as_tibble() %>%
    mutate_if(is.logical, as.numeric) %>%
    mutate(HTN_C = ifelse(HTN == 1 | HTNcx == 1, 1, 0)) %>%
    select(-HTN, -HTNcx)
  } else icd10_elix = tibble()

  analytic_elix_icd <- rbind(icd9_elix, icd10_elix) %>%
    mutate(id = as.numeric(id)) %>%
    arrange(id)

  # merge elix flg to original dataset
  analytic_elix <- original_data %>%
    lazy_dt() %>%
    left_join(analytic_elix_icd, by = "id") %>%
    as.data.table()

  # test if the icd9/10 maps generated comorbididty flags ----
  flg_by_year = analytic_elix %>%
    as_tibble() %>%
    select(facility_clm_yr, CHF:HTN_C) %>%
    rowwise() %>%
    mutate(tot = sum(CHF:HTN_C)) %>%
    ungroup() %>%
    summarise(perc_comorbidity = mean(tot)) # to remove warning, details: https://stackoverflow.com/questions/62140483/how-to-interpret-dplyr-message-summarise-regrouping-output-by-x-override

  if(any(flg_by_year$perc_comorbidity == 0)) {
    stop("comorbidity is not properly signed (icd9/10 to cmb flgs), some year don't have any pts have any comorbility flags")
  }

  # remove DX code; They were used to created comorbidity flags
  analytic_elix %>%
    select(-starts_with("DX"))
}
