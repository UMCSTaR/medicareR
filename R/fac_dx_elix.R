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
    select(id, drg, dt_profsvc_start, starts_with("dx"))

  icd10 <- analytic_elix_vars %>%
    filter(dt_profsvc_start >= "2015-10-1")

  icd9 <- analytic_elix_vars %>%
    filter(dt_profsvc_start < "2015-10-1")

  icd9_elix <- comorbid(icd9, map = icd9_map_ahrq, return_df = TRUE) %>%
    as_tibble() %>%
    mutate_if(is.logical, as.numeric) %>%
    mutate(HTN_C = ifelse(HTN == 1 | HTNcx == 1, 1, 0)) %>%
    select(-HTN, -HTNcx)

  icd10_elix <- comorbid(icd10, map = icd10_map_ahrq, return_df = TRUE) %>%
    as_tibble() %>%
    mutate_if(is.logical, as.numeric) %>%
    mutate(HTN_C = ifelse(HTN == 1 | HTNcx == 1, 1, 0)) %>%
    select(-HTN, -HTNcx)

  analytic_elix_icd <- rbind(icd9_elix, icd10_elix) %>%
    mutate(id = as.numeric(id)) %>%
    arrange(id)

  # merge elix flg to original dataset
  analytic_elix <- original_data %>%
    left_join(analytic_elix_icd, by = "id")
}
