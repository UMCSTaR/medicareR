#' Readmission Flags
#' @description using facility claim data, left join by member id
#' @details    1. Find the 30-day readmission,  maybe multiple readmission
#'    2. Roll up to a single row per member_id & prof_calim_id
#'    3. Merge the 30-day readmission flag back to the main analytic file
#'
#' @inheritParams reoperation
#' @param fac_clm_name facility claim dataset name, ".csv"; you can use ".sas7bdat" data too.
#'      But it is much slower due to read_sas is slow for big data.
#'
#' @return
#' @export
#'
#' @examples
readmission <- function(std_data_root,
                        fac_clm_name,
                        original_data) {

  message("reading fac_clm dataset...")
  if (str_detect(fac_clm_name, ".csv")) {
    fac_clm <- fread(paste0(std_data_root, fac_clm_name))
  } else if (str_detect(fac_clm_name, ".sas7bdat")) {
    fac_clm <- haven::read_sas(paste0(std_data_root, fac_clm_name))
  } else {
    stop(fac_clm_name, " has to be.csv or sas data format")
  }

  readmit_id <- original_data %>%
    lazy_dt() %>%
    left_join(lazy_dt(fac_clm) %>% transmute(
      fac_claim_id = claim_id,
      member_id,
      svc_from_dt
    ),
    by = "member_id"
    ) %>%
    filter(
      fac_claim_id.x != fac_claim_id.y, # not the same admission
      as_date(svc_from_dt) - as_date(dt_facclm_adm) <= 30,
      as_date(svc_from_dt) - as_date(dt_facclm_adm) >= 0
    ) %>%
    distinct(id) %>%
    as_tibble() %>%
    pull()

  original_data[,flg_readmit_30d := ifelse(id %in% readmit_id, 1, 0)]
}
