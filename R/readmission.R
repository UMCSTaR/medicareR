#' Readmission Flags
#' @description    1. Find the 30-day readmission,  maybe multiple readmission
#'    2. Roll up to a single row per member_id & prof_calim_id
#'    3. Merge the 30-day readmission flag back to the main analytic file
#'
#' @inheritParams reoperation
#' @param fac_clm_name facility claim dataset name, ".csv"
#'
#' @return
#' @export
#'
#' @examples
readmission <- function(std_data_root,
                        fac_clm_name,
                        original_data) {
  message("reading fac_clm dataset...")
  fac_clm <- fread(paste0(std_data_root, fac_clm_name))

  readmit_id <- original_data %>%
    left_join(fac_clm %>% transmute(
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
    pull()

  original_data %>%
    mutate(flg_readmit_30d = ifelse(id %in% readmit_id, 1, 0))
}
