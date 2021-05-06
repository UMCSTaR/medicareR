#' Severe Complication flags
#'
#' @param perc percentage cut of point for length of stay considered severe
#'
#' @export
#' @return
#'
#'
severe_cmp_flags <- function(original_data,
                             perc = 0.75) {
  original_data %>%
    lazy_dt() %>%
    group_by(e_proc_grp) %>%
    mutate(
      val_los = as.numeric(val_los),

      flg_cmp_po_severe = ifelse(
        flg_cmp_po_any == 1 &
          val_los > quantile(val_los, perc, na.rm = T),
        1,
        0
      ),
      flg_cmp_po_severe_not_poa = case_when(
        flg_cmp_po_any_not_poa == "1" & val_los > quantile(val_los, perc, na.rm = T) ~ "1",
        facility_clm_yr < 2010 ~ "N/A (no var)",
        TRUE ~ "0"
      )
    ) %>%
    ungroup() %>%
    as.data.table()
}
