#' social economic status based on zip code
#' @details join by bene zip code
#'
#' @param ses input ses dataset name
#' @param ses_zscore input ses dataset name; leave as NULL if you don't want to add Zscore
#' @param original_dataset data to add on
#'
#' @export
ses_info <- function(original_dataset = analytic_elix,
                     ses = ses,
                     ses_zscore = NULL) {
  ses <- ses %>%
    transmute(
      zip_cd = str_split_fixed(GEO_id, "US", n = 2)[, 2],
      # middle house hold income
      val_mhhi = suppressWarnings(as.numeric(MHHI__HC02_EST_VC02_)),
      # MHHI__HC02_EST_VC02_ has character value, but we only need number here,
      # so the warning is suppressed.
      e_ses_quintile = ntile(val_mhhi, 5),
      zip_cd = as.character(zip_cd)
    )

  original_dataset = original_dataset %>% mutate(zip_cd = as.character(zip_cd),
                                                 zip_cd = str_pad(zip_cd, 5, side =
                                                                    "left", pad = "0"))

  # left join
  analytic_ses = merge.data.table(original_dataset, ses, by = "zip_cd", all.x = TRUE)

  if (!"NULL" %in% class(ses_zscore)) {
    ses_zscore = ses_zscore %>%
      select(zip_cd, starts_with("e_ses_")) %>%
      mutate(zip_cd = as.character(zip_cd))

    # left join
    analytic_ses <- merge.data.table(analytic_ses,
                                     ses_zscore,
                                     by = "zip_cd", all.x = TRUE)
  }

  setDT(analytic_ses)
}
