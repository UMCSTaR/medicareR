#' social economic status based on zip code
#'
#' @param ses input ses dataset name
#' @param ses_zscore input ses dataset name; leave as NULL if you don't want to add Zscore
#' @param original_dataset data to add on
#'
#' @export

ses_info <- function(ses = ses,
                     ses_zscore = NULL,
                     original_dataset = analytic_elix) {
  ses <- ses %>%
    transmute(
      zip_cd = str_split_fixed(GEO_id, "US", n = 2)[, 2],
      val_mhhi = as.numeric(MHHI__HC02_EST_VC02_), # middle house hold income
      e_ses_quintile = ntile(val_mhhi, 5)
    ) %>%
    mutate(zip_cd = as.integer(zip_cd))

  analytic_ses <- left_join(original_dataset, ses, by = "zip_cd")

  if (!"NULL" %in% class(ses_zscore)){
    analytic_ses <- left_join(analytic_ses,
                            ses_zscore %>%
                              select(zip_cd, starts_with("e_ses_")) %>%
                              mutate(zip_cd = as.integer(zip_cd)),
                            by = "zip_cd")
  }

  analytic_ses
}
