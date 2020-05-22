#' Select Procedures from Professional Claim data
#' @details select procedure realted claims using CPT
#'     use in creating analytic file
#'
#' @param std_data_root profesisonal claim data location
#' @param professional_clm_data_name profesisonal claim data name; has to be csv format
#' @param cpt_map should include 3 variable: cpt_cd, e_proc_grp and e_proc_grp_lbl
#'
#' @return
#' @export
#'
#' @examples
select_procedure <- function(std_data_root = wd$std_data_root,
                             professional_clm_data_name = "prof_clm.csv",
                             cpt_map = define_proc_by_cpt) {
  # unique claim based on
  clm_distinct_vars <- c("member_id", "svc_start_dt", "svc_end_dt", "provider_npi", "cpt_cd", "cpt_mod")

  # read std pro claim data
  prof_clm <- fread((paste0(std_data_root, professional_clm_data_name)))

  # keep defined CPT code and
  # drop professional claim that don't have NPI
  prof_clm_select <- prof_clm[cpt_cd %in% cpt_map$cpt_cd &
    provider_npi != ""]

  # cpt mod wide to long format
  analytic_cptmod <- prof_clm_select %>%
    melt(
      measure = patterns("cpt_mod"),
      value.name = "cpt_mod"
    ) %>%
    .[, variable := NULL] %>%
    # selected unique key
    distinct(
      member_id,
      svc_start_dt,
      svc_end_dt,
      provider_npi,
      cpt_cd,
      cpt_mod
    )


  # keep cpt_mod code to one cell if it is has the same group_by info
  # e.g. if two claims have the same group_by listed vars, then two claims become one with two mod code
  analytic_cpt <- analytic_cptmod %>%
    group_by(
      member_id,
      svc_start_dt,
      svc_end_dt,
      provider_npi,
      cpt_cd
    ) %>%
    mutate(mod_n = row_number()) %>%
    pivot_wider(names_from = mod_n, values_from = cpt_mod, names_prefix = "mod") %>%
    unite("cpt_mod", starts_with("mod"), remove = TRUE, na.rm = TRUE, sep = ", ") %>%
    ungroup()

  # add cpt names label
  analytic_cpt <- analytic_cpt %>%
    left_join(cpt_map, by = "cpt_cd") %>%
    rename(
      dt_profsvc_start = svc_start_dt, # rename to distinguish from facility claim
      dt_profsvc_end = svc_end_dt,
      id_physician_npi = provider_npi
    )


  # check note ------
  # CPT_mod trasfer in sas had dropped the "80" to "8"
  # this will make assist surgeon definition differ
}
