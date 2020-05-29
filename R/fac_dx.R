#' Diagnosis Code from Facility Claim
#' @description add facility code info to the analytic dataset
#'
#' @param std_data_root data path to the standardized data folder
#' @param fac_codes_folder folder name for fac_code
#' @param fac_clm_name  data name for fac_clm data
#' @param original_data data name for the data you want to add the info to
#'
#' @return
#' @export
#'
#' @details read std fac_clm and fac_clm_code data; join fac_clm and prof_clm by memberID
#'     and service date info; create admission_type(urgent/not), 30d death; attach dx code info
#'
#' @examples

fac_dx <- function(std_data_root = wd$std_data_root,
                   fac_codes_folder = "fac_clm_code",
                   fac_clm_name = "fac_clm.csv",
                   original_data = analytic_demo) {
  # read fac clm and fac clm code data ------
  message("reading fac_clm dataset...")
  fac_clm <- fread(paste0(std_data_root, fac_clm_name))

  # read fac clm and fac clm code data ------
  fac_codes_loc = paste0(std_data_root, fac_codes_folder, "/",
                         list.files(paste0(std_data_root, fac_codes_folder)))
  # read in all fac_code cross year
  # fac_code datasets were saved by year due to sample size too big
  facclm_dx_list = list()
  message("reading fac_clm_code dataset...")

  for (i in seq_along(fac_codes_loc)) {

    fac_clm_codes = fread(fac_codes_loc[i])

    # Facility claim codes file:
    # Transpose dx codes from long to wide format
    facclm_dx_by_yr <-
      fac_clm_codes[code_type == "DX"][, var_name := paste0(code_type, seq)] %>% #diagnosis code
      dcast(member_id + claim_id ~ var_name, value.var = c("value"))

    facclm_dx_list[[i]] = facclm_dx_by_yr
  }
  # combine as one dataset
  facclm_dx = rbindlist(facclm_dx_list, fill = T)

  # Add DRG and DX from MEDPAR to the analytic file:
  # preparing the data for calculating Elixhauser comorbidity flags

  analytic_fac <- original_data %>%
    left_join(fac_clm, by = "member_id") %>% # prof clm join with facility clm by member_id
    filter(
      dt_profsvc_start <= svc_end_dt & # make sure service date is within fac clm window
        dt_profsvc_start >= svc_from_dt &
        dt_profsvc_end <= svc_end_dt &
        dt_profsvc_end >= svc_from_dt &
        !is.na(svc_from_dt) & !is.na(svc_end_dt) # no missing facility service date (can't match with prof date if missing)
    ) %>%
    left_join(facclm_dx, by = c("member_id", "claim_id")) # join by fac clm id


  # los, mortality, admission type,-------
  analytic_fac <- analytic_fac %>%
    mutate_at(vars(contains("dt")), as_date) %>%
    mutate(

      # 30-day mortality after discharge
      flg_death_30d = ifelse(dt_dod - svc_end_dt >= 0 &
                               dt_dod - svc_end_dt <= 30, 1, 0),

      # LOS
      val_los = svc_end_dt - svc_from_dt,

      # facility ID
      facility_npi,
      facility_prvnumgrp,

      # /* Facility claim year */
      facility_clm_yr = ifelse(is.na(claim_yr), year(dt_profsvc_end), claim_yr),

      # admission type: 1-Emergency, 2-Urgent, 3-Elective, 4-Other, 9-Unknown/Missing
      # https://www.resdac.org/cms-data/variables/medpar-inpatient-admission-type-code
      e_admit_type = ifelse(admit_type_cd == "1", "1_Emergency", "9-Unknown/Missing"),
      e_admit_type = ifelse(admit_type_cd == "2", "2_Urgent", e_admit_type),
      e_admit_type = ifelse(admit_type_cd == "3", "3_Elective", e_admit_type),
      e_admit_type = ifelse(admit_type_cd %in% c("4", "5", "6", "7", "8"), "4_Other", e_admit_type)
    ) %>%
    rename(
      drg = drg_cd,
      fac_claim_id = claim_id,
      fac_claim_id = claim_id,
      dt_facclm_adm = svc_from_dt,
      dt_facclm_dschg = svc_end_dt
    ) %>%
    tibble::rowid_to_column(var = "id") %>%
    # select variables
    select(
      member_id, dt_profsvc_start, dt_profsvc_end, id_physician_npi, cpt_cd, cpt_mod,
      e_proc_grp, e_proc_grp_lbl, dt_dob, dt_dod, zip_cd, flg_male, e_race_wbho, drg,
      starts_with("dx"), id, fac_claim_id, dt_facclm_adm, dt_facclm_dschg, flg_death_30d, val_los,
      facility_npi, facility_prvnumgrp, facility_clm_yr, e_admit_type
    )

  # Since there are duplicated Medpar records mapped to professional claim, the below is to drop the duplicated records
  analytic_fac %>%
    add_count(id_physician_npi, dt_profsvc_start, cpt_cd) %>%
    filter(n == 1) %>% # delete dup
    select(-n)
}
