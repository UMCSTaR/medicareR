#' Diagnosis Code from Facility Claim
#' @description add facility code info to the analytic dataset
#'
#' @param original_data    data name for the data you want to add the info to
#' @param std_data_root    data path to the standardized data folder
#' @param year  select one year to process, e.g. 2007
#' @param max_year the most recent medicare year we have; for example, in 2020, we have Medpar processed up to 2018
#' @param fac_codes_folder folder name for fac_code
#' @param fac_clm_folder     folder name for fac_clm data (.csv format is a must)
#'
#' @return
#' @export
#'
#' @details 1. read std fac_clm and fac_clm_code data;
#'   2. join fac_clm and prof_clm by memberID and service date info.
#'Prof claim dates should be within facility claim dates
#'   3. then join fac_clm_code with analytic dataset by memberID and facility claim ID (attach dx code info)
#'   4. create admission_type(urgent/not), 30d death
#'   5. facility claim code are saved by year within fac_clm_code folder (due to data size)
#'

fac_dx <- function(original_data = analytic_demo,
                   std_data_root = wd$std_data_root,
                   year = 2007,
                   max_year = 2018,
                   fac_codes_folder = "fac_clm_code",
                   fac_clm_folder = "fac_clm")
{
  # read fac clm data ------
  # create file name
  message("reading fac_clm dataset year ", year)
  if (year<max_year){
    fac_clm_name = paste0("fac_clm_", c(year,year+1), ".csv")  # 2 years
  } else {
    fac_clm_name = paste0("fac_clm_", year, ".csv")  # 1 year, will exclude December cases
  }

  fac_clm_loc = file.path(std_data_root, fac_clm_folder, fac_clm_name)

  if(!all(file.exists(fac_clm_loc))){
    stop("path doesn't exist at ", fac_clm_loc)
  }

  # read data
  if(length(fac_clm_name)==1){
    fac_clm <- fread(fac_clm_loc, colClasses = 'character')  # max year has no follow up
  } else if(length(fac_clm_name)==2){
    # to get 30 days follow up
    fac_clm = map_df(fac_clm_loc, ~fread(.x, colClasses = 'character'))
  }

  # read fac_clm_code data ------
  facclm_dx = medicareR:::load_fac_code_data(std_data_root = std_data_root,
                                             fac_codes_folder = fac_codes_folder,
                                             year = year)

  # if year is not max year, add next year to get patient 30 days death info
  if(year<max_year){
    facclm_dx_next = medicareR:::load_fac_code_data(
      std_data_root = std_data_root,
      fac_codes_folder = fac_codes_folder,
      year = (year + 1)
    )

    facclm_dx = rbind(facclm_dx, facclm_dx_next, fill=TRUE)
    rm(facclm_dx_next)
  }


  # Add DRG and DX from MEDPAR to the analytic file:
  # preparing the data for calculating Elixhauser comorbidity flags

  # change date format
  original_data[, `:=`(dt_profsvc_start  = ymd(dt_profsvc_start),
                       dt_profsvc_end  = ymd(dt_profsvc_end))]


  analytic_fac <- original_data %>%
    lazy_dt(immutable = T) %>%
    left_join(fac_clm, by = "member_id") %>% # prof clm join with facility clm by member_id
    filter(
      dt_profsvc_start <= svc_end_dt & # make sure service date is within fac clm window
        dt_profsvc_start >= svc_from_dt &
        dt_profsvc_end <= svc_end_dt &
        dt_profsvc_end >= svc_from_dt &
        !is.na(svc_from_dt) & !is.na(svc_end_dt) # no missing facility service date (can't match with prof date if missing)
    ) %>%
    left_join(facclm_dx, by = c("member_id", "claim_id")) %>%   # join by fac clm id
    as_tibble()

  # los, mortality, admission type,-------
  analytic_fac <- analytic_fac %>%
    mutate_at(vars(contains("dt")), as_date) %>%
    mutate(

      # 30-day mortality after discharge
      flg_death_30d = ifelse(dt_dod - svc_end_dt >= 0 &
                               dt_dod - svc_end_dt <= 30, 1, 0),
      # missing date indicate no death
      flg_death_30d = ifelse(is.na(flg_death_30d), 0, flg_death_30d),

      # LOS
      val_los = svc_end_dt - svc_from_dt,

      # facility ID
      facility_npi,
      facility_prvnumgrp = as.numeric(facility_prvnumgrp),

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
    # use "id_physician_npi, dt_profsvc_start, cpt_cd" as unique key
    add_count(id_physician_npi, dt_profsvc_start, cpt_cd) %>%
    filter(n == 1) %>% # delete dup
    select(-n) %>%
    as.data.table()
}
