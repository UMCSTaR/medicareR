#' Patient demographics
#' @description Add patient demographics to the analytic file
#' @details 1. Add dob, dod, gender and race
#'   2. Limit the patient population to age 65-99
#'
#' @param std_data_root data path to the membership info folder
#' @param original_data data name for the data you want to add the patient info on
#' @param filter_only_pt_65 TRUE: only keep patients ≥65 and ≤99; FLASE: no age limitation
#' @param year  select one year to process, e.g. 2007
#'
#' @export
#'
bene_info <- function(original_data,
                      std_data_root = wd$std_data_root,
                      filter_only_pt_65 = TRUE,
                      year) {
  # create member csv file name
  member_data_name = paste0("membership/member", year, ".csv")

  # validate input
  if (str_detect(member_data_name, ".csv")) {
    file_path = paste0(std_data_root, member_data_name)

    # check if file exist
    if(!all(file.exists(file_path))) stop(file_path, " doesn't exist")

    membership <- fread(file_path)
  } else if (str_detect(member_data_name, ".sas")) {
    membership <-
      haven::read_sas((paste0(std_data_root, member_data_name))) %>%
      # date format to match with fread inputs (otherwise the code to process membership has date format error)
      mutate_at(vars(contains("dt")), ~ as_date(., origin = "1960-01-01")) %>%
      mutate_at(vars(contains("dt")), ~ format(., "%d/%m/%Y"))
  }

  # setup lazy eval for dtplyr
  lazy_membership <- lazy_dt(membership)
  lazy_original_data <- lazy_dt(original_data)

  membership_process <- lazy_membership %>%
    mutate(
      zip_cd = str_sub(zip_cd, 1, 5),
      gender = as.numeric(gender),
      race = as.numeric(race),
      # gender
      # info: https://www.resdac.org/cms-data/variables/gender-code-claim
      flg_male = case_when(gender == 1 ~ 1,
                           gender == 2 ~ 0),
      # race_wbho: white, black, hispanic, other
      # https://www.resdac.org/cms-data/variables/beneficiary-race-code-encounter
      e_race_wbho = case_when(race == 5 ~ 3,!race %in% c(1, 2, 5) ~ 4,
                              TRUE ~ race)
    ) %>%
    mutate_at(vars(c("dob_dt", "dod_dt")), as_date)

  # add bene info to professional claim by member_id and member_yr
  dt = lazy_original_data %>%
    mutate_at(vars(starts_with("dt")), dmy) %>%
    mutate(member_yr = year) %>%
    left_join(membership_process, by = c("member_id", "member_yr")) %>%
    rename(dt_dob = dob_dt,
           dt_dod = dod_dt) %>%
    select(
      member_id,
      dt_profsvc_start,
      dt_profsvc_end,
      id_physician_npi,
      cpt_cd,
      cpt_mod,
      e_proc_grp,
      e_proc_grp_lbl,
      dt_dob,
      dt_dod,
      zip_cd,
      flg_male,
      e_race_wbho
    ) %>%
    as.data.table()


  # if only keep older patients ---------------------------------------------
  if (filter_only_pt_65 == TRUE) {
    # remove bene >99 and <65 yrs old
    dt[(dt_profsvc_end - dt_dob) / 365 <= 99 &
         (dt_profsvc_end - dt_dob) / 365 >= 65]
  } else if (filter_only_pt_65 == FALSE)
    dt

}
