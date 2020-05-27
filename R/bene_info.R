#' Patient demographics
#' @description Add patient demographics to the analytic file
#' @details Add dob, dod, gener and race; Limit the patient population to age 65-99
#'
#' @param data_loc data path to the membership info folder
#' @param data_name data name for the membership dataset
#' @param original_data data name for the data you want to add the patient info to
#'
#' @export
#'
bene_info <- function(data_loc,
                      data_name,
                      original_data) {

  if (!str_detect(data_name, ".csv")){
    stop("data_name has to be .csv format")
  }

  membership <- fread((paste0(data_loc, data_name)))

  membership_process <- membership %>%
    mutate(
      zip_cd = str_sub(zip_cd, 1, 5),
      # gender
      # info: https://www.resdac.org/cms-data/variables/gender-code-claim
      gender = as.numeric(gender),
      flg_male = ifelse(gender == 1, 1, gender),
      flg_male = ifelse(gender == 2, 0, gender),
      # race_wbho: white, black, hispanic, other
      # https://www.resdac.org/cms-data/variables/beneficiary-race-code-encounter
      race = as.numeric(race),
      e_race_wbho = ifelse(race == 5, 3, race),
      e_race_wbho = ifelse(!race %in% c(1, 2, 5), 4, e_race_wbho)
    ) %>%
    mutate_at(vars(c("dob_dt","dod_dt")), dmy)

  # add bene info to professional claim by member_id and member_yr
  analytic_demo <- original_data %>%
    mutate_at(vars(starts_with("dt")), dmy) %>%
    mutate(member_yr = year(dt_profsvc_end)) %>%
    left_join(membership_process, by = c("member_id", "member_yr")) %>%
    filter(
      (dt_profsvc_end - dob_dt) / 365 <= 99,
      (dt_profsvc_end - dob_dt) / 365 >= 65
    ) %>% # remove bene >99 and <65 yrs old
    rename(
      dt_dob = dob_dt,
      dt_dod = dod_dt
    ) %>%
    select(
      member_id, dt_profsvc_start, dt_profsvc_end, id_physician_npi, cpt_cd,
      cpt_mod, e_proc_grp, e_proc_grp_lbl, dt_dob, dt_dod, zip_cd, flg_male,
      e_race_wbho
    )

  analytic_demo
}
