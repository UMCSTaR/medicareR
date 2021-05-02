#' Readmission Flags
#' @description using facility claim data, left join by member id
#' @details    1. Find the 30-day readmission,  maybe multiple readmission
#'    2. Roll up to a single row per member_id & prof_calim_id
#'    3. Merge the 30-day readmission flag back to the main analytic file
#'    4. readmission to emergent inpatient admission
#'
#' @inheritParams reoperation
#' @param fac_clm_folder facility claim folder name
#' @return
#' @export
#'
#' @examples
readmission <- function(original_data,
                        std_data_root = wd$std_data_root,
                        fac_clm_folder = "fac_clm",
                        emergent_admission_medpar_claim_ids,
                        year = 2007,
                        max_year = 2018
                        ) {

  message("reading fac_clm dataset year ", year)

  # create file paths ----
  if (year<max_year){
    fac_clm_name = paste0("fac_clm_", c(year,year+1), ".csv")  # 2 years
  } else {
    fac_clm_name = paste0("fac_clm_", year, ".csv")  # 1 year, will exclude December cases
  }

  fac_clm_loc = file.path(std_data_root, fac_clm_folder, fac_clm_name)

  if(!all(file.exists(fac_clm_loc))){
    stop("path doesn't exist at ", fac_codes_loc)
  }

  # read data -----
  if(length(fac_clm_name)==1){
    fac_clm <- fread(fac_clm_loc, colClasses = 'character')  # max year has no follow up
  } else if(length(fac_clm_name)==2){
    # to get 30 days follow up
    fac_clm = map_df(fac_clm_loc, ~fread(.x, colClasses = 'character'))
  }

  # keep emergent admission medpar claims
  fac_clm = fac_clm[claim_id %in% emergent_admission_medpar_claim_ids]

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
