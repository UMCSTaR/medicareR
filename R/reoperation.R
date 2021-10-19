#' 30 days Re-operation Flags
#' @description  use procedure code ICD code to define reop
#' @details  1. Looking through the procedure codes across multiple admissions;
#'    2. Create the ICD9/10 procedure codes to re-operation mapping using pre-defined operation types;
#'    3. Get the re-operation flags if defined re operation happened within <=30 days
#'    4. emergent admission only

#'
#' @param std_data_root data path to the standardized data folder
#' @param fac_codes_folder names of the fac_clm_code data folder for inpatient medpar claims; or .sas files for only testing
#' @param reop_map predefined re-operation and ICD map
#' @param original_data data to add on
#' @param year select one year to process, e.g. 2007
#' @param max_year the most recent medicare year we have; for example, in 2020, we have Medpar processed up to 2018
#' @param emergent_admission_medpar_claim_ids a list of all emergent MedPAR admission claim IDs
#'
#'
#' @return
#' @export
#'
#' @examples

reoperation <- function(original_data,
                        reop_map,
                        std_data_root = wd$std_data_root,
                        year,
                        fac_codes_folder = "fac_clm_code",
                        emergent_admission_medpar_claim_ids,
                        max_year = 2018) {

  # read fac clm code data ------
  # note: the reason we need the current year and the following next year is to get 30days followup for cases
  # happened in December. So we need to load next years data to get follow up
  # however, for the latest year, we don't have next year's data, we will filter out cases happened in December.
  if (year<max_year){
    datset_names = paste0("fac_clm_code_", c(year,year+1), ".csv")  # 2 years
  } else {
    datset_names = paste0("fac_clm_code_", year, ".csv")  # 1 year, will exclude december cases
  }

  fac_codes_loc = file.path(std_data_root, fac_codes_folder, datset_names)

  # check
  if(!all(file.exists(fac_codes_loc))){
    stop("path doesn't exist at ", fac_codes_loc)
  }

  if (!all(stringr::str_detect(datset_names, ".csv"))) {
    stop(
      paste0(
        "Check datasets under fac code file folder ",
        fac_codes_folder,
        ". All should have.csv format"
      )
    )
  }

  # reop map --------
  # icd9
  icd9_reop <- reop_map %>%
    filter(code_set == "pr9")

  # get unique icd9 code list that defines reop
  icd9_reop <- purrr::map2(icd9_reop$start, icd9_reop$end, seq) %>%
    unlist() %>%
    unique()

  # unique icd10 code list that defines reop
  icd10_reop <- reop_map %>%
    filter(code_set == "pr10") %>%
    pull(start) %>%
    unique()

  reop <- c(icd9_reop, icd10_reop)

  # facility claim data --------
  # fac_code datasets were saved by year due to big dataset size
  fac_pr_list = list()
  message("process facility year ", year)

  # filter to only procedure code
  for (i in seq_along(fac_codes_loc)) {
    fac_clm_codes = fread(fac_codes_loc[i])

    # filter to selected reoperations ICD; and keep only emergent admission
    fac_clm_codes <- fac_clm_codes[value %in% reop & claim_id %in% emergent_admission_medpar_claim_ids]
    fac_clm_codes <- fac_clm_codes[,pr_date := as_date(pr_date)]        # date format

    fac_pr_list[[i]] = fac_clm_codes[code_type == "PR", .(member_id, icd_version, value, pr_date)]
  }

  fac_pr = rbindlist(fac_pr_list)
  rm(fac_pr_list)

  # deprecated code, not using sas data anymore
  # if (str_detect(fac_codes_folder, ".sas")) {
  #   # for testing sas files
  #   fac_clm_codes = haven::read_sas(paste0(std_data_root, fac_codes_folder))
  #
  #   setDT(fac_clm_codes)
  #   fac_clm_codes <-
  #     fac_clm_codes[, dt_pr := as_date(dt_pr, origin = "1960-01-01")] # date format
  #
  #   fac_pr = fac_clm_codes[code_type == "PR"] %>%
  #     # old sas data has slightly different name
  #     select(member_id,
  #            icd_version = code,
  #            value,
  #            pr_date = dt_pr)
  # }


  # join all fac admissions for every bene cross different time
  # left join
  reop_pre <- merge.data.table(original_data, # prof claim
                        fac_pr, # fac claim
                        by = "member_id",
                        all.x = T)

  reop_id <- reop_pre %>%
    lazy_dt() %>%
    mutate(flg_util_reop = ifelse(
      # with in 30 days after discharge from previous admission
      pr_date - as_date(dt_profsvc_end) <= 30 &
        pr_date - as_date(dt_profsvc_end) > 0,
      1,
      0
    )) %>%
    select(id, member_id, flg_util_reop) %>%
    filter(flg_util_reop == 1) %>%
    as.data.table() %>%
    pull(id) %>%
    unique()

  original_data %>%
    lazy_dt() %>%
    mutate(flg_util_reop = ifelse(id %in% reop_id, 1, 0)) %>% as.data.table()
}


#' Emergent MedPAR claim ids
#' @description This is used to refine readmission; readmission should only be defined as
#' emergent admission after the index procedure. This helps exclude any planned visit after
#' the index surgery. Only need to rerun if MedPAR original data has changed
#' or added new MedPAR year data
#'
#' @param std_data_root data path to the standardized data folder
#' @param fac_clm_folder facility claim file folder name
#'
#' @return
#' @export
#'
#' @examples
emergent_merpar_claim_ids_all_year <- function(std_data_root = wd$std_data_root,
                                               fac_clm_folder = "fac_clm") {
  fac_codes_loc = list.files(paste0(std_data_root, fac_clm_folder),full.names = T)

  claim_id = purrr::map(fac_codes_loc,
                        # 1. read selected vars
                        # 2. filter only emergent admission; https://resdac.org/cms-data/variables/inpatient-admission-type-code
                        ~fread(.x, select = c("claim_id", "admit_type_cd"))[admit_type_cd %in% c(1,2)]$claim_id

  )

  unique(unlist(claim_id))
}



