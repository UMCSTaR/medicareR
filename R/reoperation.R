#' 30 days Re-operation Flags
#' @description  use procedure code ICD code to define reop
#' @details  1. Looking through the procedure codes across multiple admissions;
#'    2. Create the ICD9/10 procedure codes to re-operation mapping using pre-defined operation types;
#'    3. Get the re-operation flags one row per professional claim for each patient;
#'    4. Merge the re-operation flags back to main analytic file;

#'
#' @param std_data_root data path to the standardized data folder
#' @param fac_codes_folder names of the fac_clm_code data folder
#' @param reop_map predefined re-operation and ICD map
#' @param original_data data to add on
#'
#' @return
#' @export
#'
#' @examples

reoperation <- function(std_data_root = wd$std_data_root,
                        reop_map,
                        fac_codes_folder = "fac_clm_code",
                        original_data ) {

  # read fac clm code data ------
  datset_names = list.files(paste0(std_data_root, fac_codes_folder))

  fac_codes_loc = paste0(std_data_root, fac_codes_folder, "/",
                         datset_names)

  if (!all(stringr::str_detect(datset_names, ".csv"))) {
    stop(paste0("Check datasets under fac code file folder ", fac_codes_folder, ". All should have.csv format"))
  } else {
    message("selected datases: ", datset_names)
  }

  # fac_code datasets were saved by year due to sample size too big
  fac_pr_list = list()
  message("reading fac_clm_code dataset...")

  # filter to only procedure code
  for (i in seq_along(fac_codes_loc)) {
    fac_clm_codes = fread(fac_codes_loc[i])

    fac_clm_codes <-
      fac_clm_codes[, pr_date := as_date(pr_date)] # date format

    fac_pr_list[[i]] = fac_clm_codes[code_type == "PR"] %>%
      select(member_id, icd_version, value, pr_date)
  }

  fac_pr = rbindlist(fac_pr_list)

  # reop map ---
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

  reop_pre <- left_join(analytic_ses, # prof claim
                        fac_pr, # fac claim
                        by = "member_id") # join all fac admissions for every bene cross different time

  reop_id <- reop_pre %>%
    mutate(flg_util_reop = ifelse(
      # with in 30 days after discharge from previous admission
      pr_date - as_date(dt_profsvc_end) <= 30 &
        pr_date - as_date(dt_profsvc_end) > 0 &
        # cpt is defined as reop
        value %in% reop,
      1,
      0
    )) %>%
    select(id, member_id, flg_util_reop) %>%
    filter(flg_util_reop == 1) %>%
    pull(id) %>%
    unique()

  original_data %>%
    mutate(flg_util_reop = ifelse(id %in% reop_id, 1, 0))
}
