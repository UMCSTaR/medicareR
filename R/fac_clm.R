#' facility claim standardization
#' @description combines all years MedPAR claims (not including dx and pr code),
#'     and removed the duplication claims;
#'
#' @param year  year of medicare data
#' @param data_file_name  original MedPAR dataset names
#' @param schema  defined in csv file. defult is "fac_clm"
#' @param src_root  locations of MedPAR on database
#' @param mapping_data select medicare original vars to mapped vars
#'
#' @return combined facility claims over years (no dup)
#'
#' @export
#'
#' @examples
fac_clm <-
  function(year,
           data_file_name,
           schema,
           src_root,
           mapping_data = import_mapping) {
    # thses varaibles will be used to dedup facility claims
    dedup_key <- c("member_id", "claim_id")
  # vars that are included in the stf fac_claim data
  fac_clm_vars <- c(
    "member_id",
    "claim_id",
    "claim_yr",
    "facility_npi",
    "facility_prvnumgrp",
    "svc_from_dt",
    "svc_end_dt",
    "dischg_disp_cd",
    "dischg_status_cd",
    "admit_type_cd",
    "claim_type_cd",
    "primary_pyr_cd",
    "drg_cd",
    "admit_dx",
    "admit_dx_vrsn",
    "icd_dx_cnt",
    "icd_pr_cnt",
    "claim_line_id"
  )

  map <- mapping_data %>%
    filter(source_schema == schema) %>%
    filter(target_column %in% fac_clm_vars)

  src_file_loc <- paste0(src_root, data_file_name)

  message(paste0("reading medpar data year ", year, "..."))
  fac_claim <- fread(src_file_loc, colClasses = "character")

  # to set all names toupper case
  # 2007 and 2008 medpar has bene_id variable as lowercase, other years as upper case
  setnames(fac_claim, toupper(names(fac_claim)))
  # rename all to target columns in std data
  setnames(fac_claim, map$source_column, map$target_column)

  # deduplicate based on member_id and medpar claim_id
  fac_claim_uniqe <- unique(fac_claim, by = dedup_key)

  fac_claim_uniqe
}
