#' Professional claim data
#' @description  use carrier claim and carrier line files.
#'      Physician and procedure info is located at carrier line file.
#' @details  variables included are:  "member_id", "claim_id", "clm_from_dt", "clm_thru_dt",
#'    "clm_disp_cd", "icd_dx_prncpal", "icd_dx_prncpal_vrsn"
#'
#' @param year               year of medicare
#' @param schema             defined in csv mapping files, e.g. "prof_clm1"
#' @param prof_clm           carrier claim file name (rdata)
#' @param prof_ln            carrier line file name (rdata)
#' @param mapping_data       select medicare original vars to mapped vars
#'
#' @return
#' @export
#'

prof_clm <-
  function(year,
           schema,
           prof_clm,
           prof_ln,
           mapping_data = import_mapping) {

    # 1. claim file ---------
    # file map
    prof_clm_map <- mapping_data %>%
      filter(
        source_schema == schema,
        on_claim_line == 0,
        # src file, not line file
        claim_or_code == 1 # var to include in prof claim file
      ) # leave out icd code for separate data file

    # select var
    var <- prof_clm_map$source_column
    prof_clm_sel_var <- prof_clm[, ..var] %>%
      # rename all to target columns in std data
      setnames(
        .,
        prof_clm_map$source_column,
        prof_clm_map$target_column
      )

    # 2. line file ------
    # file map
    line_map <- mapping_data %>%
      filter(
        source_schema == schema,
        on_claim_line == 1
      ) # line file

    # select var
    var2 <- line_map$source_column
    prof_ln_sel_var <- prof_ln[, ..var2] %>%
      # rename all to target columns in std data
      setnames(
        .,
        line_map$source_column,
        line_map$target_column
      )

    # left join line file with claim file
    prof_clm <-
      merge(
        prof_clm_sel_var,
        prof_ln_sel_var,
        all.x = TRUE,
        by = c("member_id", "claim_id")
      )

    # note:
    # compare to std data, we are missing provider_splty_desc and provider_type_cd
    # provide_splty_decs can be found https://www.resdac.org/cms-data/variables/line-cms-provider-specialty-code

    prof_clm_processed <- unique(prof_clm, by = c("member_id", "claim_id", "claim_line_id")) %>%
      setcolorder(., c("member_id", "claim_id", "claim_line_id")) %>%
      setorder(., member_id, claim_id, claim_line_id)

    prof_clm_processed
  }
