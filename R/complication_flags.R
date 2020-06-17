#' Complication Flags based on Procedure and Diagnosis ICD
#' @description complication flag and complication excluding POA
#'
#' @inheritParams fac_dx
#' @param cmp_pr procedure ICD code list used to define complication
#' @param cmp_dx diagnosis ICD code list used to define complication
#' @param all_n  number of digits to match with ICD code
#'
#' @return add two variables; complication and complication w/o POA
#' @export
#'
#' @details POA var only available after 2010-01-01.(no poa var recorded before)
#'  1. Create the ICD9/10 Dx/PR to complications mapping format
#'  2. Create the complication flags at the DX/PR code level (poa and w/o poa)
#'  note: both DX and pr code are used to define complication,
#' however pr should be within 30days, dx has no time frame.
#'
#' @examples


complication_flags <- function(std_data_root = wd$std_data_root,
                               fac_codes_folder = "fac_clm_code",
                               original_data = analytic_readmit,
                               cmp_pr,
                               cmp_dx,
                               all_n) {
  # read fac clm code data ------
  # read csv files
  if (!str_detect(fac_codes_folder, ".sas")) {
    fac_codes_loc = paste0(std_data_root, fac_codes_folder, "/",
                           list.files(paste0(std_data_root, fac_codes_folder)))

    # read in all fac_code cross year and left join with analytic file
    # fac_code datasets were saved by year due to sample size too big
    code_val_list = list()
    message("reading fac_clm_code dataset...")

    for (i in seq_along(fac_codes_loc)) {
      fac_clm_codes = fread(fac_codes_loc[i])

      code_val <- original_data %>%
        left_join(fac_clm_codes,
                  by = c("member_id", "fac_claim_id" = "claim_id")) %>%
        select(id,
               code_type,
               value,
               pr_date,
               dt_profsvc_end,
               flg_poa,
               dt_facclm_dschg)

      code_val_list[[i]] = code_val
    }

    # combine as one dataset
    code_val = rbindlist(code_val_list, fill = T)

  } else if (str_detect(fac_codes_folder, ".sas")) {
    # read sas file
    fac_clm_codes <-
      haven::read_sas(paste0(std_data_root, fac_codes_folder))


    fac_clm_codes = fac_clm_codes %>%
      mutate_at(vars(contains("dt")), ~ as_date(., origin = "1960-01-01")) %>%
      rename(pr_date = dt_pr)

    code_val <- original_data %>%
      left_join(fac_clm_codes,
                by = c("member_id", "fac_claim_id" = "claim_id")) %>%
      select(id,
             code_type,
             value,
             pr_date,
             dt_profsvc_end,
             flg_poa,
             dt_facclm_dschg)
  }


  # create icd code in facility clm based on the number of requiring matching digits
  value_n <- paste0("value_", all_n, "_d")

  for (i in seq_along(all_n)) {
    code_val <- code_val %>%
      mutate(!!value_n[i] := str_sub(value, 1, all_n[i]))
  }

  message("adding complication flag...")
  # any complication-------
  add_cmp_flg <- code_val %>%
    # dx_n: "5" "3" "4" "6" "7"
    mutate(
      flg_cmp_po_any = ifelse(
        str_detect(code_type, "DX") & # diagnosis
          (
            value %in% cmp_dx |
              # based on number of required matching digits, we look at every required digits
              value_3_d %in% cmp_dx |
              value_4_d %in% cmp_dx |
              value_7_d %in% cmp_dx |
              value_5_d %in% cmp_dx |
              value_6_d %in% cmp_dx
          ),
        1,
        0
      ),
      # pr_n: "3" "4" "7"
      flg_cmp_po_any = ifelse(
        str_detect(code_type, "PR") & # procedure
          as_date(pr_date) - as_date(dt_profsvc_end) <= 30 &
          # based on number of required matching digits, we look at every required digits
          as_date(pr_date) - as_date(dt_profsvc_end) >= 0 &
          (
            value %in% cmp_pr |
              value_3_d %in% cmp_pr |
              value_4_d %in% cmp_pr |
              value_7_d %in% cmp_pr
          ),
        1,
        flg_cmp_po_any
      )
    )

  cmp_any_id = add_cmp_flg %>%
    filter(flg_cmp_po_any == 1) %>%
    pull(id) %>%
    unique() # get unique id that has complication


  # POA any complication -------
  # adding poa vars to any complication definition
  # defined as: 1: flg_poa != yes, 2, dx and pr are mapped (same as any complication)
  # POA status only valid after 2010-01-01

  message("adding complication flag excluding POA...")

  id_not_poa <- add_cmp_flg %>%
    filter(!flg_poa %in% c("y", "Y", "1"),
           flg_cmp_po_any == 1) %>%
    pull(id) %>%
    unique() # get unique ids that have no poa and had complication


  # add any complication and any comp POA to facility clm
  original_data %>%
    mutate(
      flg_cmp_po_any = ifelse(id %in% cmp_any_id, 1, 0),
      flg_cmp_po_any_not_poa = ifelse(id %in% id_not_poa, 1, 0)
    ) %>%
    # change flg_cmp_po_any_not_poa to "N/A (no var)" before 2010-01-01
    mutate(
      flg_cmp_po_any_not_poa = ifelse(
        dt_facclm_dschg < "2010-01-01",
        "N/A (no var)" ,
        flg_cmp_po_any_not_poa
      )
    )
}
