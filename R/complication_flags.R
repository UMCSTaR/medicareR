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


complication_flags <- function(original_data = analytic_readmit,
                               std_data_root = wd$std_data_root,
                               fac_codes_folder = "fac_clm_code",
                               year = 2007,
                               max_year = 2018,
                               cmp_pr,
                               cmp_dx,
                               all_n) {
  # read fac clm code data ------
  if (!str_detect(fac_codes_folder, ".sas")) {
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


    # read in all fac_code cross year and left join with analytic file
    # fac_code datasets were saved by year due to sample size too big
    message("reading fac_clm_code dataset...")

    fac_clm_codes = map_df(fac_codes_loc, ~fread(.x, colClasses = "character"))

    code_val <- original_data %>%
      lazy_dt() %>%
      left_join(fac_clm_codes,
                by = c("member_id", "fac_claim_id" = "claim_id")) %>%
      select(id,
             code_type,
             value,
             pr_date,
             dt_profsvc_end,
             flg_poa,
             dt_facclm_dschg) %>%
      as.data.table()

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
    code_val <- code_val %>% as_tibble() %>%
      mutate(!!value_n[i] := str_sub(value, 1, all_n[i]))
  }

  message("adding complication flag year ", year)
  # any complication-------
  add_cmp_flg <- code_val %>%
    as_tibble() %>%
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

  # be careful of unique id here. It's unique within each claim year
  # if combine professional claims (original data) across years, id
  # is not unique anymore. This may cause duplication problems when
  # combine all professional claim years
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
    lazy_dt() %>%
    mutate(
      flg_cmp_po_any = ifelse(id %in% cmp_any_id, 1, 0),
      # keep poa as character variable
      flg_cmp_po_any_not_poa = ifelse(id %in% id_not_poa, "1", "0")
    ) %>%
    # change flg_cmp_po_any_not_poa to "N/A (no var)" before 2010-01-01
    mutate(
      flg_cmp_po_any_not_poa = ifelse(
        dt_facclm_dschg < "2010-01-01",
        "N/A (no var)" ,
        flg_cmp_po_any_not_poa
      )) %>%
    as.data.table()
}
