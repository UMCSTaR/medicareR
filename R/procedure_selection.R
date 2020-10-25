#' Select Procedures from Professional Claim data
#' @description select procedure related claims using CPT
#'
#' @param std_data_root     professional claim data location
#' @param prof_codes_folder professional claim folder name
#' @param cpt_map           should include 3 variable: cpt_cd, e_proc_grp and e_proc_grp_lbl
#' @param test_sas_processed_data_loc this is for testing, only assign value when you want to test,
#'     eg. compare the processing results with SAS code Zhaohui Fan created.
#'     Otherwise leave it NA. When testing, assign the location of sas pre processed prof_clm data.
#'     eg. "/Volumes/George_Surgeon_Projects/medicare_data/sample_npct_std/prof_clm.sas7bdat"
#'
#' @details 1. select procedures base on CPT
#'  2. delete missing NPI procedures(can't link to surgeons)
#'  3. delete duplicated claims
#'  4. aggregate cpt_mode (if two cases have same info but diff cpt_mode, aggregate as one case include two cpt_mode code)
#'
#' @return
#' @export
#'
#'
procedure_selection <- function(std_data_root = wd$std_data_root,
                                prof_codes_folder = "prof_clm",
                                cpt_map = define_proc_by_cpt,
                                test_sas_processed_data_loc = NA) {

  # check cpt_map data has 3 variables
  if (any(!c("cpt_cd", "e_proc_grp", "e_proc_grp_lbl") %in%
          names(cpt_map))) {
    stop("assigned cpt_map doesn't include all vars: cpt_cd, e_proc_grp, e_proc_grp_lbl")
  }

  # check if cpt_map has duplicated CPTs this will causes medicare case duplication during left join
  if(anyDuplicated(cpt_map$cpt_cd) >1) {
    warning("CPT duplications in your cpt_map.csv map!!!!!")
    message("duplicated CPT glimpse below....")
    cpt_map %>%
      dplyr::add_count(cpt_cd) %>%
      dplyr::filter(n>1) %>%
      dplyr::select(-n) %>%
      glimpse()
  }


  # check if file loc exist
  if (!file.exists(paste0(std_data_root, prof_codes_folder))) {
    stop(paste0(
      "file location doesn't exist: ",
      std_data_root,
      prof_codes_folder
    ))
  }

  # read prof clm code data ------
  prof_clm_loc = paste0(std_data_root, prof_codes_folder, "/",
                        list.files(paste0(std_data_root, prof_codes_folder)))

  # unique claim based on
  clm_distinct_vars <-
    c(
      "member_id",
      "svc_start_dt",
      "svc_end_dt",
      "provider_npi",
      "cpt_cd",
      "cpt_mod"
    )

  # read std pro claim data
  if(is.na(test_sas_processed_data_loc)){

    prof_clm_select_list = list()
    for (i in seq_along(prof_clm_loc)) {
      message(
        "reading prof_clm year ",
        stringr::str_extract(prof_clm_loc, "[0-9]+")[i],
        " data...."
      )
      # Read in selected columns--
      col2keep = fread(prof_clm_loc[i], nrows = 0) %>%
        as_tibble() %>%
        select(member_id,
               svc_start_dt,
               svc_end_dt,
               provider_npi,
               cpt_cd,
               contains("cpt_mod"),
               provider_splty) %>% names()

      prof_clm <- fread(prof_clm_loc[i], select = col2keep,
                        colClasses=list(character=col2keep))


      # keep defined CPT code and
      # drop professional claim that don't have NPI
      message("filtering procedures....")

      # filter procedure and NPI
      prof_clm_select_list[[i]] = prof_clm[cpt_cd %in% cpt_map$cpt_cd &
                                             provider_npi != ""]
    }

    prof_clm_select = rbindlist(prof_clm_select_list, fill = T)

  } else if (!is.na(test_sas_processed_data_loc)) {
    # for test purpose, comparing with SAS processed data
    prof_clm = haven::read_sas(paste0(test_sas_processed_data_loc))

    data.table::setDT(prof_clm)
    prof_clm_select <- prof_clm[cpt_cd %in% cpt_map$cpt_cd &
                                  provider_npi != ""]
  }

  # cpt mod wide to long format
  prof_clm_select =
    melt(prof_clm_select,
         measure = patterns("cpt_mod"),
         value.name = "cpt_mod")

  # delete variable created
  prof_clm_select[, variable := NULL]

  # unique cases
  analytic_cptmod = unique(prof_clm_select, by = clm_distinct_vars)

  # keep cpt_mod code to one cell if it is has the same group_by info
  # e.g. if two claims have the same group_by listed vars, then two claims become one with two mod code
  analytic_cpt <- analytic_cptmod %>%
    lazy_dt() %>%   # taking advantage of dtplyr
    group_by(
      member_id,
      svc_start_dt,
      svc_end_dt,
      provider_npi,
      cpt_cd
    ) %>%
    mutate(mod_n = row_number()) %>%
    as.data.table() %>%
    tidyr::pivot_wider(
      names_from = mod_n,
      values_from = cpt_mod,
      names_prefix = "mod"
    ) %>%
    tidyr::unite(
      "cpt_mod",
      starts_with("mod"),
      remove = TRUE,
      na.rm = TRUE,
      sep = ", "
    ) %>%
    ungroup()

  # add cpt names label
  analytic_cpt %>%
    lazy_dt() %>%
    left_join(cpt_map, by = "cpt_cd") %>%
    rename(
      dt_profsvc_start = svc_start_dt,
      # rename to distinguish from facility claim
      dt_profsvc_end = svc_end_dt,
      id_physician_npi = provider_npi
    ) %>%
    as.data.table()
}
