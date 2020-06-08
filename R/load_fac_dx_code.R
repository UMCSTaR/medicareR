#' load standardized fac clm dx code across years
#' @details filter to only diagnosis code
#'
#' @inheritParams fac_dx
#'
#' @return
#'
load_fac_code_data <- function(std_data_root,
                               fac_codes_folder) {
  # check if file loc exist
  if (!file.exists(paste0(std_data_root, fac_codes_folder))) {
    stop(paste0(
      "file location doesn't exist: ",
      std_data_root,
      fac_codes_folder
    ))
  }

  # read fac clm code data ------
  fac_codes_loc = paste0(std_data_root, fac_codes_folder, "/",
                         list.files(paste0(std_data_root, fac_codes_folder)))
  # read in all fac_code cross year
  # fac_code datasets were saved by year due to sample size too big
  facclm_list = list()
  message("reading fac_clm_code dataset...")

  for (i in seq_along(fac_codes_loc)) {
    fac_clm_codes = fread(fac_codes_loc[i])

    # Facility claim codes file:
    # Transpose dx codes from long to wide format
    facclm_dx_by_yr <-
      fac_clm_codes[code_type == "DX"][, var_name := paste0(code_type, seq)] %>% #diagnosis code
      dcast(member_id + claim_id ~ var_name, value.var = c("value"))

    facclm_list[[i]] = facclm_dx_by_yr
  }

  # combine as one dataset
  rbindlist(facclm_list, fill = T)

}
