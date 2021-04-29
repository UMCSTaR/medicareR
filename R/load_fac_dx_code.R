#' load standardized fac clm dx code across years
#' @details filter to only diagnosis code
#'
#' @inheritParams fac_dx
#'
#' @return
#'
load_fac_code_data <- function(std_data_root,
                               fac_codes_folder,
                               year) {

  # read fac clm code data ------
  fac_codes_loc = paste0(std_data_root, fac_codes_folder, "/fac_clm_code_", year, ".csv")

  # check if file loc exist
  if (!file.exists(fac_codes_loc)) {
    stop(paste0(
      "file location doesn't exist: ",
      fac_codes_loc
    ))
  }

  # read in all fac_clm_code
  message("reading fac_clm_code dataset year ", year)

  fac_clm_codes = fread(fac_codes_loc)

  # Facility claim codes file:
  # Transpose dx codes from long to wide format
  facclm_dx_by_yr <-
    fac_clm_codes[code_type == "DX"][, var_name := paste0(code_type, seq)] %>% #diagnosis code
    dcast(member_id + claim_id ~ var_name, value.var = c("value"))

  facclm_dx_by_yr
}
