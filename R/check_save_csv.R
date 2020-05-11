#' Title check if want to overwrite existing files
#'
#' @param std_data_root : data folder location
#' @param csv_data_name :data name you wantt o sign as: eg: "fac_claim.csv"
#' @param data_to_save : dataset you want to save
#' @param save_csv : save csv file or not: "yes" or "no"
#'
#' @return message
#'

check_save_csv <- function(std_data_root = wd$std_data_root,
                           csv_data_name,
                           data_to_save,
                           save_csv) {

  dataset_exist = file.exists(paste0(std_data_root, csv_data_name))

  # check if overwrite existing csv files
  if (save_csv == "yes" & dataset_exist == TRUE) {
    res <-
      readline(paste0(csv_data_name, "already exist. Are you sure you want to overwrite? yes or no: "))
  } else if (save_csv == "yes" & dataset_exist == FALSE) {
    res <- "yes"
  } else (
    res = ""
  )

  # output after confirm
  if (res == "yes") {
    message("README: saved the csv on the drive")
    fwrite(fac_claim, file = paste0(wd$std_data_root, csv_data_name))
  } else if (res == "no" | res == "") {
    message("README: Didn't save/overwrite the csv on the drive")
  }
}
