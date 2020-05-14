#' Make working directory locations
#' @description working directory location may change based on the computers and file locations;
#'     This was based on the mapped Maize location on Xilin Chen's Mac and Windows

#' @param data_root : "sample" or "full_data"
#'
#' @param medicare_file : original medicare data file folder name. e.g. demon data folder
#' @param george_file Brian_George folder loc on Maize
#' @param medicare_data_file original medicare locations
#' @param medicare_std_data_file  where the std medicare data will be saved to location

#' name is "Denom_MBSF"
#' @return list items of data directory locactions.
#' @export
#'
#' @examples
#' \dontrun{
#' working_dir_config(data_root = "sample",
#'                    medicare_file = "Denom_MBSF",
#'                    george_file = /Volumes/George_Surgeon_Projects")
#' }

mk_data_dir_path <- function(data_root = "sample",
                             medicare_file = NULL,
                             george_file,
                             medicare_data_file = "/original_medicare_selected_vars/data",
                             medicare_std_data_file = "/standardized_medicare_data_using_R/std") {
  # working directory config
  wd <- list()

  # Brian George folder maize location
  wd$george_file <- george_file

  if (dir.exists(wd$george_file) == FALSE) {
    stop(wd$george_file,
         " doesn't exist in your computer Make sure connect to maize or change the file location based on your computer config")
  } else if (dir.exists(wd$george_file) == TRUE) {
    message("the Brian George Maize folder is located at: ", wd$george_file)
  }


  # input data location: like neppes npi, etc.
  wd$input_data = paste0(wd$george_file,  "/standardized_medicare_data_using_R/input")


  # source root
  if (data_root == "sample") {
    wd$src_data_root <-
      paste0(wd$george_file, medicare_data_file, "/sample/", medicare_file, "/")
    wd$std_data_root <-
      paste0(wd$george_file, medicare_std_data_file, "/sample/")

    wd
  }
  else if (data_root == "full_data") {
    wd$src_data_root <-
      paste0(wd$george_file, medicare_data_file, "/", medicare_file, "/")
    wd$std_data_root <-
      paste0(wd$george_file, medicare_std_data_file, "/full_data/")

    wd
  }
}
