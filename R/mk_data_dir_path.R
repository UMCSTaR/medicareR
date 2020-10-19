#' Make working directory locations
#' @description working directory location may change based on the computers and file locations;
#'     This was based on the mapped Maize location on Xilin Chen's Mac and Windows

#' @param data_root : "sample" or "full_data"
#'
#' @param medicare_file : source (raw) medicare data file folder name. e.g. Denom_MBSF folder;
#'     This is used to create source medicare data location.
#' @param george_file Brian_George folder loc on Maize
#' @param medicare_data_file original medicare location
#' @param medicare_std_data_file  where the std medicare data will be saved to location
#' @param input_data_file location of outsource datasets, e.g. npppes npi files.

#' @return list items of data directory locactions.
#' @export
#'
#' @examples
#' \dontrun{
#' working_dir_config(data_root = "sample",
#'                    medicare_file = "Denom_MBSF",
#'                    george_file = "/Volumes/George_Surgeon_Projects")
#' }

mk_data_dir_path <- function(data_root = "sample",
                             medicare_file = NULL,
                             george_file,
                             medicare_data_file = "/original_medicare_selected_vars/data",
                             medicare_std_data_file = "/standardized_medicare_data_using_R",
                             input_data_file = "/standardized_medicare_data_using_R/input/") {
  # working directory config
  wd <- list()

  # check input
  if (!data_root %in% c("sample", "full_data")) {
    stop("data_root input value has to be: sample or full_data")
  }

  # Brian George folder maize location
  wd$george_file <- george_file

  if (dir.exists(wd$george_file) == FALSE) {
    stop(wd$george_file,
         " doesn't exist in your computer Make sure connect to maize or change the file location based on your computer config")
  }

  # input data location: like neppes npi, etc.
  wd$input_data = paste0(wd$george_file,  input_data_file)


  # std source root
  if (data_root == "sample") {
    wd$src_data_root <-
      paste0(wd$george_file, medicare_data_file, "/sample/", medicare_file, "/")
    wd$std_data_root <-
      paste0(wd$george_file, medicare_std_data_file, "/std/sample/")
    wd$std_analytic_root <-
      paste0(wd$george_file, medicare_std_data_file, "/analytic/sample/")

    wd
  }
  else if (data_root == "full_data") {
    wd$src_data_root <-
      paste0(wd$george_file, medicare_data_file, "/", medicare_file, "/")
    wd$std_data_root <-
      paste0(wd$george_file, medicare_std_data_file, "/std/full_data/")
    wd$std_analytic_root <-
      paste0(wd$george_file, medicare_std_data_file, "/analytic/full_data/")

    wd
  }

  # check file exist only for mac envir
  if (stringr::str_detect(george_file, "Volumes")) {
    if (file.exists(wd$input_data) == FALSE) {
      stop(paste0("path doesn't exist: ", wd$input_data))
    }

    if (file.exists(wd$std_data_root) == FALSE) {
      stop(paste0("path doesn't exist: ", wd$std_data_root))
    }

    if (file.exists(wd$std_analytic_root) == FALSE) {
      stop(paste0("path doesn't exist: ", wd$std_analytic_root))
    }

    if (file.exists(wd$src_data_root) == FALSE) {
      stop(paste0("path doesn't exist: ", wd$src_data_root))
    }
  }

  message("data paths are crearted as:")
  print(wd)

  wd

}
