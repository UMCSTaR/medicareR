#' Save files in create analytic file step uisng fwrite
#'
#' @param data file name to save
#' @param wd  directory path to the analytic file folder
#' @param save_folder name of the folder to save under
#' @param year year will be used as the csv data name
#'
#' @return
#' @export
#'

save_file <- function(data,
                      std_analytic_root = wd$std_analytic_root,
                      save_folder = NULL,
                      year) {

  path = paste0(std_analytic_root, save_folder)

  if(!file.exists(path)){
    stop(path, " doesn't exist.")
  }

  fwrite(data, file = paste0(path, "/", year, ".csv"))
}



#' Read analytic file
#'
#' @param std_analytic_root
#' @param folder_name
#' @param year
#' @param ... from fread
#'
#' @return
#' @export

read_file <- function(std_analytic_root = wd$std_analytic_root,
                      folder_name = NULL,
                      year,
                      ...) {
  fread(paste0(std_analytic_root, folder_name, "/", year, ".csv"), ...)
}

