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
