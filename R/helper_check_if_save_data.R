#' Title check if to overwrite existing data
#'
#' @param wd Where the data you want to save at
#' @param save_or_not "yes" or "no"; save or not save data on maize drive; if no, then this helper file
#'     will not pop up questions.
#' @param csv_name what dataset name you want to save as? has to be `.csv`. e.g. "membership.csv"
#' @param rdata_name the dataset you want to save, e.g. membership
#' @return
#'
helper_check_if_save_data <- function(save_or_not,
                                      wd = NULL,
                                      rdata_name = NULL,
                                      csv_name = NULL) {

  if (save_or_not == "yes") {
    # check if data already existed
    dataset_exist = file.exists(paste0(wd, csv_name))

    if (dataset_exist == TRUE) {
      res <- menu(
        title = paste0("Overwrite ", csv_name, "?"),
        choices = c("Yep", "No")
      )
    } else if (dataset_exist == FALSE) {
      res <- 1
    }

    # output after confirm
    if (res == 1) {
      fwrite(rdata_name, file = paste0(wd$std_data_root, csv_name))
      message(paste0("README: saved the ", csv_name, " on the drive"))
    } else if (res == 2) {
      message(paste0("README: Didn't saved the ", csv_name, " on the drive"))
    }
  } else if (save_or_not == "no") {
    message(paste0("Dataset was not requested to be saved."))
  }

}
