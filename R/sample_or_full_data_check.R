#' Title check if user want to use smaple or full data
#' @details to avid accidental full data processing, there will be a popup question to
#' confirm if "full_data" was chosen
#'
#' @param use_sample_or_not : two option "sample" or "full_data"
#'
#' @return
#'
#' @examples
sample_or_full_data_check <- function(use_sample_or_not) {
  if (use_sample_or_not == "sample") {
    message("sample data are used in the process.")
  } else if (use_sample_or_not == "full_data") {
    full_data_confirm <- menu(
      title = "Are you sure you want to process Full data?",
      choices = c("yes", "no")
    )
    # if no, then stop
    if (full_data_confirm == 2) {
      stop("you stopped using full data")
    }
  }
}
