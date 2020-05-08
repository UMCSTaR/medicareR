#' Title: find brian geroge folder location
#' This script detects the computer enviromnent automatically
# and assiagn variable `Brian_George_folder_loc` to existed directory.
# if the locations changed, you can change the value assiagned to Brian_George_folder_loc
#'
#' @param your_BG_folder_loc: put your own BG location if the function can't find yours. e.g."/Volumes/George_Surgeon_Projects/"
#'
#' @return Brian_George folder location on maize, if it can't automaticly detect the location, you
#' can also manually specify the location
#' @export
#'
#' @examples
#'
#'
#'
find_maize_folder <- function(your_BG_folder_loc = NA) {

 if(is.na(your_BG_folder_loc)){

  type = Sys.info()[['sysname']]

  if (type == "Darwin") {
    Brian_George_folder_loc = "/Volumes/George_Surgeon_Projects/"
    file_exist = dir.exists(Brian_George_folder_loc)

    if (file_exist == TRUE) {
      message("Mac Envirment; Brian_George_folder_loc is located at: ",
              Brian_George_folder_loc)
    } else if (file_exist == FALSE) {
      rm(type, file_exist)
      stop(
        "Mac Envirment; Brian_George_folder_loc is NOT located at:",
        Brian_George_folder_loc,
        ". Please Check if you are connected to Maize"
      )
    }

  } else if (type == "Windows") {
    Brian_George_folder_loc = "X:\\George_Surgeon_Projects/"
    file_exist = dir.exists(Brian_George_folder_loc)

    if (file_exist == TRUE) {
      message("Windows PC Envirment; Brian_George_folder_loc is located at:",
              Brian_George_folder_loc)
    } else if (file_exist == FALSE) {
      rm(type, file_exist)
      stop(
        "Windows PC Envirment; Brian_George_folder_loc is NOT located at:",
        Brian_George_folder_loc,
        ". Please Check if your Brian_George_folder_loc is mapped differently"
      )
    }
  }
 } else if (!is.na(your_BG_folder_loc)){
   your_BG_folder_loc
 }
}
