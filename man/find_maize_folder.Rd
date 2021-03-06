% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/computer_config.R
\name{find_maize_folder}
\alias{find_maize_folder}
\title{Find Brian George folder location}
\usage{
find_maize_folder(your_BG_folder_loc = NA)
}
\arguments{
\item{your_BG_folder_loc}{put your own BG location if the function can't find yours. e.g."/Volumes/George_Surgeon_Projects"}
}
\value{
Brian_George folder location on maize, if it can't automatically detect the location, you
can also manually specify the location
}
\description{
This script detects the computer environment automatically
and assign variable \code{Brian_George_folder_loc} to an detected directory.
If the locations can't be detected, you manually assign  value to Brian_George_folder_loc
}
\examples{
\dontrun{
# if this can't detect your BG folder, you can assign it by
find_maize_folder(your_BG_folder_loc = "/Volumes/George_Surgeon_Projects")
}

}
