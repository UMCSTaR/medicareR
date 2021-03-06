% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fac_dx_elix.R
\name{fac_dx_elix}
\alias{fac_dx_elix}
\title{Elixhauser flags based on Dx ICD code using ahrq map}
\usage{
fac_dx_elix(original_data)
}
\arguments{
\item{original_data}{data to add on}
}
\value{

}
\description{
main function is using ICD Package comorbid(, map = icd9_map_ahrq);
icd9_map_ahrq for icd9; icd10_map_ahrq for icd10
ICD package map reference: https://rdrr.io/cran/icd/man/icd9_map_ahrq.html
}
\note{
ICD9/10 transition date is "2015-10-1"; icd10 is service date >= "2015-10-1"
}
