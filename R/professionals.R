#' Read in physicains npi data
#' @description  used in step3_professional
#'      combine all years physician to one dataset and delete who have missing NPI
#'
#' @param year  year of medicare data
#' @param data_file_name  original medicare dataset names
#' @param schema  defined in csv file. defult is "professional"
#' @param src_root  locations of carrier line on database
#' @param mapping_data: select medicare original vars to mapped vars
#'
#' @return combined physician NPI across years
#' @export
#'
#' @examples
professionals <- function(year, data_file_name, schema, src_root, mapping_data = import_mapping) {
  # read in data loc, variable names
  map <- mapping_data %>%
    filter(source_schema == schema)

  src_file_loc <- paste0(src_root, data_file_name)

  # read medicare files
  data.table::fread(src_file_loc, select = map$source_column, colClasses = "character") %>%
    rename_all(~ map$target_column) %>%
    filter(!is.na(provider_npi) & provider_npi != "") %>%
    setDT()
}
