#' Combine Facility NPI across years
#' @details combine years facility NPI and delete duplications
#'
#' @param year : year of MEDPAR data
#' @param data_file_name : MEDPAR dataset names
#' @param schema : user defined in the mapping csv files. default is "facilities"
#' @param src_root : MedPAR dataset locations
#' @param mapping_data: select medicare original vars to mapped vars
#'
#' @return
#' @export
#'
#' @examples
facilities <- function(year, data_file_name, schema, src_root,
                       mapping_data = import_mapping) {
  # read in data loc, variable names
  map <- mapping_data %>%
    filter(source_schema == schema)

  src_file_loc <- paste0(src_root, data_file_name)

  # read files
  fread(src_file_loc, select = map$source_column, colClasses = "character") %>%
    setnames(., map$source_column, map$target_column) %>%
    .[!is.na(facility_npi) & facility_npi != ""]
}
