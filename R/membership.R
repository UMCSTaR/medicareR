#' read in membership data (demon/MBSF)
#' @description  just combine all years bene to one dataset and keep unique record by "member_id"
#'     and "member_year"
#'
#' @inheritParams import_map
#' @param  data_file_name names of the original medicare dataset
#' @param  year year of medicare data
#' @param src_root the original medicare data folder location
#'
#'
#' @return
#' @export
#'
#' @examples
#'
membership <- function(year,
                       data_file_name,
                       schema,
                       src_root,
                       mapping_data = import_mapping) {
  # read in data loc, variable names
  map <- mapping_data %>%
    filter(source_schema == schema)

  src_file_loc <- paste0(src_root, data_file_name)

  # read Medpar files
  message(paste0("reading MBSF data year ", year, "..."))
  fread(src_file_loc, select = map$source_column) %>%
    setnames(., map$source_column, map$target_column)
}
