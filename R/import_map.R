#' import schema and where the schema come from (medicare data)
#' include: dataset name, select varaible and map to variable names
#' @param schema : schema of the output file defined in the mapping csv files. for example ""membership" is a schema
#' @param source: defualt "source_schema" to generate std medicare data
#' "source_file": get medicare data files name; This option is used for standarzation process
#' "source_schema": to get user defined schema name; only use to get all vars under the same
#' medicare dataset. This was used to create sas code.
#' @param import_src: dataset that includes medicare original data to mapped std data
#' @param import_mapping: dataset that includes medicare original variable names to std data var names
#'
#' @return
#' @export
#'
#' @examples
#'
import_map <- function(schema,
                       source = "source_schema",
                       src_data = import_src,
                       mapping_data = import_mapping) {
  import_rule <- src_data %>%
    select(source_schema, source_dir, source_file, line_file) %>%
    left_join(mapping_data) %>%
    arrange(source_dir)

  # this is for data processing
  if (source == "source_schema") {
    import_rule %>%
      filter(str_detect(source_schema, schema))
    # this used to create sas file
  } else if (source == "source_file") {
    import_rule %>%
      filter(str_detect(source_file, schema))
  }
}
