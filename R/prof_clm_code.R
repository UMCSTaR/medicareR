#' Title: professional claim diagnosis code
#' @details process using carrier claim; produce long format dx icd and icd version
#'
#' @param year   year of medicare
#' @param schema defined in mapping csv, eg "prof_clm1"
#' @param data_file_name_clm original carrier claim dataset names
#' @param src_root_clm location to carrier claim dataset
#'
#' @return
#' @export
#' @importFrom dplyr mutate select rename_at
#'
#' @examples
prof_clm_code <-
  function(year,
           schema,
           data_file_name_clm,
           src_root_clm,
           mapping_data = import_mapping) {

    # read src carrier claim
    src_file_loc_clm <- paste0(src_root_clm, data_file_name_clm)
    prof_clm <- fread(src_file_loc_clm, colClasses = "character")

    # pre defined variable map
    clm_code_map <- mapping_data %>%
      filter(
        source_schema == schema,
        on_claim_line == 0
      ) %>% # src file, not line file
      filter(target_column %in% c("icd_dx[1-12]", "vrsn_icd_dx[1-12]")) %>%
      mutate(
        source_column = str_replace(source_column, "\\[(.*?)\\]", "\\\\d+"),
        target_column = str_remove(target_column, "\\[(.*?)\\]")
      )

    # id vars
    id_var_tbl <- mapping_data %>%
      filter(
        source_schema == schema,
        on_claim_line == 0,
        str_detect(source_column, "ID")
      )

    # select code ralated vars
    prof_clm_code <- prof_clm %>%
      select(
        id_var_tbl$source_column,
        matches(clm_code_map$source_column)
      ) %>%
      rename_at(vars(id_var_tbl$source_column), ~ id_var_tbl$target_column)

    # icd code
    prof_clm_code_icd <-
      prof_clm_code[, .SD, .SDcols = names(prof_clm_code) %like% "ICD_DGNS_CD|id"] %>%
      melt(
        measure.vars = patterns("ICD_DGNS_CD"),
        variable.name = "seq",
        value.name = "value"
      ) %>%
      .[value != ""] %>%
      .[, `:=`(seq = str_extract(seq, "\\d+"), code_type = "DX")] %>%
      setorder(member_id)


    # icd code version
    prof_clm_code_version <-
      prof_clm_code[, .SD, .SDcols = names(prof_clm_code) %like% "ICD_DGNS_VRSN_CD|id"] %>%
      melt(
        measure.vars = patterns("ICD_DGNS_VRSN_CD"),
        variable.name = "seq",
        value.name = "icd_version"
      ) %>%
      .[icd_version != ""] %>%
      .[, `:=`(seq = str_extract(seq, "\\d+"), code_type = "DX")] %>%
      setorder(member_id)



    # combine icd code with icd version
    prof_clm_dx_code <- merge(prof_clm_code_icd, prof_clm_code_version, all.x = TRUE) %>%
      .[!is.na(value) & value != ""] %>%
      unique(., by = c("member_id", "claim_id", "code_type", "seq", "value"))

    prof_clm_dx_code
  }
