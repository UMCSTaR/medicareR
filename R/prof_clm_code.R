#' professional claim diagnosis code
#' @description  process using carrier claim; produce long format dx icd and icd version
#'
#' @param year   year of medicare
#' @param schema defined in mapping csv, eg "prof_clm1"
#' @param prof_clm carrier claim data
#'
#' @return
#' @export
#' @importFrom dplyr mutate select rename_at
#'
#' @examples
prof_clm_code <-
  function(year,
           schema,
           prof_clm,
           mapping_data = import_mapping) {

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

    # select var names
    tofind <- paste(clm_code_map$source_column, collapse="|") # create regx for str_extract format
    to_select_vars = na.omit(stringr::str_extract(names(prof_clm), tofind)) # delete NA value
    combo_select_vars = c(id_var_tbl$source_column, to_select_vars) # combine id vars with code vars

    prof_clm_code = prof_clm[, ..combo_select_vars]

    # rename id vars
    setnames(prof_clm_code, id_var_tbl$source_column, id_var_tbl$target_column)

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
