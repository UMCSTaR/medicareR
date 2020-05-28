#' Facility Claim Code Standardization
#' @description long format of diagnosis and procedure code, code version, date
#'     and POA
#'
#' @inheritParams  fac_clm
#'
#' @return
#' @export
#' @import data.table
#' @import stringr
#'
#' @examples
fac_clm_code <- function(year,
                         data_file_name,
                         schema,
                         src_root,
                         mapping_data = import_mapping) {
  src_file_loc <- paste0(src_root, data_file_name)

  id_var <- c("BENE_ID", "MEDPAR_ID")

  # get ID var names
  id_var_tbl = mapping_data %>%
    filter(source_schema == schema,
           source_column %in% id_var)

  # import vars -----
  map <- mapping_data %>%
    filter(
      source_schema == schema, # year and schema table
      str_detect(source_column, "&")
    ) %>%
    mutate(
      source_column = str_replace(source_column, "\\[(.*?)\\]", "\\\\d+"),
      target_column = str_remove(target_column, "\\[(.*?)\\]")
    ) # replace macro ro regular expression

  # read data
  message(paste0("reading medpar data year ", year, "..."))
  fac_claim_code <- fread(src_file_loc, colClasses = "character")

  # set all names toupper case
  # 2007 and 2008 medpar has bene_id variable as lowercase, other years as upper case
  setnames(fac_claim_code, toupper(names(fac_claim_code)))

  # diagnosis code ---------
  dx_var <- str_subset(names(fac_claim_code), "ID|DGNS") # detect vars

  dx <- fac_claim_code[, dx_var, with = FALSE] # subset vars

  # icd code ------
  dx_long <-
    # subset
    dx[, str_subset(names(fac_claim_code), "DGNS_\\d+_CD|ID"),
      with = FALSE
    ] %>%
    melt(
      id.vars = id_var,
      measure.vars = patterns("DGNS_\\d+_CD"),
      variable.name = "seq",
      value.name = "value"
    )

  # delete missing
  dx_long <- dx_long[!is.na(value) & value != ""]

  # create seq num
  dx_long <- dx_long[, `:=`(code_type = "DX", icd_version = NA, seq = str_extract(seq, "\\d+"))]

  # add POA -----
  if (sum(str_detect(names(dx), "POA")) > 0) {
    dx_poa <- dx[, .SD, .SDcols = patterns("POA_DGNS_\\d+_IND_CD|ID")] %>%
      melt(
        id.vars = id_var,
        measure.vars = patterns("POA_DGNS_\\d+_IND_CD"),
        variable.name = "seq",
        value.name = "flg_poa"
      )

    # delet missing
    dx_poa <- dx_poa[flg_poa != ""]

    # create seq num
    dx_poa <- dx_poa[, seq := str_extract(seq, "\\d+")]

    # arrange ID
    setorder(dx_poa, BENE_ID)

    # full join dx_long and dx_poa
    dx_long <- merge(dx_long, dx_poa, all = TRUE)
  }

  # add ICD version
  if (sum(str_detect(names(dx), "DGNS_VRSN_CD_")) > 0) {
    dx_icd_version <- dx[, .SD, .SDcols = patterns("DGNS_VRSN_CD_|ID")] %>%
      melt(
        id.vars = id_var,
        measure.vars = patterns("DGNS_VRSN_CD_"),
        variable.name = "seq",
        value.name = "icd_version"
      )

    dx_icd_version <- dx_icd_version[, `:=`(seq = str_extract(seq, "\\d+"))]

    setorder(dx_icd_version, BENE_ID)

    dx_long <- merge(dx_long[, icd_version := NULL], dx_icd_version, all.x = TRUE)
  }

  # procedure code and date -------
  pr <- fac_claim_code[, .SD, .SDcols = patterns("ID|SRGCL")]

  # add rowname "id"
  pr <- pr[, id := rownames(pr)]


  # pr code -----
  pr_code <- pr[, .SD, .SDcols = patterns("SRGCL_PRCDR_\\d+_CD|^id")] %>%
    melt(
      id.vars = "id",
      measure.vars = patterns("SRGCL_PRCDR_\\d+_CD"),
      variable.name = "seq",
      value.name = "value"
    )

  # delete missing
  pr_code <- pr_code[!is.na(value) & value != ""]

  # create seq num
  pr_code <- pr_code[, `:=`(seq = str_extract(seq, "\\d+"), code_type = "PR")]

  # arrange id
  setorder(pr_code, id)


  # pr date ------
  pr_date <- pr[, .SD, .SDcols = patterns("SRGCL_PRCDR_PRFRM_\\d+_DT|^id")] %>%
    melt(
      id.vars = "id",
      variable.name = "seq",
      value.name = "pr_date"
    )

  # delete missing
  pr_date <- pr_date[!is.na(pr_date) & pr_date != ""]

  # create seq num
  pr_date <- pr_date[, `:=`(
    seq = str_extract(seq, "\\d+"),
    pr_date = lubridate::dmy(pr_date)
  )]

  # arrange id
  setorder(pr_date, id)

  if (sum(str_detect(names(pr), "VRSN")) > 0) {
    pr_icd_version <- pr[, .SD, .SDcols = patterns("SRGCL_PRCDR_VRSN_CD|id|ID")] %>%
      melt(
        measure.vars = patterns("SRGCL_PRCDR_VRSN_CD"),
        variable.name = "seq",
        value.name = "icd_version"
      )

    # delete missing
    pr_icd_version <- pr_icd_version[!is.na(icd_version) & icd_version != ""]

    # create seq num
    pr_icd_version[, `:=`(seq = str_extract(seq, "\\d+"), BENE_ID = NULL, MEDPAR_ID = NULL)]

    # arrange id
    setorder(pr_icd_version, id)

    # left join
    pr_code <- merge(pr_code, pr_icd_version, all.x = TRUE)
    rm(pr_icd_version)
  }

  claim_pr <- pr[, .SD, .SDcols = patterns("id|ID")] %>%
    merge(pr_code, all.x = TRUE, by = "id") %>%
    merge(pr_date, all.x = TRUE, by = c("id", "seq"))

  # delete id
  claim_pr[, id := NULL]

  # delete na value
  claim_pr <- claim_pr[!is.na(value)]

  claim_pr <- setorder(claim_pr, BENE_ID, MEDPAR_ID)

  # rm(pr, pr_code, pr_date)

  # icd version and poa
  if (!"icd_version" %in% names(claim_pr)) {
    claim_pr[, icd_version := NA]
  }

  if (!"flg_poa" %in% names(claim_pr)) {
    claim_pr[, flg_poa := NA]
  }


  # combine claim pr and dx -----------
  l <- list(claim_pr, dx_long)
  fac_claim_code <- rbindlist(l, use.names = TRUE, fill = TRUE)[!is.na(value)]

  setorder(fac_claim_code, BENE_ID, MEDPAR_ID)

  setcolorder(
    fac_claim_code,
    c(
      "BENE_ID",
      "MEDPAR_ID",
      "seq",
      "code_type",
      "icd_version",
      "value",
      "flg_poa",
      "pr_date"
    )
  )

  # rename ID variables
  setnames(fac_claim_code, id_var_tbl$source_column, id_var_tbl$target_column)

  fac_claim_code
}
