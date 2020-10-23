#' facility claim standardization
#' @description combines all years MedPAR claims (not including dx and pr code),
#'     and removed the duplication claims;
#'
#' @param year  year of medicare data
#' @param data_file_name  original MedPAR dataset names
#' @param schema  defined in csv file. defult is "fac_clm"
#' @param src_root  locations of MedPAR on database
#' @param mapping_data select medicare original vars to mapped vars
#'
#' @return combined facility claims over years (no dup)
#'
#' @export
#' @importFrom  dplyr filter
#'
#' @examples
fac_clm <-
  function(year,
           data_file_name,
           schema,
           src_root,
           mapping_data = import_mapping) {
    # these variables will be used to dedup facility claims
    dedup_key <- c("member_id", "claim_id")

    # vars that are included in the std fac_claim data
    map <- mapping_data %>%
      filter(source_schema == schema,
             claim_or_code == 1)

    src_file_loc <- paste0(src_root, data_file_name)

    message(paste0("reading medpar data year ", year, "..."))
    fac_claim <- fread(src_file_loc, colClasses = "character")

    # to set all names toupper case
    # 2007 and 2008 medpar has bene_id variable as lowercase, other years as upper case
    setnames(fac_claim, toupper(names(fac_claim)))
    # subset and rename all to target columns in std data
    fac_claim <- fac_claim[, map$source_column, with = FALSE]
    setnames(fac_claim, map$source_column, map$target_column)

    # deduplicate based on member_id and medpar claim_id
    fac_claim_uniqe <- unique(fac_claim, by = dedup_key)

    fac_claim_uniqe
  }
