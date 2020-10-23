#' 6 months prior comorbidity flags
#' @description 1.	Self merge the data
#'    2.	Identify the 6-month prior comorbidities
#'    3.	Merge back to main analytic file
#'    4.	Cut the data off Jan-June 2007 since no enough data to look back 6 months
#'
#' @param original_data
#' @param month how many months to look back for comorbidity, default is 6 months
#'
#' @return
#' @export
#'
comorbidity_6m <- function(original_data = analytic_30dcmp,
                                   month = 6) {
  # get unique cases
  cmb <- original_data %>%
    lazy_dt() %>%
    select(member_id, fac_claim_id, dt_facclm_adm, dt_facclm_dschg, CHF:HTN_C) %>%
    distinct(member_id, fac_claim_id, dt_facclm_adm, dt_facclm_dschg, .keep_all = T) %>%
    select(-fac_claim_id) %>%
    as_tibble()

  # left join by member_id and x month constrain to find if a bene has admission within
  # 6 months prior, if yes, then cmbs is combined with prior admissions.
  # e.g. if pt A admitted 3 month prior to this admission, and had depression cmb;
  # in this admission, the bene doesn't have depression as cmb, then the 6m_cmb has depression as
  # cmb

  days = month*30

  joined_cmb <- left_join(cmb, cmb, by = "member_id") %>%
    filter(
      as_date(dt_facclm_adm.x) - as_date(dt_facclm_adm.y) >= 0,
      as_date(dt_facclm_adm.x) - as_date(dt_facclm_adm.y) <= days
    ) # 6-month prior

  x <- joined_cmb %>%
    select(contains(".x"), -dt_facclm_adm.x, -dt_facclm_dschg.x) %>%
    rename_all(., ~str_remove(., "\\.x")) # . is wildcard, so use \\\\ to excape

  y <- joined_cmb %>%
    select(contains(".y"), -dt_facclm_adm.y, -dt_facclm_dschg.y) %>%
    rename_all(., ~str_remove(., "\\.y"))

  # if any x month cmb is positive, then assign value 1
  # add cmb from x mons prior with current, eg, if 6mon prior and current both q, x+y =2>1 so, positive,
  # if only one is 1, x+y =1>=1, positive
  month_name = paste0("_",month,"mon")

  cmb_vars <- (x + y) %>%
    mutate_all(.funs = list(~ ifelse(.x > 0, 1, 0))) %>%
    rename_all(.funs = list(~ str_replace(., "$", month_name)))


  comorbitities_6mon <- cbind(
    joined_cmb %>%
      transmute(member_id,
                dt_facclm_adm = dt_facclm_adm.x,
                dt_facclm_dschg = dt_facclm_dschg.x
      ),
    cmb_vars
  )

  # keep one max record per key
  comorbitities_6mon_dedup <- comorbitities_6mon %>%
    group_by(member_id, dt_facclm_adm, dt_facclm_dschg) %>%
    summarise_at(vars(contains("mon")), function(x) max(x)) %>%
    ungroup()


  # Merge back to main analytic file,
  # and cut the Jan-June 2007 data since they don't have enough data to
  # look back x month prior
  cut_date = as_date(paste0("2007-", (13-month),"-1"))

  original_data[dt_facclm_adm >= cut_date] %>%
    merge.data.table(comorbitities_6mon_dedup, by = c("member_id", "dt_facclm_adm", "dt_facclm_dschg"), all.x = TRUE) %>%
    ungroup()
}
