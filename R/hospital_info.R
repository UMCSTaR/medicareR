#' hospital info using AHA data
#'
#' @param aha_data : American Hospital Association data
#' @param original_data : data to add on
#'
#' @return
#' @export
#'
#' @examples
hospital_info <- function(aha_data, original_data) {
  # remove hospital that has >1 records per year
  aha_dup <- aha_data %>%
    filter(PRVNUMGRP != "") %>%
    add_count(year, PRVNUMGRP) %>%
    filter(n > 1) %>%
    select(PRVNUMGRP, year)

  aha_no_dup <- aha_data %>%
    filter(PRVNUMGRP != "") %>%
    add_count(year, PRVNUMGRP) %>%
    filter(n == 1)

  analytic_hosp <- original_data %>%
    anti_join(aha_dup, by = c(
      "facility_prvnumgrp" = "PRVNUMGRP",
      "facility_clm_yr" = "year"
    )) %>%  # no dup hospital record
    left_join(aha_no_dup, by = c(
      "facility_prvnumgrp" = "PRVNUMGRP",
      "facility_clm_yr" = "year"
    )) %>%
    transmute(id,
              flg_hosp_teach_combined = provteach_all, #  Teaching defined by five fields: mapp3, mapp5, mapp8, mapp12, mapp13
              flg_hosp_teach_coth = provteach, #  Member of Council of Teaching Hospital
              val_hosp_rn2cenptday_ratio = rntopt,
              val_hosp_rn2inptday_ratio = nurse_ratio,
              val_hosp_rn2bed_ratio = RN_BED_ratio,
              e_hosp_beds_4grp = bedsize_cat, # 1.<200, 2.200-349, 3.350-499, 4.>=500
              e_hosp_beds_3grp = bedsize_cat2, # 1.<250, 2.250-499, 3.>=500
              e_hosp_region_4grp = case_when(
                region_cat == "North-East" ~ 1,
                region_cat == "West" ~ 2,
                region_cat == "Mid-west" ~ 3,
                region_cat == "South" ~ 4
              ),
              flg_hosp_urban = urban,
              flg_hosp_urban_cbsa = cbsa_urban, # Based on census bureau type */
              flg_hosp_critical = CriticalAH, # Critical access hospital */
              flg_hosp_ca = NCI_hosp, # Cancer hospital (SERV='41') or cancer program approved by ACS */
              val_hosp_mcday2inptday_ratio = mcdipdtoipdtot, # Total facility medicaid days/total facility inpatient days  */
              e_hosp_profit = profit, #  1.for-profit, 2.not-for-profit, 3-other */
              flg_hosp_tech = tech_hosp, # Technology hospital */
              flg_hosp_transplant_svc = Transplant_serv, # Any transplant service */
              flg_hosp_transplant_hosp = Transplant_hosp, # Transplant hospital */
              flg_hosp_tech_transplant = Tech_Cardiac_Transplant, # Technology hosp or transplant services   */
              flg_hosp_ICU_hosp = MedSurg_HospICU,
              flg_hosp_ICU_hlthsys = MedSurg_HlthsysICU,
              flg_hosp_ICU_network = MedSurg_NWICU,
              e_hosp_trauma_4grp = TRAUML90_cat # 1=regional resource trauma center,
              # 2=community trauma center,
              # 3=rural trauma hospital,
              # 4 or greater=other (specific to some states) */
    )

  # adding back vars
  left_join(original_data, analytic_hosp, by = "id")
}
