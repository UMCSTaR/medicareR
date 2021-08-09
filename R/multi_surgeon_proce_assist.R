#' Multiple suregons, procedures and assistant surgeons flags
#' @description   1. One admission has been operated by multiple physicians;
#'    2. One admission has multiple operations;
#'    3. Assistant physicians (using cpt mod code 80|82)
#'
#' @param original_data data to add flags on
#'
#' @return
#' @export
#'
multi_surgeon_proc_assit_flags <- function(original_data) {
  message("add multiple surgeons flags.....")
  multi_doc <- original_data %>%
    as_tibble() %>%
    filter(id_physician_npi != "" | is.na(id_physician_npi)) %>%
    distinct(
      member_id,
      dt_facclm_adm,
      dt_facclm_dschg,
      id_physician_npi
    ) %>%
    add_count(member_id,
              dt_facclm_adm,
              dt_facclm_dschg,
              name = "n_doc"
    ) %>%
    select(-id_physician_npi)

  # multiple procedures
  # CPT count per admission
  message("add multiple procedures flags.....")

  multi_proc <- original_data %>%
    as_tibble() %>%
    distinct(
      member_id,
      dt_facclm_adm,
      dt_facclm_dschg,
      cpt_cd
    ) %>%
    add_count(member_id,
              dt_facclm_adm,
              dt_facclm_dschg,
              name = "n_proc"
    ) %>%
    select(-cpt_cd)

  # add flg
  message("add assistant surgeon flags.....")

  analytic_phy <- left_join(lazy_dt(original_data), multi_doc) %>%
    left_join(multi_proc) %>%
    distinct(id, .keep_all = T) %>%
    mutate(
      flg_multi_surgeon = ifelse(n_doc > 1, 1, 0),
      flg_multi_cpt = ifelse(n_proc > 1, 1, 0),
      flg_assistant_surgeon = ifelse(str_detect(cpt_mod, "80|82"), 1, 0),
      flg_two_surgeon = ifelse(str_detect(cpt_mod, "62"), 1, 0),
      flg_surgical_team = ifelse(str_detect(cpt_mod, "66"), 1, 0)
    ) %>% as.data.table()

  analytic_phy
}
