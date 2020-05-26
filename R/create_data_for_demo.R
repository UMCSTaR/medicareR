# comment out to avoit it running when building the package.

# prof_clm = fread("/Volumes/George_Surgeon_Projects/standardized_medicare_data_using_R/std/sample/prof_clm.csv")
#
# prof_clm_not_real = prof_clm %>%
#   sample_n(1000) %>%
#   mutate_all(sample) %>%
#   mutate(member_id = stringi::stri_rand_strings(1000,10),
#          claim_id = stringi::stri_rand_strings(1000,15)) %>%
#   sample_n(50)
#
#
# prof_clm_code = fread("/Volumes/George_Surgeon_Projects/standardized_medicare_data_using_R/std/sample/prof_clm_code.csv")
#
# prof_clm_code_not_real = prof_clm_code %>%
#   sample_n(1000) %>%
#   mutate_all(sample) %>%
#   mutate(member_id = stringi::stri_rand_strings(1000,10),
#          claim_id = stringi::stri_rand_strings(1000,15)) %>%
#   sample_n(50)
#
# usethis::use_data(prof_clm_code_not_real)


# MBSF data ---------
# year = c(2007:2017)
#
# denom_data_loc = paste0(
#   "/Volumes/George_Surgeon_Projects/original_medicare_selected_vars/data/sample/Denom_MBSF/denom",
#   stringr::str_sub(year, 3, 4),
#   ".csv"
# )
#
# for (i in seq_along(year)){
# data = fread(denom_data_loc[[i]], nrows = 1000) %>%
#   sample_n(80)
#
# data = data %>%
#  mutate_at(vars(contains('bene_id')), ~stringi::stri_rand_strings(80,10)) %>%
#  mutate_at(vars(contains('ZIP')), ~stringi::stri_rand_strings(80,10, "[0-9]")) %>%
#  sample_n(10)
#
# glimpse(data)
#
# assign(paste0("denom", stringr::str_sub(year[[i]], 3, 4)), data)
# }
#
# usethis::use_data(denom07)
# usethis::use_data(denom08)
# usethis::use_data(denom09)
# usethis::use_data(denom10)
# usethis::use_data(denom11)
# usethis::use_data(denom12)
# usethis::use_data(denom13)
# usethis::use_data(denom14)
# usethis::use_data(denom15)
# usethis::use_data(denom16)
# usethis::use_data(denom17)
#

