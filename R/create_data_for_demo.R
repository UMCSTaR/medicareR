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
