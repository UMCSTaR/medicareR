# Brian_George_folder_loc <- medicareR::find_maize_folder()
# wd <- medicareR::mk_data_dir_path(
#   data_root = "sample",           # "sample", "full_data"
#   george_file = Brian_George_folder_loc
# )
#
# load("data/define_proc_sample.rdata")
#
# library(tidyverse)
# procedure_selection(std_data_root = wd$std_data_root,
#                     prof_codes_folder = "prof_clm",
#                     cpt_map = define_proc_sample,
#                     test_sas_processed_data_loc = "/Volumes/George_Surgeon_Projects/medicare_data/sample_npct_std/prof_clm.sas7bdat")
#
# debugonce(procedure_selection)
