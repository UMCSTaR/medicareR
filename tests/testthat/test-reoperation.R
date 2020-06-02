wd = mk_data_dir_path(data_root = "sample",
                      medicare_file = "Denom_MBSF",
                      george_file = "/Volumes/George_Surgeon_Projects")

analytic_ses = fread(paste0(wd$std_analytic_root, "analytic_ses.csv"))
reop_define <- haven::read_sas(paste0(wd$input_data, "icd910_pr_2reop_fmt.sas7bdat"))


test_that("csv file exists", {
  if(!file.exists("/Volumes/George_Surgeon_Projects")){
    skip('Not connected to maize /Volumes/George_Surgeon_Projects')}
  expect_error(
  reoperation(std_data_root = wd$std_data_root,
              reop_map = reop_define,
              fac_codes_folder = ""))
})
