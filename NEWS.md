# medicareR 0.0.1

## add more procedures
* fixed all warnings from tidyverse by changing data processing to `dtplyr` back end.
* improved speed and memory usage for standardization step for prof claims (read claim data once for two dataset process)
* changed some loops to `furrr` map; multi-session processing
* moved out build-in constant variable selections to user input map flags (std fac_clm.r, prof_clm.r)

## Update 2021

**Folder/data files structure changes**

**1. Standardization**
  - rerun sample sections from full data, because some years have no bene data. Code was not change, the data needed updates.
  - Edited import mapping to include a new fac_clm variable [SS_LS_SNF_IND_CD](https://resdac.org/cms-data/variables/medpar-short-staylong-staysnf-indicator-code)
  - Created inpatient only MedPAR facility claims. Saved inpatient Medpar claims under `fac_clm_hosp` folder. These were used for readmission and re-operation redefinition
  - Changed standardization steps membership, professional, facility claim and professional claims to be processed and saved by year. 
  - added a profiling document for performance comparison before and after code changes
  
**2. Analytic file functions**
 - *procedure_selection*: process and save by year
 - *bene_info*:  process and save by year; add option to not filter patient age
 - *fac_dx*: process and save by year; used fac_clm and fac_clm_code data folders; used current year and next year of facility claims data to make sure to have follow up like 30 days death
 - *fac_dx_elix*: make empty icd9 and icd10 tables if some years don't have both of them. Fix Null error. Remove row ICD diagnosis code from analytic file
 - *ses_info*: change zip code to be character to fix zip code that start with 0
 - *emergent_merpar_claim_ids_all_year*: a newly added function to get emergent admission claim IDs; This is used for readmission and reoperation redefinition
 - *reoperation*: use medpar claims path to only inpatient claims; emergent claims (unplanned procedures)
 - *readmission*: use medpar claims path to only inpatient claims; emergent readmission
 - *complication_flags*: process by year; use current year and the next year facility claim codes to ensure 30 days follow up
 - *multi_surgeon_proc_assit_flags*: add surgeon roles including two surgeons and surgical team
 - *save_file* and *read_fiile*: newly added functions that makes read and save files by year easier
 
 **3. Analytic file**
  - Create primary surgeon only claim cases; added if had assistant surgeon with the primary surgeon. We exclude any claims filed by assistant surgeon or surgical team.
