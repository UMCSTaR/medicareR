# medicareR 0.0.1

## add more procedures
* fixed all warnings from tidyverse by changing data processing to `dtplyr` back end.
* improved speed and memory usage for standardization step for prof claims (read claim data once for two dataset process)
* changed some loops to `furrr` map; multi-session processing
* moved out build-in constant variable selections to user input map flags (std fac_clm.r, prof_clm.r)

## Update 2021

**1. Standardization**
  - rerun sample sections from full data, because some years have no bene data. Code was not change, the data needed updates.
  - Edited import mapping to include a new fac_clm variable [SS_LS_SNF_IND_CD](https://resdac.org/cms-data/variables/medpar-short-staylong-staysnf-indicator-code)
  - Created inpatient only MedPar facility claims. Saved inpatient Medpar claims under `fac_clm_hosp` folder. These were used for readmission and re-operation redefinition
  - Changed standardization steps membership, professional, facility claim and professional claims to be processed and saved by year. 
  - added a profiling document for performance comparision before and after code changes
  
**2. Analytic file functions**
 - *procedure_selection*: process and save by year
 - *bene_info*:  process and save by year;
 
