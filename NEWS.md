# medicareR 0.0.1

# add more procedures
* fixed all warnings from tidyverse by changing data processing to `dtplyr` back end.
* improve speed and memory usage for standardization step for prof claims (read claim data once for two dataset process)
* changed some loops to furrr map; multi-session processing
* moved out build-in constant variable selections to user input map flags (std fac_clm.r, prof_clm.r)
