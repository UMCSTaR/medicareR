#' Procedure and Diagnosis code that defines complication
#' @description create procedure and diagnosis icd list using for complication definition
#' @details  use for define complications (complication_flags)
#'
#' @param complication_map : pre-defined csv input data names located at input data folder
#'     "icd910_dx_pr_2cmp_fmt.sas7bdat"
#'
#' @return  cmp_pr = ICD9/10 for procedures;
#'    cmp_dx = ICD9/10 for diagnosis;
#'    all_n = number of digits to match with
#'
#' @examples
cmp_pr_dx_code_map <- function(complication_map = cmp_define) {

  # number of digits to match with icd9/10 code
  # for example, n_code =3, 967 is 3 digits
  pr_n <- complication_map %>%
    filter(str_detect(code_set, "pr")) %>%
    mutate(n_code = str_sub(fmtname, -2, -2)) %>%
    pull(n_code) %>%
    unique()


  dx_n <- complication_map %>%
    filter(str_detect(code_set, "dx")) %>%
    mutate(n_code = str_sub(fmtname, -2, -2)) %>%
    pull(n_code) %>%
    unique()

  # icd9
  # pr
  cmp_pr9 <- complication_map %>%
    filter(str_detect(code_set, "pr9"))

  # seq along the between icd
  cmp_pr9 <- map2(cmp_pr9$start, cmp_pr9$end, seq) %>%
    unlist() %>%
    unique()

  # dx
  cmp_dx9 <- complication_map %>%
    filter(str_detect(code_set, "dx9"))

  cmp_dx9 <- map2(cmp_dx9$start, cmp_dx9$end, seq) %>%
    unlist() %>%
    unique()

  # icd10
  # pr
  cmp_pr10 <- complication_map %>%
    filter(str_detect(code_set, "pr10")) %>%
    # if there are more numbers between star and end
    mutate(
      alph = str_sub(start, 0, -3),
      start_2 = str_sub(start, -2),
      end_2 = str_sub(end, -2)
    )

  # check if start equals to end icd10 code
  if (cmp_pr10 %>%
      filter(start != end) %>%
      nrow() == 0) {

    cmp_pr10 = unique(c(cmp_pr10$start, cmp_pr10$end))
  } else {

    # seq along start to end icd10 code
    cmp_pr10 <- pmap(
      list(
        alph = cmp_pr10$alph,
        start = cmp_pr10$start_2,
        end = cmp_pr10$end_2
      ),
      icd10_seq
    ) %>%
      unlist() %>%
      unique()
  }


  # dx ---
  cmp_dx10 <- complication_map %>%
    filter(str_detect(code_set, "dx10")) %>%
    # if there are more numbers between star and end
    mutate(
      alph = str_sub(start, 0, -3),
      start_2 = str_sub(start, -2),
      end_2 = str_sub(end, -2)
    )

  # seq along start to end to find every icd10 code including between
  # a function to seq along numeric numbers
  icd10_seq <- function(alph, start, end) {
    if (!is.na(as.numeric(start))) {
      last2 <- str_pad(
        seq(start, end),
        width = 2, pad = "0", side = "left"
      )
    } else {
      last2 <- start
    }

    paste0(alph, last2)
  }

  cmp_dx10 <- pmap(
    list(
      alph = cmp_dx10$alph,
      start = cmp_dx10$start_2,
      end = cmp_dx10$end_2
    ),
    icd10_seq
  ) %>%
    unlist() %>%
    unique()

  # create new icd values based on the digits required to match
  all_n <- as.numeric(
    unique(
      c(pr_n, dx_n)
    )
  )

  # comb --
  list(
    cmp_pr = c(cmp_pr9, cmp_pr10),
    cmp_dx = c(cmp_dx9, cmp_dx10),
    all_n = all_n
  )
}
