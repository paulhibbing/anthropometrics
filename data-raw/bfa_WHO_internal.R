rm(list = ls())
devtools::load_all()


load("R/sysdata.rda")


read_who_0_5 <- function(f, s) {
  readxl::read_excel(f) %>%
  dplyr::select(Age, L, M, S) %>%
  dplyr::mutate(
    Age = Age / 30.4375,
    Sex = s
  ) %>%
  dplyr::relocate(Age, Sex) %>%
  data.frame(stringsAsFactors = FALSE)
}

read_who_5_19 <- function(f, s) {
  pdftools::pdf_text(f) %>%
  purrr::map_dfr(function(x) {
    strsplit(x, "\r\n") %>%
    unlist(.) %>%
    utils::tail(-4) %>%
    utils::head(-2) %>%
    gsub("^ +", "", .) %>%
    strsplit("[: ]+") %>%
    purrr::map_dfr(function(x) data.frame(
      t(x),
      row.names = NULL
    ))
  }) %>%
  dplyr::select(X3, X4, X5, X6) %>%
  stats::setNames(c("Age", "L", "M", "S")) %>%
  dplyr::mutate(Sex = s) %>%
  dplyr::relocate(Age, Sex)
}


standards_who <-
  rbind(
    read_who_0_5("data-raw/bfa-boys-percentiles-expanded-tables.xlsx", "M"),
    read_who_5_19("data-raw/bmifa-boys-5-19years-per.pdf", "M"),
    read_who_0_5("data-raw/bfa-girls-percentiles-expanded-tables.xlsx", "F"),
    read_who_5_19("data-raw/bmifa-girls-5-19years-per.pdf", "F")
  ) %>%
  dplyr::mutate(dplyr::across(!Sex, as.numeric))


rm(read_who_0_5, read_who_5_19)


ls() %>%
paste(collapse = ", ") %>%
paste0("usethis::use_data(", ., ", internal = TRUE, overwrite = TRUE)") %>%
parse(text = .) %>%
eval(.)
