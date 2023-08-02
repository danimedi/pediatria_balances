library(readr)
library(glue)

get_text <- function(...) {
  res <- get_balance(...)
  txt <- read_lines("template.txt")
  txt <- paste0(txt, collapse = "\n")
  txt <- glue(txt)
  txt
}
