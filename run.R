library(readxl)
source("get_balance.R")
source("get_text.R")
source("get_full_balances.R")

dat <- read_excel("data.xlsx")
dat$fecha <- as.Date(dat$fecha)
output <- get_full_balances(dat)
writeLines(output, con = "output.txt")
