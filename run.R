source("get_balance.R")
source("get_text.R")
source("get_full_balances.R")

dat <- read_csv("data.csv")
output <- get_full_balances(dat)
writeLines(output, con = "output.txt")
