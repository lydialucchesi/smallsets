# This code creates the synthetic dataset about phone calls.
# It uses charlatan, an R package for making fake data.

# Load packages
library(charlatan)
library(stringr)

set.seed(14)

# Get 1000 phone numbers for U.S.
phone_num <- ch_phone_number(n = 1000, locale = "en_US")

# Format to just area code
phone_num_only <- strsplit(phone_num, "[x]")
for (i in 1:length(phone_num_only)) {
  phone_num_only[i] <- phone_num_only[[i]][1]
}
digits <- gsub("\\D+", "", phone_num_only)
substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}
ten <- substrRight(digits, 10)
code <- substr(ten, 1, 3)

# Prepare random missing values
M <- MissingDataProvider$new()

# Create data frame
dat <- data.frame(
  Name = ch_name(n = 1000, local = "en_US"),
  Age = ch_integer(n = 1000, min = 20, max = 79),
  Area = code,
  Min = M$make_missing(x = ch_integer(n = 1000, 1, 71)),
  Sec = ch_integer(n = 1000, 0, 60)
)

# Remove titles from names
dat$Name <- gsub("Mr. ", "", dat$Name)
dat$Name <- gsub("Mrs. ", "", dat$Name)
dat$Name <- gsub("Ms. ", "", dat$Name)
dat$Name <- gsub("Miss ", "", dat$Name)
dat$Name <- gsub("Dr. ", "", dat$Name)
dat$Name <- gsub("Judge ", "", dat$Name)
dat$Name <- gsub("Unnamed ", "", dat$Name)

# Use only first names
dat$Name <- word(dat$Name, 1)
