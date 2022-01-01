# Alternative preprocessing approach

# Do not filter based on income (keep all incomes, including reported losses)
# Replace no reported working hours with zero

# start smallset ca_data
ca_data <- subset(ca_data, AGEP > 16)
# snap ca_data
ca_data$WKHP <- ifelse(is.na(ca_data$WKHP), 0, ca_data$WKHP)
# snap ca_data
ca_data <- subset(ca_data, PWGTP >= 1)
ca_data$I36K <- ifelse(ca_data$PINCP > 36000, 1, 0)
# end smallset ca_data