# Alternative preprocessing approach

# Do not filter based on income (keep all incomes, including reported losses)
# Replace no reported working hours with zero
# Use median income for binary cut-off

# start smallset ca_data
ca_data <- subset(ca_data, AGEP > 16)
ca_data$WKHP <- ifelse(is.na(ca_data$WKHP), 0, ca_data$WKHP)
ca_data <- subset(ca_data, PWGTP >= 1)
ca_data$PWGTP <- NULL
ca_data$Income <- ifelse(ca_data$PINCP > 22500, 1, 0)
# end smallset ca_data