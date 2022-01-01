# Original preprocessing approach proposed on page 21

# Text below copied from page 21
# AGEP (Age): Must be greater than 16
# PINCP (Total person's income): Must be greater than 100
# WKHP (Usual hours worked per week past 12 months): Must be greater than 0
# PWGTP (Person weight (relevant for re-weighting 
#        dataset to represent the general US population 
#        most accurately)): Must be greater than or equal to 1


# start smallset ca_data
ca_data <- subset(ca_data, AGEP > 16)
# snap ca_data
ca_data <- ca_data
ca_data <- subset(ca_data, PINCP > 100)
ca_data <- subset(ca_data, WKHP > 0)
# snap ca_data
ca_data <- ca_data
ca_data <- subset(ca_data, PWGTP >= 1)
ca_data$I36K <- ifelse(ca_data$PINCP > 36000, 1, 0)
# end smallset ca_data