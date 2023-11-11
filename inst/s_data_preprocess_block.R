# smallsets snap 7 s_data caption[Remove rows where C2 is FALSE.]caption
# smallsets snap 12 s_data caption[Replace missing values in C6 and C8 with
# column means. Drop C7 because there are too many missing values.]caption
# smallsets snap 16 s_data caption[Create a new column, C9, by summing C3 and
# C4.]caption

# remove rows where C2 is false
s_data <- s_data[s_data$C2 == TRUE,]

# deal with missing data
s_data$C6[is.na(s_data$C6)] <- mean(s_data$C6, na.rm = TRUE)
s_data$C8[is.na(s_data$C8)] <- mean(s_data$C8, na.rm = TRUE)
s_data$C7 <- NULL

# create a new variable
s_data$C9 <- s_data$C3 + s_data$C4
