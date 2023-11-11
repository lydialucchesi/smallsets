# smallsets snap s_data caption[Removed rows where C2
# is FALSE.]caption
s_data <- s_data[s_data$C2 == TRUE,]

# smallsets snap 9 s_data caption[Replaced missing values in C6 and
# C8 with column means. Dropped C7 because there are too many
# missing values.]caption
s_data$C6[is.na(s_data$C6)] <- mean(s_data$C6, na.rm = TRUE)
s_data$C8[is.na(s_data$C8)] <- mean(s_data$C8, na.rm = TRUE)
s_data$C7 <- NULL

# smallsets snap +1 s_data caption[Created a new column,
# C9, by summing C3 and C4.]caption
s_data$C9 <- s_data$C3 + s_data$C4

# smallsets resume caption[Ran the model but then 
# decided to make another change to the dataset.]caption

# smallsets snap 27 s_data caption[Created a new categorical 
# column, C10, based on C9 terciles.]caption
t <- quantile(s_data$C9, c(0:3 / 3))
s_data$C10 = with(s_data, cut(
  C9,
  t,
  include.lowest = T,
  labels = c("Low", "Med", "High")
))

