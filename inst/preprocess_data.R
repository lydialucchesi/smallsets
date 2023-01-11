# smallsets start mydata caption[Remove rows where C2 
# is FALSE.]caption
mydata <- mydata[mydata$C2 == TRUE,]

mydata$C6[is.na(mydata$C6)] <- mean(mydata$C6, na.rm = TRUE)
# smallsets snap mydata caption[Replace missing values in C6 and
# C8 with column means. Drop C7 because there are too many 
# missing values.]caption
mydata$C8[is.na(mydata$C8)] <- mean(mydata$C8, na.rm = TRUE)
mydata$C7 <- NULL

mydata$C9 <- mydata$C3 + mydata$C4
# smallsets end mydata caption[Create a new column,
# C9, by summing C3 and C4.]caption