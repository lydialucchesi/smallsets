# start smallset mydata caption[Remove rows where C2 
# is FALSE.]caption
mydata <- mydata[mydata$C2 == TRUE,]

mC6 <- tapply(mydata$C6, mydata$C1, function(x)
  mean(x, na.rm = TRUE))
C6sub <- as.factor(mydata$C1)
levels(C6sub) <- mC6
mydata$C6[is.na(mydata$C6)] <- round(as.numeric(levels(C6sub))[C6sub][is.na(mydata$C6)], 2)

mC8 <- tapply(mydata$C8, mydata$C1, function(x)
  mean(x, na.rm = TRUE))
C8sub <- as.factor(mydata$C1)
levels(C8sub) <- mC8
# snap mydata caption[Replace missing values in C6 and 
# C8 with category (C1) means. Drop C7 (too many missing 
# values).]caption
mydata$C8[is.na(mydata$C8)] <- round(as.numeric(levels(C8sub))[C8sub][is.na(mydata$C8)], 2)

mydata$C7 <- NULL

mydata$C9 <- mydata$C3 + mydata$C4
# end smallset mydata caption[Create a new column, 
# C9, by summing C3 and C4.]caption
