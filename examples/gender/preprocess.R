# This code preprocesses the dataset.
# It includes smallset comments for tracking and snapshots.

source("examples/gender/data.R")

# start smallset dat

# Replacing missing minute values with zero
dat[is.na(dat$Min), c("Min")] <- 0

# Calculate total call time
# snap dat
dat$Time <- (dat$Min * 60) + dat$Sec

# Find birthdate
dat$Year <- 2021 - dat$Age

# Reorder columns
dat <-
  dat[, c("Name", "Age", "Year", "Area", "Min", "Sec", "Time")]

# Infer gender based on "Name" and "Year"
predGen <- function(i) {
  gen <-
    gender(dat$Name[i],
           method = "ssa",
           years = c(dat$Year[i] - 9, dat$Year[i] + 10))$gender
  if (is.logical(gen)) {
    return("NA")
  } else {
    return(gen)
  }
}

# snap dat
dat$Gen <- sapply(c(1:nrow(dat)), FUN = predGen)

# Remove rows with missing gender
dat <- subset(dat, !(dat$Gen == "NA"))

# end smallset dat
