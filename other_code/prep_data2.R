
source("gen_data.R")
plot(df$count, df$time)

# start smallset
# snap df
df$defect <- ifelse(df$defect > 1, 1, 0)
# snap df
df$total <- df$count + df$time
# if I drop incomplete cases here I get an error
# snap df
df <- df[complete.cases(df),]
# snap df
df$time <- df$time * 60
df$char <- "testing char"
# snap df
# end smallset