
source("gen_data.R")
plot(df$count, df$time)

# start smallset
# snap df
df$defect <- ifelse(df$defect > 1, 1, 0)
model <- lm(count ~ time, data = df)
# snap df
df$count <- round(ifelse(is.na(df$count), predict(model, df), df$count))
df$time <- NULL
# snap df
df[is.na(df)] <- 0
prepped <- subset(df, year > 0)
# snap prepped
# end smallset


