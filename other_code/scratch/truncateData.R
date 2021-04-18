library(ggplot2)

df <- data.frame(x = c(1), y = c(1))

dfText <- data.frame(x = c(1), y = c(1), text = c("testing the width"), text2 = c(120102))
dfText$text <- as.character(dfText$text)

dfText$text <- str_trunc(dfText$text, 6, "right")
dfText$text2 <- str_trunc(dfText$text2, 4, "right")

ggplot() +
  geom_tile(data = df, aes(x = x, y = y)) +
  coord_equal() +
  geom_text(data = dfText, aes(x = x, y = y, label = text))
  xlim(-5, 5)
