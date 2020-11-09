df <- data.frame(x = numeric(), 
                 y = numeric(),
                 col1 = character(), 
                 col2 = character(),
                 col3 = character(),
                 col4 = character(),
                 col5 = character(),
                 col6 = character())

c_1_7 <- data.frame(x = c(1), 
                    y = c(7),
                    col1 = c("G"), 
                    col2 = c("G"),
                    col3 = c("G"),
                    col4 = c("G"),
                    col5 = c("G"),
                    col6 = c("G"))
df <- rbind(df, c_1_7)

c_2_7 <- data.frame(x = c(2), 
                    y = c(7),
                    col1 = c("G"), 
                    col2 = c("G"),
                    col3 = c("G"),
                    col4 = c("G"),
                    col5 = c("G"),
                    col6 = c("G"))
df <- rbind(df, c_2_7)

c_3_7 <- data.frame(x = c(3), 
                    y = c(7),
                    col1 = c("G"), 
                    col2 = c("G"),
                    col3 = c("Y"),
                    col4 = c("NA"),
                    col5 = c("NA"),
                    col6 = c("NA"))
df <- rbind(df, c_3_7)

c_4_7 <- data.frame(x = c(4), 
                    y = c(7),
                    col1 = c("G"), 
                    col2 = c("P"),
                    col3 = c("G"),
                    col4 = c("G"),
                    col5 = c("G"),
                    col6 = c("G"))
df <- rbind(df, c_4_7)

c_4_7 <- data.frame(x = c(4), 
                    y = c(7),
                    col1 = c("G"), 
                    col2 = c("G"),
                    col3 = c("G"),
                    col4 = c("P"),
                    col5 = c("G"),
                    col6 = c("G"))
df <- rbind(df, c_4_7)

c_5_7 <- data.frame(x = c(5), 
                    y = c(7),
                    col1 = c("G"), 
                    col2 = c("G"),
                    col3 = c("G"),
                    col4 = c("P"),
                    col5 = c("G"),
                    col6 = c("G"))
df <- rbind(df, c_5_7)

p1 <- ((0) + (.16677)) / 2
p2 <- ((.16677) + (.16677 + .16677)) / 2
p3 <- ((.16677 + .16677) + (.16677 + .16677 + .16677)) / 2
p4 <- ((.16677 + .16677 + .16677) + (.16677 + .16677 + .16677 + .16677)) / 2
p5 <-
  ((.16677 + .16677 + .16677 + .16677) + (.16677 + .16677 + .16677 + .16677 + .16677)) / 2
p6 <-
  ((.16677 + .16677 + .16677 + .16677 + .16677) + (.16677 + .16677 + .16677 + .16677 + .16677 +
                                                     .16677)) / 2

plotted1 <- data.frame(x = p1, y = 7, color = df$col1)
plotted2 <- data.frame(x = p1, y = 7, color = df$col2)
plotted3 <- data.frame(x = p3, y = 7, color = df$col3)
plotted4 <- data.frame(x = p4, y = 7, color = df$col4)
plotted5 <- data.frame(x = p5, y = 7, color = df$col5)
plotted6 <- data.frame(x = p6, y = 7, color = df$col6)

plotted <- rbind(plotted1, plotted2, plotted3, plotted4, plotted5, plotted6)


