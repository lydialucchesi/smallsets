
fts
check

tab1 <-
  as.data.frame(ftsList[[1]][[1]]$body$styles$text$color$data)
xs <-
  data.frame(variable = colnames(tab), x = seq(1, length(colnames(tab)), 1))

for (i in 1:ncol(tab)) {
  tab[, i] <- as.character(tab[, i])
}
tab$y <- seq(nrow(tab), 1,-1)
tabLong <- reshape2::melt(tab, id = c("y"))
tabLong <- merge(tabLong, xs)
tabLong$plotID <- j
tabLong$pointID <- seq(1, nrow(tabLong), 1)
print(head(tabLong))


allDat <- data.frame()
for (j in 1:length(ftsList[[1]])) {
  
  print(j)
  tab <-
    as.data.frame(ftsList[[1]][[j]]$body$styles$text$color$data)
  
  xs <-
    data.frame(variable = colnames(tab), x = seq(1, length(colnames(tab)), 1))
  
  for (i in 1:ncol(tab)) {
    tab[, i] <- as.character(tab[, i])
  }
  tab$y <- seq(nrow(tab), 1,-1)
  tabLong <- reshape2::melt(tab, id = c("y"))
  tabLong <- merge(tabLong, xs)
  tabLong$plotID <- j
  print(head(tabLong))
  
  allDat <- rbind(allDat, tabLong)
}


for (n in 1:6) {
  
}


portions <- 1 / max(allDat$plotID)




# arrays?

idList <- list()

ftsList[[1]][[1]]$body$dataset
ftsList[[1]][[1]]$body$styles$text$color$data

ids <- matrix(data = seq(1, nrow(ftsList[[1]][[1]]$body$dataset) * ncol(ftsList[[1]][[1]]$body$dataset)),
              nrow(ftsList[[1]][[1]]$body$dataset),
              ncol(ftsList[[1]][[1]]$body$dataset))

idList[[1]] <- ids

ftsList[[1]][[2]]$body$dataset
ftsList[[1]][[2]]$body$styles$text$color$data
ids <- matrix(data = seq(1, nrow(ftsList[[1]][[2]]$body$dataset) * ncol(ftsList[[1]][[2]]$body$dataset)),
              nrow(ftsList[[1]][[2]]$body$dataset),
              ncol(ftsList[[1]][[2]]$body$dataset))



idList[[2]] <- ids

ftsList[[1]][[3]]$body$dataset
ftsList[[1]][[3]]$body$styles$text$color$data
ids <- matrix(data = seq(1, nrow(ftsList[[1]][[3]]$body$dataset) * ncol(ftsList[[1]][[3]]$body$dataset)),
              nrow(ftsList[[1]][[3]]$body$dataset),
              ncol(ftsList[[1]][[3]]$body$dataset))



ftsList[[1]][[4]]$body$dataset
ftsList[[1]][[4]]$body$styles$text$color$data
ids <- matrix(data = seq(1, nrow(ftsList[[1]][[4]]$body$dataset) * ncol(ftsList[[1]][[4]]$body$dataset)),
              nrow(ftsList[[1]][[4]]$body$dataset),
              ncol(ftsList[[1]][[4]]$body$dataset))




trackList <- list()
trackList[[1]] <- ftsList[[1]][[1]]$body$styles$text$color$data
trackList[[2]] <-  ftsList[[1]][[2]]$body$styles$text$color$data
trackList[[3]] <- ftsList[[1]][[3]]$body$styles$text$color$data
trackList[[4]] <- ftsList[[1]][[4]]$body$styles$text$color$data
trackList[[5]] <- ftsList[[1]][[5]]$body$styles$text$color$data
trackList[[6]] <- ftsList[[1]][[6]]$body$styles$text$color$data






j4 <- subset(allDat, plotID == 4)
j5 <- subset(allDat, plotID == 5)

cell_1_1 <- subset(allDat, y == 1 & x == 1)














j1 <- subset(allDat, plotID == 1)
j1$plotID <- NULL
names(j1)[names(j1) == 'value'] <- 'col1'

j2 <- subset(allDat, plotID == 2)

js <- merge(j1, j2)
names(js)[names(js) == 'value'] <- 'col2'
js$plotID <- NULL

j3 <- subset(allDat, plotID == 3)

js <- merge(js, j3)
names(js)[names(js) == 'value'] <- 'col3'
js$plotID <- NULL

j4 <- subset(allDat, plotID == 4)

j4$x <- ifelse(j4$x > 2, j4$x + 1, j4$x)

js <- left_join(js, j4)
names(js)[names(js) == 'value'] <- 'col4'
js$plotID <- NULL





j1$col1 <- 

trackList[[1]]



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

c_1_ <- data.frame(x = numeric(1), 
                    y = numeric(7),
                    col1 = character("G"), 
                    col2 = character("G"),
                    col3 = character("G"),
                    col4 = character("G"),
                    col5 = character("G"),
                    col6 = character("G"))


