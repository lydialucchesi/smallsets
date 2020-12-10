
# catchIntervals <- data.frame(min = esnaps[esnaps$type == "catch", "r"])
# for (r in 1:nrow(esnaps)) {
#   if (esnaps$type[r] == "catch") {
#     esnaps$r[r] <- esnaps$r[r + 1] - 1
#   }
# }
# catchIntervals$max <- esnaps[esnaps$type == "catch", "r"]

catch <- FALSE
for (z in 1:nrow(catchIntervals)) {
  if (between(h, catchIntervals$min[z], catchIntervals$max[z])) {
    catch <- TRUE
  }
}

if (isTRUE(catch)) {
  esnapsSUB <- subset(esnaps, esnaps$type == "catch")
} else {
  esnapsSUB <- subset(esnaps, esnaps$type == "external")
}

for (o in eID:nrow(esnaps)) {
  if (h <= esnaps$r[o + 1]) {
    break
  }
}


