library(data.table)

asteroids <- readLines('map0.txt')

asteroids <- strsplit(asteroids, '')

positions <- lapply(asteroids, function(x) data.table(x=which(x=='#')-1))
lapply(as.list(1:length(positions)), function(i) positions[[i]][, y := i-1])

positions <- rbindlist(positions)

paths <- vector('list', nrow(positions))
for (i in 1:length(paths)) {
  paths[[i]] <- positions[-i, ]
  paths[[i]][, m := (y - positions[i, y])/(x - positions[i, x])]
  paths[[i]][, b := y - (m * x)]
}


# task1
which.max(sapply(paths, function(x) nrow(unique(x[, .(m, b)]))))

