library(data.table)

asteroids <- readLines('map.txt')

# task1
asteroids <- strsplit(asteroids, '')

positions <- lapply(asteroids, function(x) data.table(x=which(x!='.')-1))

lapply(as.list(1:length(positions)), function(i) positions[[i]][, y := (i-1)])

positions <- rbindlist(positions)

paths <- vector('list', nrow(positions))
for (i in 1:length(paths)) {
  paths[[i]] <- positions[-i, ]
  paths[[i]][, alpha := atan2((y - positions[i, y]),(x - positions[i, x]))]
 
}


i <- which.max(sapply(paths, function(x) nrow(unique(x[, .(alpha)]))))
m <- max(sapply(paths, function(x) nrow(unique(x[, .(alpha)]))))
message(sprintf('Best point is: (%s, %s).\nView to %s asteroids', 
        positions[i, x], positions[i, y], m))



#task2
best_loc_paths <- copy(paths[[i]])
#need to flip the coordinate system
best_loc_paths[, alpha := alpha + (pi/2)]
best_loc_paths[alpha < 0 , alpha := alpha +(2*pi)]
best_loc_paths[, dist := sqrt((x - best_loc$x)^2 + (y - best_loc$y)^2)]

best_loc_paths[, round := order(dist), by = alpha]

best_loc_paths <- best_loc_paths[order(round, alpha)]

best_loc_paths[, destroyed := .I]

x <- best_loc_paths[200, x]
y<- best_loc_paths[200, y]
message(sprintf('The 200th destroyed is: (%s, %s)', x, y))
message(sprintf('The solution is: %s', 100*x + y))

