
  library(data.table)
  
  wires <- readLines('inputs.txt')
  
  
  wires <- strsplit(wires, ',')
  
  paths <- vector('list', length(wires))
  for (w in 1:length(wires)){
    paths[[w]] <- vector('list', length(wires[[w]]))
    current_x <- 0
    current_y <- 0
    for (d in 1:length(wires[[w]])) {
      direction <- gsub('(.)(.+)', '\\1', wires[[w]][d])
      distance <- as.numeric(gsub('(.)(.+)', '\\2', wires[[w]][d]))
      
      next_x <- current_x
      next_y <- current_y
      if (direction == 'R') {
        next_x <- next_x + distance
      } else if (direction == 'L') {
        next_x <- next_x - distance
      } else if (direction == 'U') {
        next_y <- next_y + distance
      } else if (direction == 'D') {
        next_y <- next_y - distance
      }
      
      xs <- current_x:next_x
      ys <- current_y:next_y
      paths[[w]][[d]] <- data.table(x = xs, y = ys)
      paths[[w]][[d]] <- tail(paths[[w]][[d]], -1)
      
      current_x <- next_x
      current_y <- next_y
    }
    paths[[w]] <- rbindlist(paths[[w]])
    paths[[w]][, step := .I]
  }
  
  intersections <- merge(paths[[1]], paths[[2]], by = c('x', 'y'))
  intersections[, dist_from_center := abs(x) + abs(y)]
  intersections[, total_steps := step.x + step.y]
  
  intersections[order(dist_from_center)]
  intersections[order(total_steps)]


