library(data.table)


calculate_total_orbits <- function(map) {
  
  x <- data.table(center = gsub('(.+)\\)(.+)','\\1', map),
                  satellite = gsub('(.+)\\)(.+)','\\2', map))
  
  x[, counts := 1]
  x[, done := F]
  all_centers <- x[, unique(center)]
  x[!satellite %in% all_centers, done := T ]
  i <- 0
  while(x[, sum(done) != .N]){
    i <- i+1
    if (i%%100 == 0) message(i)
    not_done_centers <- x[done == F, unique(center)]
    not_done_sats <- x[done == F, unique(satellite)]
    
    to_close <- setdiff(not_done_sats, not_done_centers)[1]
    to_add <- x[done == T & center == to_close, sum(counts)]
    x[satellite == to_close, counts := counts + to_add]
    x[satellite == to_close, done := T]
    
  }
  
  total_orbits <- x[, sum(counts)]
  message(sprintf('Total orbits: %s', total_orbits))
  return(x)
}


map <- readLines('map.txt')
solution <- calculate_total_orbits(map)


