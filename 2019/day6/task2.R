find_santa <- function(map, from, to){
  x <- data.table(center = gsub('(.+)\\)(.+)','\\1', map),
                  satellite = gsub('(.+)\\)(.+)','\\2', map))
  
  x[, my_path := as.numeric(NA)]
  x[, santa_path := as.numeric(NA)]
  my_position <- from
  santa_position <- to
  
  step <- 0
  while (TRUE) {
    step <- step + 1
    if (step %% 100 == 0) message(step)
    
    if (my_position != 'COM') {
      x[satellite == my_position, my_path := step]
    } 
    if (santa_position != 'COM') {
      x[satellite == santa_position, santa_path := step]  
    }
    
    
    if ((my_position %in% x[!is.na(santa_path), satellite]) |
        (santa_position %in% x[!is.na(my_path), satellite])) {
      break
    }
    
    
    if (santa_position != 'COM') {
      santa_position <- x[satellite == santa_position, center]  
    }
    if (my_position != 'COM') {
      my_position <- x[satellite == my_position, center]
    } 
    
    
  }
  
  my_intersect <- x[(my_path) & !is.na(santa_path), my_path]
  santa_intersect <- x[!is.na(my_path) & !is.na(santa_path), santa_path]
  
  shotest_route <- x[my_path < my_intersect |
                      santa_path < santa_intersect, .N -2]
  
  message(sprintf('The shortest route %s you and %s takes %s steps', 
                  from, to, shotest_route))
  return(shotest_route)
}



map <- readLines('map.txt')
solution <- find_santa(map, 'YOU', 'SAN')



