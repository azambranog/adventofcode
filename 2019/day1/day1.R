f <- function(x) {
  fuel <- 0
  r <- floor(x/3)-2 
  while(r > 0){
    fuel <- fuel + r
    r <- floor(r/3)-2 
  }
  return(fuel)
}

