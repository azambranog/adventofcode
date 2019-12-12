library(data.table)

apply_gravity <- function(moons) {
  n_moons <- nrow(moons)
  
  base_mat <- matrix(0, nrow=n_moons, ncol=n_moons)
  gravity <- rep(list(base_mat), 3)
  
  for (ax in 1:3) {
    for (i in 1:n_moons) {
      for (j in i:n_moons) {
        gravity[[ax]][i, j] <-  sign(moons[i, ax] - moons[j, ax])
        gravity[[ax]][j, i] <- -gravity[[ax]][i, j]
      }
    }
  }
  vel_corrections <- sapply(gravity, function(x) apply(x, 2, sum))
  moons[, 4:6] <- moons[, 4:6] + vel_corrections
  moons[, 1:3] <- moons[, 1:3] + moons[, 4:6]
  
  return(moons)
}

calculate_energy <- function(moons) {
  pot <- apply(moons[, 1:3], 1, function(x) sum(abs(x)))
  kin <- apply(moons[, 4:6], 1, function(x) sum(abs(x)))
  e <- pot*kin
  return(sum(e))
}




moons <- readLines('moons.txt')

x <- as.numeric(gsub('<x=(.+), y=(.+), z=(.+)>', '\\1', moons))
y <- as.numeric(gsub('<x=(.+), y=(.+), z=(.+)>', '\\2', moons))
z <- as.numeric(gsub('<x=(.+), y=(.+), z=(.+)>', '\\3', moons))

moons <- data.frame(x = x, y=y, z=z, vx=0, vy=0, vz=0)



#task1
steps <- 1000
for (s in 1:steps) {
  if (s%%100 == 0){
    message(sprintf('Step: %s', s))
  } 
  moons <- apply_gravity(moons)
}
e <- calculate_energy(moons)
message(sprintf('The Total energy after step %s is %s', s, e))



