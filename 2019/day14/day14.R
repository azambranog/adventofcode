source('helpers.R')

reactions_raw <- readLines('reactions.txt')

reactions <- create_reactions_book(reactions_raw)

ore_per_fuel <- function(fuel_needed) {
  # task1
  need <- reactions$lhs['FUEL', ]*fuel_needed
  reserve <- need
  reserve[reserve >0] <- 0
  
  while (!((need['ORE'] != 0) & (sum(need>0)==1))) {
    
    x <- ceiling(need/reactions$rhs)
    x['ORE'] <- need['ORE']
    
    reserve <- reserve + x*reactions$rhs - need 
    reserve['ORE'] <- 0
    
    
    need <-  apply(reactions$lhs * t(x), 2, sum)
    need <- pmax(need - reserve, 0)
    reserve <- pmax(reserve - need, 0)
  }
  
  message(sprintf('For %s FUEL you need %s ORE', fuel_needed, need['ORE']))
  return(need[1, 'ORE'])
}

ore_per_fuel(1)

#task2
fuel_per_ore <- function(ore) {
  base_ore <- ore_per_fuel(1)
  try_fuel <- ceiling(ore/base_ore)
  
  best_fuel <- 0
  correction <- 100
  while (correction != 0){
    
    ore_need <- ore_per_fuel(try_fuel)
    
    correction <- ceiling((ore - ore_need)/base_ore)
    
    if (correction > 0) {
      best_fuel <- max(best_fuel, try_fuel)
    }
    
    try_fuel <- try_fuel + correction
    
  }
  
  message('see last two exceucutions')
  
}



fuel_per_ore(1000000000000)








