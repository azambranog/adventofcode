source(file.path('..', 'day13', 'computer_with_input.R'))
source('repair_program.R')
library(data.table)
library(ggplot2)

calculate_best_input <- function(my_map, optimal, position) {
  types <- vector('list', 4)
  types[[1]] <- my_map[x == position[1] & y == position[2] + 1, type] 
  types[[2]] <- my_map[x == position[1] & y == position[2] - 1, type] 
  types[[3]] <- my_map[x == position[1] - 1 & y == position[2], type] 
  types[[4]] <- my_map[x == position[1] + 1 & y == position[2], type] 
  
  non_visited <- which(sapply(types, length) == 0)

  if(length(non_visited) == 1) {
    next_step <- non_visited
    backstep <- FALSE
  } else if (length(non_visited) != 0) {
    next_step <- sample(non_visited, 1)
    backstep <- FALSE
  } else {
    prev_pos <- optimal[nrow(optimal)-1, ]
    prev_pos <- c(prev_pos[1, x], prev_pos[1, y])
    delta <- prev_pos - position
    if (all(delta == c(0,1))){
      next_step <- 1
    } else if (all(delta == c(0,-1))){
      next_step <- 2
    } else if (all(delta == c(-1,0))){
      next_step <- 3
    } else if (all(delta == c(1,0))){
      next_step <- 4
    }
    backstep <- T
  }
  result <- list(next_step=next_step, backstep=backstep)
  return(result)
}

robot <- function(mission=c('explore', 'find'),
                  max_iter=Inf, verbose=F, report_every=100, show_plot=F, 
                  step_by_step=F) {
  
  program <- list(finished = F, current_output = NA, current_pointer = 1, 
                  output_program = repair_program, current_relative_base = 0,
                  waiting_input =F)
  
  position <- c(0, 0)
  results <- data.table(x=0, y=0, type=1)
  optimal <- data.table(x=0, y=0)
  input <- NA
  i <- 0
  message('START MISSION: ', toupper(mission))
  while(!program$finished){
    if(i %% report_every == 0) {
      message('STEP ', i)
    }
    next_step <- calculate_best_input(results, optimal, position)
    input <- next_step$next_step
    
    
    suppressMessages(
     program <- computer_with_input(program$output_program, 
                                      program$current_pointer, 
                                      program$current_relative_base, 
                                      c(input), stopmode=T)
    )
    
    if (program$produced_output){
      output <- program$current_output
      
      if(input == 1) {
        test_position <- position + c(0,1)
      } else if (input == 2) {
        test_position <- position + c(0,-1)
      } else if (input == 3) {
        test_position <- position + c(-1,0)
      } else if (input == 4) {
        test_position <- position + c(1,0)
      }
      
      if(nrow(results[x == test_position[1] & y == test_position[2], ]) == 0){
        results <- rbindlist(list(results, 
                                  data.table(x=test_position[1], 
                                             y=test_position[2], 
                                             type=output)))
      } 
      

     if (output == 0) {
       if(verbose) {
         message(sprintf('------ ROBOT hit a wall: (%s, %s)', 
                         test_position[1], test_position[2]))
       }
     } else if (output == 1) {
       position <- test_position
       if (next_step$backstep) {
         optimal <- head(optimal, -1)
         if(nrow(optimal) == 1) {
           message('------ FULL MAP DISCOVERED')
           break
         }
           
       } else {
         optimal <- rbindlist(list(optimal, 
                                   data.table(x=position[1], y=position[2])))
       }
       if (verbose) {
         message(sprintf('------ ROBOT moved to: (%s, %s)', 
                         position[1], position[2]))
       }
     } else if (output == 2) {
       position <- test_position
       optimal <- rbindlist(list(optimal, 
                                 data.table(x=position[1], y=position[2])))
       if(verbose) {
         message(sprintf('------ ROBOT moved to: (%s, %s) -- TANK found', 
                         position[1], position[2]))
       }
       if (mission == 'find') {
         break
       }
     }
    }
    
    if(show_plot){
      if(!is.null(dev.list())) {
        dev.off()
        }
      p <- ggplot() +
        geom_tile(data = results, aes(x=x, y = y, fill = factor(type)), color ='black') +
        geom_tile(data = optimal, 
                  aes(x=x, y = y), fill = 'green', color ='black') +
        geom_point(aes(x=position[1], y = position[2])) +
        theme_bw() +
        coord_fixed() + 
        theme(legend.position = 'none')
      print(p)
    }
    if(step_by_step) {
      if (readline('q to stop, enter to continue > ')=='q')break
    }
    
    i <- i +1
    if (i == max_iter) {
      message('MAX ITERATIONS REACHED')
      break
    }
  }
  
  message('ROBOT DONE')
  if (output != 2) {
    message('Did not find oxygen')
  } else {
    message(sprintf('Found oxygen after: %s steps', i))
  }
  
  p <- ggplot() +
    geom_tile(data = results, aes(x=x, y = y, fill = factor(type)), color ='black') +
    geom_tile(data = optimal, 
              aes(x=x, y = y), fill = 'green', color ='black') +
    geom_point(aes(x=position[1], y = position[2])) +
    theme_bw() +
    coord_fixed() + 
    theme(legend.position = 'none')
  print(p)
  
  message(sprintf('Optimal path in: %s steps', nrow(optimal)-1))
  return(list(map=results, optimal_path=optimal))
}

fill_oxygen <- function(my_map, report_every=100, plotmode =F){
  route <- my_map[type != 0]
  route[, filled := F]
  
  current_points <- route[type == 2, .(x, y)]
  
  step <- -1
  while(!all(route[, filled])){
    if (step %% report_every == 0) {
      message(sprintf('Step %s - %s places to fill', 
                      step, route[filled==F, .N]))
    }
    step <- step +1
    next_points<- data.table(x=numeric(0), y=numeric(0))
    for (i in 1:nrow(current_points)){
      cx <- current_points[i, x]
      cy <- current_points[i, y]
      
      route[x == cx & y == cy, filled := T]
      
      next_points <- rbindlist(list(next_points, 
                                    route[((x==cx+1 & y==cy) | 
                                            (x==cx-1 & y==cy) |
                                            (x==cx & y==cy+1) | 
                                            (x==cx & y==cy-1)) & filled == F,
                                          .(x, y)])
                               )
    }
    next_points <- unique(next_points)
    current_points <- copy(next_points)
    if(plotmode){
      if(!is.null(dev.list())) {
        dev.off()
      }
      p <- ggplot(route) +
        geom_tile(aes(x=x, y = y, fill = factor(filled)), color ='black') +
        geom_point(data = current_points, aes(x=x, y = y)) +
        theme_bw() +
        coord_fixed() + 
        theme(legend.position = 'none') +
        ggtitle(sprintf('step %s', step))
      print(p)
      if (readline('q to quit > ')=='q') {break}
    }
  }
  
  message(sprintf('FILLED AFTER %s STEPS', step))
  return(step)
}

# task 2
res <- robot(mission='find',report_every = 500)

#task 2
full_map <- robot(mission='explore',report_every = 500)
fill_time <- fill_oxygen(full_map$map)


