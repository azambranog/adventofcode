source('ascii_program.R')
source(file.path('..', 'day13', 'computer_with_input.R'))


source(file.path('..', 'day13', 'computer_with_input.R'))

map2mat <- function(map) {
  x <- lapply(map, unlist)
  x <- x[!sapply(x, is.null)]
  x <- matrix(unlist(x), nrow = length(x), byrow = T)
  return(x)
}

print_map <- function(mat_map) {
  x <- apply(mat_map, 1, function(x)paste0(x, collapse=''))
  x <- paste0(x, collapse = '\n')
  cat(x)
}

calculate_calibration <- function(mat_map, verbose = F){
  cross <- matrix(c(F,T,F,T,T,T,F,T,F), nrow=3, byrow = T)
  n_col <- ncol(mat_map)
  n_row <- nrow(mat_map)
  calib <- 0
  
  for (i in 2:(n_col-1)){
    for (j in 2:(n_row-1)) {
      block <- mat_map[(j-1):(j+1), (i-1):(i+1)] != '.'
      if (all(block == cross)){
        if (verbose){
          message(sprintf('Intersection -> (%s, %s)', j-1, i-1))
          
        }
        calib <- calib + ((i-1)*(j-1))
      }
    }
  }
  return(calib)
}

cameras <- function() {
  program <- list(finished = F, current_output = NA, current_pointer = 1, 
                  output_program = ascii_program, current_relative_base = 0,
                  waiting_input =F)
  
  map <- list(list())
  ro <- 1
  co <- 1
  while (!program$finished) {
    
    suppressMessages(
      program <- computer_with_input(program$output_program, 
                                     program$current_pointer, 
                                     program$current_relative_base, 
                                     c(input), stopmode=T)
    )
    
    if (program$produced_output) {
      output <- intToUtf8(program$current_output)

      if(output == '\n'){
        ro <- ro + 1
        co <- 1
        map[[ro]] <- list()
      } else {
        map[[ro]][co] <- output
        co <- co +1
      }
    }
  }
  
  mat_map <- map2mat(map)
  print_map(mat_map)
  message('\n\n CAMERA DONE')
  return(mat_map)
}


calculate_path <- function(map){
  robot <- map[!map  %in% c('.', '#')]
  position <- which(map==robot, arr.ind = T)[1, 2:1]
  orientation <- get_orientation(robot)
  
  out_seq <- list()
  i <- 0
  end <- F
  while (!end) {
    end <- T
    i <- i + 1
    next_step <- sum_positions(position, orientation)

    if (position_is_valid(next_step, map)) {
      if (map[next_step[2], next_step[1]] == '#') {
        steps <- 0
        while (map[next_step[2], next_step[1]] == '#') {
          steps <- steps + 1
          position <- next_step
          next_step <- sum_positions(position, orientation)
          if(!position_is_valid(next_step, map)) {
            break
          }
        }
        out_seq[i] <- as.character(steps)
        end <- F
        next
      }
    }
    
    
    for (o in c('L', 'R')){
      test_orient <- rotate(orientation, direction=o)
      test_pos <- sum_positions(position, test_orient)
      if (position_is_valid(test_pos,map)){
        if (map[test_pos[2], test_pos[1]] == '#') {
          out_seq[i] <- o
          orientation <- test_orient
          end <- F
          break
        }
      }
    }
    
  }
  
  return(unlist(out_seq))
  
}

get_orientation <- function(robot) {
  if(robot == '^') {
    orient <- c(0, 1)
  } else if(robot == '<') {
    orient <- c(-1, 0)
  } else if(robot == '>') {
    orient <- c(1, 0)
  } else if(robot == 'v') {
    orient <- c(0, -1)
  }
  return(orient)
}

rotate <- function(orientation, direction=c('L', 'R')) {
  if (direction == 'L') {
    res <- c(-orientation[2], orientation[1]) 
  } else if(direction == 'R') {
    res <- c(orientation[2], -orientation[1]) 
  }
  return(res)
}

sum_positions <- function(x, y) {
  res <- c(0,0)
  res[1] <- x[1] + y[1]
  res[2] <- x[2] - y[2]
  return(res)
}

position_is_valid <- function(pos, map) {
  
  return((pos[1] <= ncol(map)) &
    (pos[1] >= 1) &
    (pos[2] <= nrow(map)) &
    (pos[2] >= 1))
}

send_robot <- function(sequence, A, B, C, videofeed=F) {
  
  program <- list(finished = F, current_output = NA, current_pointer = 1, 
                  output_program = ascii_program, current_relative_base = 0,
                  waiting_input =F, produced_output=F)
  program$output_program[1] <- 2
  
  inputs <- list(str_2_ascii(sequence), str_2_ascii(A), 
              str_2_ascii(B), str_2_ascii(C))
  if(videofeed) {
    inputs[[5]] <- str_2_ascii('y')
  } else {
    inputs[[5]] <- str_2_ascii('n')
  }
  
  ix <- 0
  input <- NA
  while (!program$finished) {
    
    if (program$waiting_input) {
      ix <- ix + 1
      if (ix <= length(inputs)) {
        input <- inputs[[ix]]
      }
    }  else {
      input <- NA
    }
    

    suppressMessages(
      program <- computer_with_input(program$output_program, 
                                     program$current_pointer, 
                                     program$current_relative_base, 
                                     input, stopmode=T)
    )
    
    if (program$produced_output) {
      output <- program$current_output
      
      cat(intToUtf8(output))
    }
    
    
  }
  
  message('\n\n ROBOT DONE')
  message('DUST COLLECTED: ', output)
  return(output)
  
}

str_2_ascii <- function(x) {
  return(sapply(c(strsplit(x, '')[[1]], '\n'), utf8ToInt))
}

#task_1
map <-cameras()
calib <- calculate_calibration(map)
message(sprintf('CALIBRATION VALUE: %s', calib))

#task_2
path <- calculate_path(map)
A <- 'R,12,L,6,R,12'
B <- 'L,8,L,6,L,10'
C <- 'R,12,L,10,L,6,R,10'
path_2 <- paste0(path, collapse = ',')
path_2 <- gsub(A, 'A', path_2)
path_2 <- gsub(B, 'B', path_2)
path_2 <- gsub(C, 'C', path_2)

dust <- send_robot(path_2, A, B, C, videofeed = F)

