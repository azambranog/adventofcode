computer_with_input <- function(x, pointer_position, relative_base_position, 
                                inputs, stopmode=F){
  y <- x
  pointer <- pointer_position
  relative_base <- relative_base_position
  opcode <- y[pointer] %% 100
  inputs_given <- 0
  current_result <- NA
  finished <- TRUE
  waiting_input <- FALSE
  produced_output <- FALSE
  while (opcode != 99){
    if(!opcode %in% 1:9) {
      message('ERROR BAD OPCODE')
      return(NULL) 
    }
    
    #nicestuff to get the operator values
    if (opcode %in% c(1,2,7,8)) {
      nargs <- 3
    } else if (opcode %in% c(5,6)) {
      nargs <- 2
    } else if (opcode %in% c(3,4,9)) {
      nargs <- 1
    }
    modes <- numeric(nargs)
    values <- numeric(nargs)
    indexes <- numeric(nargs)
    cumval <- y[pointer]
    for (i in length(modes):1) {
      modes[i] <- cumval %/% (10^(1 + i))
      cumval <- cumval - (modes[i]*10^(1+i))
      
      if (modes[i] == 0) {
        indexes[i] <- y[pointer + i] +1
      } else if (modes[i] == 1) {
        indexes[i] <- pointer + i
      } else if (modes[i] == 2) {
        indexes[i] <- y[pointer + i] + relative_base + 1
      } 
      else {
        message('ERROR BAD MODE')
        return(NULL)
      }
      values[i] <- y[indexes[i]]
    }
    #workaround for "dynamic mem"
    values[is.na(values)] <-0
    
    #operations
    if (opcode == 1){
      y[indexes[3]] <- values[1] + values[2]
      pointer <- pointer + 4
    } 
    else if (opcode == 2) {
      y[indexes[3]] <- values[1] *  values[2]
      pointer <- pointer + 4
    } 
    else if (opcode == 3) {
      if(all(is.na(inputs)) & stopmode){
        waiting_input <- TRUE
        break
      }
      
      inputs_given <- inputs_given + 1
      message('Program requests input')
      message(sprintf('Using input %s: %s', inputs_given, inputs[inputs_given]))
      y[indexes[1]] <- inputs[inputs_given]
      pointer <- pointer + 2
    } 
    else if (opcode == 4) {
      current_result <- y[indexes[1]] 
      message(sprintf('Output: %s', current_result))
      pointer <- pointer + 2
      if(stopmode){
        produced_output <- TRUE
        break
      }
    } 
    else if (opcode == 5) {
      if (values[1] != 0) {
        pointer <- values[2] + 1
      } else {
        pointer <- pointer + 3
      }
    } 
    else if (opcode == 6) {
      if (values[1] == 0) {
        pointer <- values[2] + 1
      } else {
        pointer <- pointer + 3
      }
    } 
    else if (opcode == 7) {
      if (values[1] < values[2]) {
        y[indexes[3]] <- 1
      } else {
        y[indexes[3]] <- 0
      }
      pointer <- pointer + 4
    } 
    else if (opcode == 8) {
      if (values[1] == values[2]) {
        y[indexes[3]] <- 1
      } else {
        y[indexes[3]] <- 0
      }
      pointer <- pointer + 4
    } 
    else if (opcode == 9) {
      relative_base <- relative_base + y[indexes[1]]
      pointer <- pointer + 2
    }
    
    opcode <- y[pointer] %% 100
    
  }
  message(sprintf('Most current result is %s', current_result))
  
  if(y[pointer] %% 100 != 99) {
    finished <- FALSE
    message('PROGRAM IS HALTED')
  }
  message('DONE')
  
  
  return(list(finished=finished, current_output=current_result, 
              current_pointer=pointer, output_program=y,
              current_relative_base=relative_base, waiting_input=waiting_input,
              produced_output=produced_output))
}


