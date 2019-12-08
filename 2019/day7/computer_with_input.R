computer_with_input <- function(x, pointer_position, inputs){
  y <- x
  pointer <- pointer_position
  opcode <- y[pointer] %% 100
  inputs_given <- 0
  current_result <- NA
  finished <- TRUE
  while (opcode != 99){
    if(!opcode %in% 1:8) {
      message('ERROR BAD OPCODE')
      return(NULL) 
    }
    
    #nicestuff to get the operator values
    modes <- numeric(3)
    values <- numeric(3)
    cumval <- y[pointer]
    for (i in length(modes):1) {
      modes[i] <- cumval %/% (10^(1 + i))
      cumval <- cumval - (modes[i]*10^(1+i))
      
      if (modes[i] == 0) {
        values[i] <- y[y[pointer + i] +1]
      } else if (modes[i] == 1) {
        values[i] <- y[pointer + i]
      } else {
        message('ERROR BAD MODE')
        return(NULL)
      }
    }
    
    #operations
    if (opcode == 1){
      y[y[pointer + 3] + 1] <- values[1] + values[2]
      pointer <- pointer + 4
    } 
    else if (opcode == 2) {
      y[y[pointer + 3] + 1] <- values[1] *  values[2]
      pointer <- pointer + 4
    } 
    else if (opcode == 3) {
      inputs_given <- inputs_given + 1
      message('Program requests input')
      message(sprintf('Using input %s: %s', inputs_given, inputs[inputs_given]))
      y[y[pointer + 1] + 1] <- inputs[inputs_given]
      pointer <- pointer + 2
    } 
    else if (opcode == 4) {
      current_result <- y[y[pointer + 1] + 1] 
      message(sprintf('Value at position %s is: %s', y[pointer + 1], current_result))
      message('This is the most current result')
      pointer <- pointer + 2
      break
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
        y[y[pointer + 3] + 1] <- 1
      } else {
        y[y[pointer + 3] + 1] <- 0
      }
      pointer <- pointer + 4
    } 
    else if (opcode == 8) {
      if (values[1] == values[2]) {
        y[y[pointer + 3] + 1] <- 1
      } else {
        y[y[pointer + 3] + 1] <- 0
      }
      pointer <- pointer + 4
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
              current_pointer=pointer, output_program = y))
}


