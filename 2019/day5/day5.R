source('data.R')


computer <- function(x){
  y <- x
  pointer <- 1
  opcode <- y[pointer] %% 100
  
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
      input <- as.numeric(readline('Give me a parameter: '))
      y[y[pointer + 1] + 1] <- input
      pointer <- pointer + 2
    } 
    else if (opcode == 4) {
      message(sprintf('Value at position %s is: %s', y[pointer + 1], 
                      y[y[pointer + 1] + 1]))
      pointer <- pointer + 2
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
  
  message('DONE')
  
  return(y)
}



output_program <- computer(data)
