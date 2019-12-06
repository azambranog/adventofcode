reset_input <- function(noun, verb){
  x <- c(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,2,19,13,23,1,23,10,27,1,13,27,
         31,2,31,10,35,1,35,9,39,1,39,13,43,1,13,43,47,1,47,13,51,1,13,51,55,1,5,
         55,59,2,10,59,63,1,9,63,67,1,6,67,71,2,71,13,75,2,75,13,79,1,79,9,83,2,83,
         10,87,1,9,87,91,1,6,91,95,1,95,10,99,1,99,13,103,1,13,103,107,2,13,
         107,111,1,111,9,115,2,115,10,119,1,119,5,123,1,123,2,127,1,127,5,0,
         99,2,14,0,0)
  x[2] <- noun
  x[3] <- verb  
  return(x)
}


computer <- function(x, verbose=F){
  y <- x
  pointer <- 1
  opcode <- y[pointer]
  
  while (opcode != 99){
    if(!opcode %in% c(1, 2)) {
      message('ERROR')
      return(NULL) 
    }
    
    op1 <- y[pointer + 1] +1
    op2 <- y[pointer + 2] +1
    dest <- y[pointer + 3] +1
    
    if (opcode == 1){
      y[dest] <- y[op1] + y[op2]
    } else if (opcode == 2) {
      y[dest] <- y[op1] * y[op2]
    }
    
    pointer <- pointer + 4
    opcode <- y[pointer]
   
  }
  if (verbose == T) {
    message('DONE')
    message(sprintf('Position zero is: %s', y[1])) 
  }
  return(y)
}