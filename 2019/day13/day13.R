library(data.table)

source(file.path('..', 'day11', 'computer_with_input.R'))
source('game_programm.R')


run_game <- function() {
  program <- list(finished = F, current_output = NA, current_pointer = 1, 
                  output_program = game_programm, current_relative_base = 0)
  
  draw <- data.table(x = numeric(0), y = numeric(0), type = numeric(0))
  
  score <- 0
  
  while(!program$finished){
    
    step <- list(x=0, y=0, type=0)
    for (s in c('x', 'y', 'type')){
      suppressMessages(
        program <- computer_with_input(program$output_program, 
                                       program$current_pointer, 
                                       program$current_relative_base, 
                                       color, stopmode=T)
      )
      step[[s]] <- program$current_output
    }
    if (program$finished) {
      break
    } else if (step$x == -1 & step$y == 0) {
      score <- step$type
    } else {
      draw <- unique(rbind(draw, as.data.table(step)), by = c('x', 'y'), fromLast = TRUE)
    }
  }
  return(list(board = draw, score = score))
}


draw_board <- function(board) {
  lis <- rep('', max(board$y) + 1)
  for(i in min(board$y):max(board$y)){
    li <- rep(' ', max(board$x) + 1)
    for(j in min(board$x):max(board$x)){
      type <- board[y == i & x == j, type]
      if (type == 1) {li[j+1] <- '#'}
      if (type == 2) {li[j+1] <- 'x'}
      if (type == 3) {li[j+1] <- '_'}
      if (type == 4) {li[j+1] <- 'o'}
    }
    lis[i+1] <- paste0(li, collapse = '')
  }
  cat(paste0(lis, collapse = '\n'))  
}
draw_board(results$board)



# task1
results <- run_game()
message(sprintf('There are %s tiles on the board', results$board[type == 2, .N]))
draw_board(results$board)
