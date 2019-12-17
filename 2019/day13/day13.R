source('computer_with_input.R')
source('game_program.R')


run_game <- function(play = F, self_play = F) {
  program <- list(finished = F, current_output = NA, current_pointer = 1, 
                  output_program = game_program, current_relative_base = 0, 
                  waiting_input =F)
  if (play) {
    program$output_program[1] <- 2
  }
  
  draw <- list()
  
  score <- 0
  input <- NA

  while(!program$finished){
    
    step <- list(x=0, y=0, type=0)
    for (s in c('x', 'y', 'type')){
      
      suppressMessages(
        program <- computer_with_input(program$output_program, 
                                       program$current_pointer, 
                                       program$current_relative_base, 
                                       c(input), stopmode=T)
      )
      
      if(program$waiting_input) {
        draw_board(list(board = draw, score = score), clear = T)
        if (self_play) {
          ball <- get_ball_pos(draw)
          bar <- get_bar_pos(draw)
          input <- sign(ball-bar)
          break
          
        } else {
          input <- readline('move >')
          if(input == 'a') {input <- -1}
          else if(input %in% c('s', 'l')) {input <- 1}
          else {input <- 0}
          break
        }
      } else {
        input <- NA
      }
      
      step[[s]] <- program$current_output
    }
    if (program$finished) {
      break
    } else if (program$waiting_input) {
      next
    } else if (step$x == -1 & step$y == 0) {
      score <- step$type
    } else {
      if(length(draw) < (step$y + 1)) {
        draw[[step$y + 1]] <- c(as.numeric(NA))
      }
      type <- step$type
      if (type == 0) {type_c <- ' '}
      if (type == 1) {type_c <- '#'}
      if (type == 2) {type_c <- 'x'}
      if (type == 3) {type_c <- '_'}
      if (type == 4) {type_c <- 'o'}
      draw[[step$y + 1]][[step$x + 1]] <- type_c
    }
  }
  draw_board(list(board = draw, score = score), clear = T)
  return(list(board = draw, score = score))
}


draw_board <- function(results, clear = F) {
  d <- sapply(results$board, function(x) paste0(x, collapse = ''))
  d <- paste0(d, collapse = '\n')
  if (clear) {
    cat("\014") 
  }
  cat(d)
  cat(sprintf('\n~~ Score: %s\n', results$score))
}

get_bar_pos <- function(draw) {
  max(sapply(draw, function(x) max(which(x == '_'))))
}

get_ball_pos <- function(draw) {
  max(sapply(draw, function(x) max(which(x == 'o'))))
}

# task1
results <- run_game()
message(sprintf('There are %s tiles on the board', 
                sum(sapply(results$board, function(x) sum(x == 'x')))))


#task2
#results <- run_game(play = T)

#task2 AUTO
results <- run_game(play = T, self_play = T)

