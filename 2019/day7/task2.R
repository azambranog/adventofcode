library(gtools)


source('amp_program.R')
source('computer_with_input.R')

all_modules_finished <- function(ml){
  return(all(sapply(ml, function(x) x$finished)))
}


sequences <- permutations(5, 5, v=5:9)
ini_modules_states <- vector('list', 5)
ini_modules_states <- lapply(ini_modules_states, 
                             function(x)list(finished=FALSE, current_output=NA,
                                             current_pointer=1, 
                                             output_program = amp_program)
                             )



largest_output <- 0
for (i in 1:nrow(sequences)) {
  s <- sequences[i, ]
  message(sprintf('###############'))
  message(sprintf('SEQUENCE: %s (%s)', i, paste(s, collapse = ', ')))
  message(sprintf('###############'))

  signal <- 0
  modules_states <- ini_modules_states
  
  while (!all_modules_finished(modules_states)){
    
    for (j in 1:length(s)){
      message(sprintf('--- Module: %s', LETTERS[j]))
      if (!modules_states[[j]]$finished) {
        
        if (modules_states[[j]]$current_pointer == 1) {
          input_data <- c(s[j], signal)
        } else {
          input_data <- signal
        }
        
        program_result <- computer_with_input(modules_states[[j]]$output_program,
                                              modules_states[[j]]$current_pointer, 
                                              input_data)
        modules_states[[j]] <- program_result
        
      }
      if(!is.na(modules_states[[j]]$current_output)) {
        signal <- modules_states[[j]]$current_output
      }

      message(sprintf('*** SIGNAL: %s', signal))
      
    }
    
  }

  if(signal > largest_output) {
    largest_output <- signal
    largest_sequnece <- s
    message(sprintf('####################'))
    message(sprintf('New optimal sequence found'))
    message(sprintf('Sequence: %s', paste(largest_sequnece, collapse = ', ')))
    message(sprintf('Thrust output: %s', largest_output))
    message(sprintf('####################'))
  }
}

message(sprintf('####################'))
message(sprintf('OPTIMAL'))
message(sprintf('Sequence: %s', paste(largest_sequnece, collapse = ', ')))
message(sprintf('Thrust output: %s', largest_output))
message(sprintf('####################'))

