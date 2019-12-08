library(gtools)


source('amp_program.R')
source('computer_with_input.R')


sequences <- permutations(5, 5, v=0:4)
largest_output <- 0
for (i in 1:nrow(sequences)) {
  s <- sequences[i, ]
  message(sprintf('###############'))
  message(sprintf('SEQUENCE: %s (%s)', i, paste(s, collapse = ', ')))
  message(sprintf('###############'))
  input <- 0
  for (j in 1:length(s)){
    
    module_has_finished <- FALSE
    pointer_position <- 1
    module_program <- amp_program
    while (module_has_finished == FALSE){
      program_result <- computer_with_input(module_program, pointer_position, 
                                            c(s[j], input))
      input <- program_result$current_output
      module_has_finished <- program_result$finished
      module_program <- program_result$output_program
      pointer_position <- program_result$current_pointer
    }
    
  }
  if(input > largest_output) {
    largest_output <- input
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

