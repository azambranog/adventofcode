source('paint_program.R')
source('computer_with_input.R')
library(data.table)
library(ggplot2)

robot <- function(start_color) {
  
  program <- list(finished = F, current_output = NA, current_pointer = 1, 
                  output_program = paint_program, current_relative_base = 0)
  
  position <- c(0, 0)
  results <- data.table(x=0, y=0, color=start_color)
  pointing <- c(0, 1)
  
  while(!program$finished){
    
    message(sprintf('------- ROBOT PAINTING POSITION: (%s, %s)', position[1], position[2]))
    
    ix <- results[, which(x == position[1] & y == position[2])]
    
    if(length(ix) == 0){
      results <- rbindlist(list(results, data.table(x=position[1], y=position[2], color=0)))
      ix <- nrow(results)
    }
    
    color <- results[ix, color]
    
    
    for (step in c('color', 'move')){
      suppressMessages(
        program <- computer_with_input(program$output_program, 
                                       program$current_pointer, 
                                       program$current_relative_base, 
                                       color, stopmode=T)
      )
      
      if (step == 'color') {
        results[ix, color :=  program$current_output]
      } else if (step == 'move' & !is.na(program$current_output)) {
        if (program$current_output == 0){
          pointing <- c(-pointing[2], pointing[1])
        } else if (program$current_output == 1) {
          pointing <- c(pointing[2], -pointing[1])
        }
        position <- position + pointing
      }
    }
  }
  
  message('ROBOT DONE')
  return(results)
}
 
#task1
results_task_1 <- robot(0)
message('Total panels painted: ', nrow(results_task_1))
#giberish printed
p1 <- ggplot(results_task_1) + 
  geom_tile(aes(x = x, y = y, fill = factor(color)))
print(p1)

#task2
results_task_2 <- robot(1)
message('Total panels painted: ', nrow(results_task_2))
#giberish printed
p2 <- ggplot(results_task_2) + 
  geom_tile(aes(x = x, y = y, fill = factor(color))) +
  coord_fixed() + 
  theme(legend.position = 'none')
print(p2)



