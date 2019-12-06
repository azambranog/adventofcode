source('computer.R')


computer(reset_input(12, 2), verbose=T)[1]


inputs <- expand.grid(noun = 0:99, verb = 0:99)
target <- 19690720
for (i in 1:nrow(inputs)) {
  
  noun <- inputs[i, 'noun']
  verb <- inputs[i, 'verb']
  if (computer(reset_input(noun, verb))[1] == target) {
   message(sprintf('For target %s, noun is %s, verb is %s', target, noun, verb))
   break 
  }
}