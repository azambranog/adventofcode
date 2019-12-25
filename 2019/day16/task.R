input <- readLines('input.txt')

#task1
run_phases <- function(signal, max_phases) {
  base_pattern <- c(0, 1, 0, -1)
  signal <- strsplit(signal, '')[[1]]
  signal <- as.numeric(signal)
  n <- length(signal)
  phase <- 0
  while(phase < max_phases){
    phase <- phase +1
    
    output_signal <- numeric(n)
    for(i in 1:n){
      pattern <- rep(base_pattern, each=i, length.out = n+1)
      pattern <- pattern[2:(n+1)]

      output_signal[i] <- abs(sum(pattern*signal))%%10
    }
    signal <- output_signal
    
    result <- paste(signal[1:8], collapse ='')
    message(sprintf('After phase %s -> %s ...',  phase, result))
  }
  return(result)
}

output <- run_phases(input, 100)


#task2
run_phases_with_offset <- function(signal, max_phases) {
  signal <- paste0(rep(input, 10000), collapse='')
  signal <- strsplit(signal, '')[[1]]
  offset <- as.numeric(paste0(signal[1:7], collapse = ''))
  signal <- as.numeric(signal)
  
  signal <- signal[(offset+1):length(signal)]
  signal <- as.numeric(signal)

  n <- length(signal)
  
  phase <- 0
  while(phase < max_phases){
    
    phase <- phase +1
    
    signal <- cumsum(signal[n:1])[n:1] %% 10
    
    result <- paste(signal[1:8], collapse ='')
    message(sprintf('After phase %s -> %s ...',  phase, result))
  }
  return(result)
}


output2 <- run_phases_with_offset(paste0(rep(input, 10000), collapse=''), 100)
