library(data.table)

r <- c(265275, 781584)

pos <- as.character(r[1]:r[2])

#rep digits
pos <- pos[grepl('(.)\\1', pos)]

pos <- strsplit(pos, '')
pos <- lapply(pos, as.numeric)
#always ascending (good song with funky sax)
result <- sapply(pos, function(x) all((x - shift(x)) >= 0, na.rm = T))
sum(result)


result2 <- sapply(pos[result], function(x){
  dec <- x - shift(x)
  dec <- paste(dec[-1], collapse = '')
  #lonely pairs
  return(grepl('([^0]0[^0])|([^0]0$)|(^0[^0])', dec))
})
sum(result2)
