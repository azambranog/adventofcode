pic_raw <- readLines('image_data.txt', warn = F)


pic_raw <- as.numeric(strsplit(pic_raw, '')[[1]])


pic_dims <- c(25,6)
nlayers <- length(pic_raw)/(pic_dims[1] * pic_dims[2]) 
pic <- array(pic_raw, dim=c(pic_dims, nlayers))


# TASK1
least_zeros <- which.min(apply(pic, 3, function(x) sum(x == 0)))
solution <- sum((pic[, ,least_zeros] == 1)) * sum((pic[, ,least_zeros] == 2)) 
message(sprintf('Solution for task 1 is: %s', solution))


#TASK2
final_pic_indexes <- apply(pic, c(1,2), function(l) min(which(l!=2)))
final_pic <- array(' ', dim=dim(final_pic_indexes))

for(i in 1:dim(final_pic)[1]) {
  for(j in 1:dim(final_pic)[2]) {
    if(pic[i, j, final_pic_indexes[i, j]] == 1){
      final_pic[i, j] <- '*'
    }
  } 
}

message('Solution for task 2 is')
write((final_pic), '', sep = '', ncolumns = pic_dims[1])




