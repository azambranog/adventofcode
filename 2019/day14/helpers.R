library(data.table)


eq_to_matrix <- function(l) {
  
  res <- lapply(l, function(x) {
    r <- as.numeric(gsub('^(.+) (.+)$', '\\1', x))
    names(r) <- gsub('^(.+) (.+)$', '\\2', x)
    r <- as.data.table(as.list(r))
    return(r)
  })
  
  res <- rbindlist(res, fill = T)
  res <- data.frame(res)
  res[is.na(res)] <- 0
  return(res)
}

rhs_to_vector <- function(l) {
  res <- sapply(l, function(x) {
    r <- as.numeric(gsub('^(.+) (.+)$', '\\1', x))
    names(r) <- gsub('^(.+) (.+)$', '\\2', x)
    return(r)
  })
  return(res)
}

create_reactions_book <- function(reactions_raw) {
  
  reactions <- strsplit(reactions_raw, ' => ')
  
  rhs <- lapply(reactions, function(x) x[2])
  rhs <- rhs_to_vector(rhs)
  
  lhs <- lapply(reactions, function(x) unlist(strsplit(x[1], ', ')))
  lhs <- eq_to_matrix(lhs)
  
  rownames(lhs) <- names(rhs)
  
  lhs['ORE', 'FUEL'] <- 0
  lhs['ORE', 'ORE'] <- 1
  rhs <- c(rhs, ORE=0) 
  lhs[is.na(lhs)] <- 0

  return(list(rhs = rhs[names(rhs)], lhs = lhs[names(rhs), names(rhs)]))
  
}
