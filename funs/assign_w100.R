assign_w100 <- function(dt_w_init, w_spec){
  
  
  
  p100_weights <- unique(w_spec[[1]]$p100_weights)
  
  for(i in 2:length(w_spec)){
    p100_weights <- c(p100_weights, unique(w_spec[[i]]$p100_weights))
  }
  
  w_100 <- t(rmultinom(n = nrow(dt_w_init), 1, p100_weights))
  colnames(w_100) <- paste0('w', seq_along(w_spec), '_100')
  dt_w_init <- cbind(dt_w_init, w_100)
  
  for (wnum in seq_along(w_spec)){
    dt_w_init[, sprintf('w_%02d', wnum)] <- ifelse(dt_w_init[, paste0('w', wnum, '_100')] == 1, 
                                                   100,
                                                   dt_w_init[, sprintf('w_%02d', wnum)])
    
  }
  
  dt_w_init <- dt_w_init[, -grep('_100', colnames(dt_w_init))]

  return(dt_w_init)
  
}

