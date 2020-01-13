pvf_apply <- function(data, w_spec){
  
  for (wnum in seq_along(w_spec)){
    
    data[, sprintf('u_%02d', wnum)] <- pvf(var = data[, w_spec[[wnum]]$wname[[1]]],
                                               high = stats::quantile(data[,w_spec[[wnum]]$wname[[1]]][[1]], 0.975),
                                               low = stats::quantile(data[, w_spec[[wnum]]$wname[[1]]][[1]], 0.025),
                                               type = w_spec[[wnum]]$br[[1]])
    data[, sprintf('w_%02d_norm', wnum)] <- 
      data[, sprintf('w_%02d', wnum)]/rowSums(data[, grep('^w_', names(data))]) 
    
    data[, sprintf('uw_%02d_norm', wnum)] <- 
      data[, sprintf('w_%02d_norm', wnum)] * data[, sprintf('u_%02d', wnum)]
    
  }
  
  data[, 'mcda'] <- rowSums(data[, grep('^uw_', names(data))])
  return(data)
  
}


