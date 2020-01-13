assign_weights <- function(data, w_spec){
  
  
  dt_w <- data
  
  for(i in seq_along(w_spec)){
    
    dt_w <- suppressMessages(dplyr::left_join(dt_w, w_spec[[i]][ , !(names(w_spec[[i]]) %in% c('p100_weights'))]))
    
    for (id in seq_len(nrow(dt_w))){
      
      dt_w[id, sprintf('w_%02d', i)] <- round(100*stats::rbeta(n = 1, 
                                                               shape1 = fun_a(mu_w = dt_w[['w_mu']][id]/100,
                                                                              sd_w = dt_w[['w_sd']][id]/100),
                                                               shape2 = fun_b(mu_w = dt_w[['w_mu']][id]/100,
                                                                              sd_w = dt_w[['w_sd']][id]/100)), 0) 
      
      
    }
    
    dt_w$br <- dt_w$wname <- dt_w$w_mu <- dt_w$w_sd <- NULL
    
  }
  
  for (id in seq_len(nrow(dt_w))){
  
    dt_w[id, 'max_w'] <- max(dt_w[id, grep('^w_', names(dt_w), value = T)])
    
  
  }
  
  for(i in seq_along(w_spec)){
  
    for (id in seq_len(nrow(dt_w))){
      dt_w[id, sprintf('w_%02d', i)] <- ifelse(dt_w[id, sprintf('w_%02d', i)] == dt_w[id, 'max_w'],
                                               100,
                                               dt_w[id, sprintf('w_%02d', i)])
      
    }
    
    
  }
  
  dt_w$max_w <- NULL
  return(dt_w)
}
