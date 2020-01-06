mi_weights <- function(data, vars_bl, w_spec, num_m, mi_method = 'norm', 
                       n_iter = 20, trunc_range = TRUE){
  
  for (wnum in seq_along(w_spec)){
    data[, sprintf('w_%02d', wnum)] <- ifelse(data[, 'miss'] == 1, 
                                              NA, 
                                              data[, sprintf('w_%02d', wnum)])  
    
    data[, sprintf('u_%02d', wnum)] <- NULL
    data[, sprintf('w_%02d_norm', wnum)] <- NULL
    data[, sprintf('uw_%02d_norm', wnum)] <- NULL
  }
  
  data[, 'mcda'] <- NULL
  
  predM <- mice::make.predictorMatrix(data)
  predM[, names(data)[!names(data) %in% c(grep('^w_', names(data), value = T), vars_bl)]] <- 0
  
  if (trunc_range){
    post <- mice::make.post(data) 
    for (wnum in seq_along(w_spec)){
      post[sprintf('w_%02d', wnum)] <- 
        "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(0, 100))"
    }
    
    mice_out <- mice::mice(data =  data, 
                           m = num_m,
                           predictorMatrix = predM,
                           method = mi_method,
                           maxit = n_iter,
                           printFlag = FALSE,
                           post = post)
  }
  else {
    mice_out <- mice::mice(data =  data, 
                           m = num_m,
                           predictorMatrix = predM,
                           method = mi_method,
                           maxit = n_iter,
                           printFlag = FALSE)

  }
  
  
  per_m_sum <- tibble::tibble(m = seq(1, num_m, 1))
  for( i in 1:num_m){
    dt_tmp <- mice::complete(mice_out, i)
    dt_tmp <- pvf_apply(data = dt_tmp, w_spec =  w_spec)
    
    per_m_sum[i, 'qhat'] <- mean(dt_tmp$mcda[dt_tmp$trt=='c']) - mean(dt_tmp$mcda[dt_tmp$trt=='t'])
    per_m_sum[i, 'u'] <- var(dt_tmp$mcda[dt_tmp$trt=='c'])/nrow(subset(dt_tmp, trt=='c')) + 
      var(dt_tmp$mcda[dt_tmp$trt=='t'])/nrow(subset(dt_tmp, trt=='t'))
    
    
  }
  
  mi_res <- tibble::tibble(qbar = mean(per_m_sum$qhat),
                           ubar = mean(per_m_sum$u),
                           b = var(per_m_sum$qhat),
                           t = ubar + (1 + 1/nrow(per_m_sum))*b,
                           v = (nrow(per_m_sum) - 1)*(t/((1 + 1/nrow(per_m_sum))*b))^2)
  
  return(mi_res)
  
  
}

