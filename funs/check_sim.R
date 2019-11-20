check_sim <- function(dt, ch = c('ble1','bls1_c','bls1_t','out','w2')){
  
  if(ch == 'ble1'){
    
    ch_out <- dt%>%
      purrr::map_df(.f = function(x){
        xx <- tibble::tibble(dis_est = x$ch_ble1[2,1],
                             age_est = x$ch_ble1[3,1])
        
      }, .id = 'sim')
    
    }
  if(ch == 'bls1_c'){
  
    ch_out <- dt%>%
      purrr::map_df(.f = function(x){
        xx <- tibble::tibble(dis_est = x$ch_bls1_c[2,1],
                             age_est = x$ch_bls1_c[3,1])
        
      }, .id = 'sim')
    
  }

if(ch == 'bls1_t'){
  
  ch_out <- dt%>%
    purrr::map_df(.f = function(x){
      xx <- tibble::tibble(dis_est = x$ch_bls1_t[2,1],
                           age_est = x$ch_bls1_t[3,1])
      
    }, .id = 'sim')
  
}
  if(ch == 'out'){
    
    ch_out <- dt%>%
      purrr::map_df(.f = function(x) x$ch_out, .id = 'sim')%>%
      dplyr::group_by(trt)%>%
      dplyr::summarise_at(.vars = c('e1_mean','s1_mean','e1_sd'), .funs = 'mean')%>%
      dplyr::mutate(e1_mean_ch = ifelse(trt=='c', e1_mean - e1_mean_c,
                                        e1_mean - e1_mean_t),
                    s1_mean_ch = ifelse(trt=='c', s1_mean - ae_c,
                                        s1_mean - ae_t),
                    e1_sd_ch = ifelse(trt=='c', e1_sd - e1_sd_c,
                                      e1_sd - e1_sd_t))
    
    
  }
  
  if(ch == 'w2'){
    
    ch_out <- dt%>%
      purrr::map_df(.f = function(x){
        xx <- tibble::tibble(age_est = x$w2_ch[2,1],
                             dis2v1_est = x$w2_ch[3,1],
                             dis3v1_est = x$w2_ch[4,1])
        
      }, .id = 'sim')
    
  }
  return(ch_out)
}


