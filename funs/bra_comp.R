bra_comp <- function(dt, all_w = TRUE, mi = FALSE){
 
  if(all_w & !mi){
    out <- dt%>%
      purrr::map_df(.f = function(x){
        tt <- x$res_all
        tt1 <- tibble::tibble(l_ci = tt$conf.int[1],
                              u_ci = tt$conf.int[2])
        
        tt2 <- tt1%>%
          dplyr::mutate(infavor = ifelse(u_ci < 0, 't', 'c'))
      }, .id = 'sim')
  }
  if(!all_w & !mi){
    out <- dt%>%
      purrr::map_df(.f = function(x){
        tt <- x$res_waval
        tt1 <- tibble::tibble(l_ci = tt$conf.int[1],
                              u_ci = tt$conf.int[2])
        
        tt2 <- tt1%>%
          dplyr::mutate(infavor = ifelse(u_ci < 0, 't', 'c'))
      }, .id = 'sim')
  }
  
  if(mi){
    out <- dt%>%
      purrr::map_df(.f = function(x){
        tt <- x$mi_res
       
        tt1 <- tt%>%
          dplyr::mutate(infavor = ifelse(ub < 0, 't', 'c'))
      }, .id = 'sim')
  }
  return(out)
  
}


