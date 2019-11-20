bra_comp <- function(dt, all_w = TRUE){
 
  if(all_w){
    out <- dt%>%
      purrr::map_df(.f = function(x){
        tt <- x$res
        tt1 <- tibble::tibble(l_ci = tt$conf.int[1],
                              u_ci = tt$conf.int[2])
        
        tt2 <- tt1%>%
          dplyr::mutate(infavor = ifelse(u_ci < 0, 't', 'c'))
      }, .id = 'sim')
  }
  else{
    out <- dt%>%
      purrr::map_df(.f = function(x){
        tt <- x$res_waval
        tt1 <- tibble::tibble(l_ci = tt$conf.int[1],
                              u_ci = tt$conf.int[2])
        
        tt2 <- tt1%>%
          dplyr::mutate(infavor = ifelse(u_ci < 0, 't', 'c'))
      }, .id = 'sim')
  }
  
  return(out)
  
}


