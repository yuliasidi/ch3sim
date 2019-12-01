dt_outs <- function(dt, b1 = 0, b2 = 0, b3 = 0, b1cov = 'age_bl', b2cov, b3cov = '',
                    out_m, out_sd = 0, type = c('prop', 'cont')){
  
 if (type=='cont'){
   int <- out_m - b1*mean(dt[[b1cov]]) - b2*mean(dt[[b2cov]])
   
   dt1 <- dt%>%
     dplyr::mutate(out_mu = int + b1*!!rlang::sym(b1cov) + b2*!!rlang::sym(b2cov),
                   out = stats::rnorm(n = dplyr::n(), mean = out_mu, sd = out_sd))%>%
     dplyr::select(-out_mu)
   
   
 }

  if (type=='prop'){
    if (b3==0){
      int <- log(out_m/(1-out_m)) - b1*mean(dt[[b1cov]]) - b2*mean(dt[[b2cov]])
      
      dt1 <- dt%>%
        dplyr::mutate(out_mu = 1/(1 + exp( - int - b1*!!rlang::sym(b1cov) - b2*!!rlang::sym(b2cov))),
                      out = stats::rbinom(dplyr::n(), 1, out_mu))%>%
        dplyr::select(-out_mu)
    }
    else{
      int <- log(out_m/(1-out_m)) - b1*mean(dt[[b1cov]]) - b2*mean(dt[[b2cov]]) -
        b3*mean(dt[[b3cov]])
      
      dt1 <- dt%>%
        dplyr::mutate(out_mu = 1/ (1 + exp(- int - b1*!!rlang::sym(b1cov) - b2*!!rlang::sym(b2cov) - 
                      b3*!!rlang::sym(b3cov))),
                      out = stats::rbinom(dplyr::n(), 1, out_mu))%>%
        dplyr::select(-out_mu)
    }
    
  }
  
 return(dt1) 
  }
