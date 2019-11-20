dt_outs <- function(dt, e1_mean, e1_sd, b1_e1, s1_mean, b1_s1, b2_s1 ){
  
  #define intercepts to be used in the outcome models below
  int_e1 <- e1_mean - b1_e1*mean(dt$dis_st)
  int_s1 <- log(s1_mean/(1 - s1_mean)) - b1_s1*mean(dt$dis_st) - 
    b2_s1*mean(dt$age)
  
  dt1 <- dt%>%
    dplyr::mutate(mu_e1 =  int_e1 + b1_e1*dis_st,
                  e1 = rnorm(n = dplyr::n(), mean = mu_e1, sd = e1_sd),
                  mu_s1 = 1/(1+exp(- int_s1 - b1_s1*dis_st - b2_s1*age)),
                  s1 = rbinom(dplyr::n(), 1, mu_s1))#%>%
    #dplyr::select(-c(mu_e1, mu_s1))
  
  
  }
