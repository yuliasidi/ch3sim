dt_bl <- function(n_arm, b1_mean, b2_mean, b1_sd, b2_sd, rho_b1b2){
  
  dt <- 
    MASS::mvrnorm(n = n_arm, 
                  mu = c(b1_mean, b2_mean), 
                  Sigma = matrix(c(dis_bl_sd^2, rho_b1b2*b1_sd*b2_sd, 
                                   rho_b1b2*b1_sd*b2_sd, b2_sd^2), 2, 2))
  colnames(dt) <- c("dis_st", "age")

  dt1 <- dt%>%
    tibble::as_tibble()%>%
    dplyr::mutate(age = ifelse(age>95, 95, age),
                  pat_id = seq(1, length(dt[,1]), 1),
                  dis_cat = dplyr::case_when(dis_st <= 55 ~ 3,
                                            dis_st > 55 & dis_st <= 70 ~ 2,
                                            TRUE ~ 1))
return(dt1)

  }
                