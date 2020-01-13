#the below is taken from Wen et. al 2014, the authors suggested bootstrap from p.9
out <- numeric(10000)

for (j in 1:10000){
  
  dt_boot <- dplyr::sample_n(dt_out, size = nrow(dt_out), replace = TRUE)

  tt1 <-dt_w%>%dplyr::group_by(bcvac_bl)%>%dplyr::summarise(perc = n()/nrow(dt_w))
  mean_w_01 <- v1_w1_mu%*%tt1[,2][[1]]
  
  tt2 <- dt_w%>%dplyr::group_by(sex)%>%dplyr::summarise(perc = n()/nrow(dt_w))
  mean_w_02 <- v1_w2_mu%*%tt2[,2][[1]]
  mean_w_03 <- v1_w3_mu%*%tt2[,2][[1]]
  
  mcda_w1 <- mean_w_01[[1]]/(mean_w_01[[1]] + mean_w_02[[1]] + mean_w_03[[1]])
  mcda_w2 <- mean_w_02[[1]]/(mean_w_01[[1]] + mean_w_02[[1]] + mean_w_03[[1]])
  mcda_w3 <- mean_w_03[[1]]/(mean_w_01[[1]] + mean_w_02[[1]] + mean_w_03[[1]])
  
  
  dt_boot_sum <-
    dt_boot%>%
    dplyr::group_by(trt)%>%
    dplyr::summarise_at(c('bcva_48w', 'ae_oc', 'ae_noc'), 'mean')%>%
    dplyr::mutate(f_v1 = (stats::quantile(dt_boot$bcva_48w, 0.975) - bcva_48w)/
                    (stats::quantile(dt_boot$bcva_48w, 0.975) - stats::quantile(dt_boot$bcva_48w, 0.025)),
                  f_v2 = 1 - ae_oc,
                  f_v3 = 1 - ae_noc,
                  mcda = f_v1*mcda_w1 + f_v2*mcda_w2 + f_v3*mcda_w3)
  
  out[j] <- dt_boot_sum$mcda[dt_boot_sum$trt=='c'] - dt_boot_sum$mcda[dt_boot_sum$trt=='t']
}

wen_uci <- stats::quantile(out, 0.975)    



