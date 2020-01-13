
library(dplyr, warn.conflicts = F, quietly = T)

source('dt_sim.R')

#weights related functions
source('funs/weight_define_each.R')
source('funs/fun_a.R')
source('funs/fun_b.R')
source('funs/assign_weights.R')
source('funs/pvf_apply.R')
source('funs/pvf.R')
source('funs/mi_weights.R')

#assume that PE weights are affected only by BCVA at BL
#patients who have lower BCVA at BL would have higher weights on average that patients who have higher 
#BCVA values at BL 
v1_w1_mu  <- c(70, 50, 30) 
v1_w1_sd  <- rep(5, 3)

#assume that AEs weights are affected by sex, and that women would have lower weights than men 
v1_w2_mu <- c(50, 80)
v1_w2_sd <- rep(5, 2)
v1_w3_mu <- c(70, 90)
v1_w3_sd <- rep(5, 2)

p_miss <- 0.9

#scenario: three weights- BCVA, and AEs
#BCVA is defined as a function of BCVA at BL
#AEs are defined as a function of sex
#Scenario 2: patients care more about non-ocular AEs than other AEs  or PE



x1 <- parallel::mclapply(X = 1:1000,
                         mc.cores = 20,
                         FUN = function(i){
                           
#generate simulated data to be used with weights

set.seed(888*i)

dt_out <- dt_sim()

#weights specification
w1_spec <- weight_define_each(data = dt_out, name_weight = 'bcva_48w', br_spec = 'benefit', 'bcvac_bl', w_mu = v1_w1_mu, w_sd = v1_w1_sd)
w2_spec <- weight_define_each(data = dt_out, name_weight = 'ae_oc', br_spec = 'risk', 'sex', w_mu = v1_w2_mu, w_sd = v1_w2_sd)
w3_spec <- weight_define_each(data = dt_out, name_weight = 'ae_noc', br_spec = 'risk', 'sex', w_mu = v1_w3_mu, w_sd = v1_w3_sd)

#cobmine weights into one list
l <- list(w1_spec, w2_spec, w3_spec)

#assign weights based on the mean/sd specification provided by the user
#for each patient, the highest weight will be assigned 100 
dt_w <- assign_weights(data = dt_out, w_spec = l)


#standardize weights and apply utilization function that calculates mcda scores for each patient
dt_final <- pvf_apply(data = dt_w, w_spec = l)

#treatment arms comparison using all the weight, only XX% of the weights

dt_final[, 'miss'] <- stats::rbinom(n = nrow(dt_final), 1, prob = p_miss)

mcda_test_all <- stats::t.test(dt_final$mcda[dt_final$trt=='c'], dt_final$mcda[dt_final$trt=='t'])

mcda_test_obs <- stats::t.test(dt_final$mcda[dt_final$trt=='c' & dt_final$miss == 0],
                               dt_final$mcda[dt_final$trt=='t' & dt_final$miss == 0])

mcda_test_mi <- mi_weights(data = dt_final, 
                           vars_bl = c('bcva_bl', 'age_bl', 'sex', 'cst_bl', 'srf', 'irf', 'rpe'),
                           w_spec = l, num_m = 10, mi_method = 'norm')
###########################
#summarise the br results #
###########################

br_comp <- tibble::tibble(meth = 'all',
                          mean_diff = mcda_test_all$estimate[1] - mcda_test_all$estimate[2],
                          se_diff = mean_diff/mcda_test_all$statistic)

br_result <- tibble::tibble(res = ifelse(mcda_test_all$conf.int[2] < 0, 'benefit', 'no benefit'),
                            meth = 'all')
br_result[, 'sim_id'] <- i

#comparison with Wen et al 2014
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
out <- list(br_comp, br_result, wen_uci)%>%purrr::set_names('br_comp', 'br_result', 'wen_uci')

return(out)

})


saveRDS(x1, 'mcda_results/mcda_c3_sc2_all_wencomp.rds')