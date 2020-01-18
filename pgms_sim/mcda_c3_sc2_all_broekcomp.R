
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

#for comparison with Broenk. assume that patients from a preference study are 
#healthier than patients in the study

#0*70 + 0.1*50 + 0.9*30 

v1_w1_mu  <- c(32, 32, 32) 
v1_w1_sd  <- rep(7, 3)

#assume that AEs weights are affected by sex, and that women would have lower weights than men 
v1_w2_mu <- c(66.5, 66.5)
v1_w2_sd <- rep(7, 2)
v1_w3_mu <- c(81, 81)
v1_w3_sd <- rep(7, 2)

#scenario: three weights- BCVA, and AEs
#BCVA is defined as a function of BCVA at BL
#AEs are defined as a function of sex
#Scenario 2: patients care more about non-ocular AEs than other AEs  or PE



x1 <- parallel::mclapply(X = 1:1000,
                         mc.cores = 7,
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

mcda_test_all <- stats::t.test(dt_final$mcda[dt_final$trt=='c'], dt_final$mcda[dt_final$trt=='t'])

###########################
#summarise the br results #
###########################

br_result <- tibble::tibble(res = ifelse(mcda_test_all$conf.int[2] < 0, 'benefit', 'no benefit'),
                            meth = 'all')
br_result[, 'sim_id'] <- i


out <- list(br_result)%>%purrr::set_names('br_result')

return(out)

})


x1%>%
  purrr::map_df(.f = function(x) x$br_result, .id = 'sim')%>%
  dplyr::group_by(meth, res)%>%
  dplyr::summarise(pp = n()/length(x1))%>%
  dplyr::filter(res=='benefit')

saveRDS(x1, 'mcda_results/mcda_c3_sc2_all_broekcomp.rds')