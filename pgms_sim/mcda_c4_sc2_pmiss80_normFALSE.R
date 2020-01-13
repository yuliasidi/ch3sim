
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

#scenario: four weights- BCVA, CST and AEs
#BCVA is defined as a function of BCVA at BL
#AEs are defined as a function of sex
#CST is defined as a function of CST at BL

#Scenario 2: patients care more about PE and ocular AEs than non-ocular AEs or CST

#################
#define weights #
#################

#assume that PE weights are affected only by BCVA at BL
#patients who have lower BCVA at BL would have higher weights on average that patients who have higher 
#BCVA values at BL 
v1_w1_mu  <- c(90, 60, 30) 
v1_w1_sd  <- rep(7, 3)

#assume that AEs weights are affected by sex, and that women would have lower weights than men 
v1_w2_mu <- c(70, 80)
v1_w2_sd <- rep(7, 2)
v1_w3_mu <- c(30, 40)
v1_w3_sd <- rep(7, 2)

#assume that CST weights are affected by CST at BL, patients with higher CST at BL, will give higher
#weights for the CST outcome
v1_w4_mu <- c(15, 30)
v1_w4_sd <- rep(7, 2)


p_miss <- 0.8


x1 <- parallel::mclapply(X = 1:1000,
                         mc.cores = 24,
                         FUN = function(i){
                           
#generate simulated data to be used with weights

set.seed(888*i)

dt_out <- dt_sim()

#weights specification
w1_spec <- weight_define_each(data = dt_out, name_weight = 'bcva_48w', br_spec = 'benefit', 'bcvac_bl', w_mu = v1_w1_mu, w_sd = v1_w1_sd)
w2_spec <- weight_define_each(data = dt_out, name_weight = 'ae_oc', br_spec = 'risk', 'sex', w_mu = v1_w2_mu, w_sd = v1_w2_sd)
w3_spec <- weight_define_each(data = dt_out, name_weight = 'ae_noc', br_spec = 'risk', 'sex', w_mu = v1_w3_mu, w_sd = v1_w3_sd)
w4_spec <- weight_define_each(data = dt_out, name_weight = 'cst_16w', br_spec = 'risk', 'cstc_bl', w_mu = v1_w4_mu, w_sd = v1_w4_sd)


#cobmine weights into one list
l <- list(w1_spec, w2_spec, w3_spec, w4_spec)

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
                           w_spec = l, num_m = 10, mi_method = 'norm', 
                           trunc_range = FALSE)
###########################
#summarise the br results #
###########################

br_comp <- tibble::tibble(meth = 'all',
                          mean_diff = mcda_test_all$estimate[1] - mcda_test_all$estimate[2],
                          se_diff = mean_diff/mcda_test_all$statistic)

br_comp[2, 'meth'] <- 'obs'
br_comp[2, 'mean_diff'] <- mcda_test_obs$estimate[1] - mcda_test_obs$estimate[2]
br_comp[2, 'se_diff'] <- (mcda_test_obs$estimate[1] - mcda_test_obs$estimate[2])/
  mcda_test_obs$statistic

br_comp[3, 'meth'] <- 'mi'
br_comp[3, 'mean_diff'] <- mcda_test_mi$qbar
br_comp[3, 'se_diff'] <- sqrt(mcda_test_mi$t)
br_comp[3, 'ubar'] <- mcda_test_mi$ubar
br_comp[3, 'b'] <- mcda_test_mi$b

br_result <- tibble::tibble(res = ifelse(mcda_test_all$conf.int[2] < 0, 'benefit', 'no benefit'),
                            meth = 'all')
br_result[2, 'res']  <- ifelse(mcda_test_obs$conf.int[2] < 0, 'benefit', 'no benefit')
br_result[2, 'meth'] <- 'obs'
br_result[3, 'res']  <- ifelse(mcda_test_mi$qbar + qt(0.975, df = mcda_test_mi$v)*
                                 sqrt(mcda_test_mi$t) < 0, 'benefit', 'no benefit')
br_result[3, 'meth'] <- 'mi'

br_result[, 'sim_id'] <- i

out <- list(br_comp, br_result)%>%purrr::set_names('br_comp', 'br_result')

return(out)

})


saveRDS(x1, sprintf('mcda_results/mcda_c4_sc2_pmiss%d_%s%s.rds', 100*0.8, 'norm', FALSE))
