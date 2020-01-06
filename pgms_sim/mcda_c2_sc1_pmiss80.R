
library(dplyr, warn.conflicts = F, quietly = T)

source('dt_sim.R')

#weights related functions
source('funs/weight_define_each.R')
source('funs/make_spec.R')
source('funs/fun_a.R')
source('funs/fun_b.R')
source('funs/assign_weights.R')
source('funs/assign_w100.R')
source('funs/pvf_apply.R')
source('funs/pvf.R')
source('funs/mi_weights.R')

v1_w1_mu  <- c(80, 50, 20) 
v1_w1_sd  <- rep(5, 3)
v1_w2_mu <- c(20, 60, 80)
v1_w2_sd <- rep(5, 3)

p_w100 <- c(0.5, 0.5)
p_miss <- 0.8

#scenario 1: only two weights- BCVA and non-ocular AEs
#both weights are defined only as a function of BCVA at BL
#mean and sds per weight


x1 <- parallel::mclapply(X = 1:1000,
                         mc.cores = 24,
                         FUN = function(i){
                           
#generate simulated data to be used with weights

set.seed(888*i)

dt_out <- dt_sim()


#weights specification
w1_spec <- weight_define_each(data = dt_out, name_weight = 'bcva_48w', br_spec = 'benefit', 'bcvac_bl', w_mu = v1_w1_mu, w_sd = v1_w1_sd)
w2_spec <- weight_define_each(data = dt_out, name_weight = 'ae_noc', br_spec = 'risk', 'bcvac_bl', w_mu = v1_w2_mu, w_sd = v1_w2_sd)

#cobmine weights into one data and specify probability to assign 100 to each weight
l <- make_spec(w1_spec, w2_spec, p100_weights = p_w100)

#assign initial weights based on the mean/sd specification provided by the user
dt_w <- assign_weights(data = dt_out, w_spec = l)

#assign 100 to one of the weights using probabilities specified in l
dt_w_final <- assign_w100(dt_w_init = dt_w, w_spec = l)

#standardize weights and apply utilization function that calculates mcda scores for each patient
dt_final <- pvf_apply(data = dt_w_final, w_spec = l)

#treatment arms comparison using all the weight, only XX% of the weights

dt_final[, 'miss'] <- stats::rbinom(n = nrow(dt_final), 1, prob = p_miss)

mcda_test_all <- stats::t.test(dt_final[dt_final[, 'trt'] == 'c','mcda'], dt_final[dt_final[, 'trt'] == 't','mcda'])

mcda_test_obs <- stats::t.test(dt_final[dt_final[, 'trt'] == 'c' & dt_final[, 'miss'] == 0, 'mcda'], 
                               dt_final[dt_final[, 'trt'] == 't' & dt_final[, 'miss'] == 0, 'mcda'])

mcda_test_mi <- mi_weights(data = dt_final, 
                           vars_bl = c('bcva_bl', 'age_bl', 'sex', 'cst_bl', 'srf', 'irf', 'rpe'),
                           w_spec = l, num_m = 10, mi_method = 'norm')

#summarise the br results
br_result <- tibble::tibble(res = ifelse(mcda_test_all$conf.int[2] < 0, 'benefit', 'no benefit'),
                            meth = 'all')
br_result[2, 'res']  <- ifelse(mcda_test_obs$conf.int[2] < 0, 'benefit', 'no benefit')
br_result[2, 'meth'] <- 'obs'
br_result[3, 'res']  <- ifelse(mcda_test_mi$qbar + qt(0.975, df = mcda_test_mi$v)*
                                 sqrt(mcda_test_mi$t) < 0, 'benefit', 'no benefit')
br_result[3, 'meth'] <- 'mi'

br_result[, 'sim_id'] <- i

out <- list(br_result)%>%purrr::set_names('br_result')

return(out)

})


saveRDS(x1, 'mcda_results/mcda_c2_sc1_pmiss80.rds')