
library(dplyr, warn.conflicts = F, quietly = T)

#scenario 1: only two weights- BCVA and non-ocular AEs
#both weights are defined only as a function of BCVA at BL
#mean and sds per weight

                         
#generate simulated data to be used with weights
i<-8
set.seed(888*i)

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
p_miss <- 0.7

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


dt_final%>%group_by(trt)%>%summarise(mean(miss))


data <- dt_final
w_spec <- l
  for (wnum in seq_along(w_spec)){
    data[, sprintf('w_%02d', wnum)] <- ifelse(data[, 'miss'] == 1, 
                                              NA, 
                                              data[, sprintf('w_%02d', wnum)])  
    
    data[, sprintf('u_%02d', wnum)] <- NULL
    data[, sprintf('w_%02d_norm', wnum)] <- NULL
    data[, sprintf('uw_%02d_norm', wnum)] <- NULL
  }
  
  data[, 'mcda'] <- NULL
  vars_bl <- c('bcva_bl','age_bl')
  
  predM <- mice::make.predictorMatrix(data)
  predM[, names(data)[!names(data) %in% c(grep('^w_', names(data), value = T), vars_bl)]] <- 0
  
  num_m <- 10
  post <- mice::make.post(data) 
  for (wnum in seq_along(w_spec)){
    post[sprintf('w_%02d', wnum)] <- 
      "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(0, 100))"
  }
    
  mice_out <- mice::mice(data = data, 
                         m = num_m,
                         predictorMatrix = predM,
                         method = 'norm',
                         maxit = 30,
                         printFlag = FALSE,
                         post = post)
  plot(mice_out)
  
  per_m_sum <- tibble::tibble(m = seq(1, num_m, 1))
  
  for( i in 1:num_m){
    dt_tmp <- mice::complete(mice_out, i)
    dt_tmp <- pvf_apply(data = dt_tmp, w_spec =  w_spec)
    
    per_m_sum[i, 'qhat'] <- mean(dt_tmp$mcda[dt_tmp$trt=='c']) - mean(dt_tmp$mcda[dt_tmp$trt=='t'])
    per_m_sum[i, 'u'] <- var(dt_tmp$mcda[dt_tmp$trt=='c'])/nrow(subset(dt_tmp, trt=='c')) + 
      var(dt_tmp$mcda[dt_tmp$trt=='t'])/nrow(subset(dt_tmp, trt=='t'))
    
    per_m_sum[i, 'w_01_qhat'] <- mean(dt_tmp$w_01)
    per_m_sum[i, 'w_02_qhat'] <- mean(dt_tmp$w_02)
    
    
    
    
  }
  
  dt_final%>%summarise(w1_m = mean(w_01), w2_m = mean(w_02))
  dt_final%>%filter(miss==0)%>%summarise(w1_m = mean(w_01), w2_m = mean(w_02))
  per_m_sum%>%summarise_at(c('w_01_qhat', 'w_02_qhat'), 'mean')
  
  mi_res <- tibble::tibble(qbar = mean(per_m_sum$qhat),
                           ubar = mean(per_m_sum$u),
                           b = var(per_m_sum$qhat),
                           t = ubar + (1 + 1/nrow(per_m_sum))*b,
                           v = (nrow(per_m_sum) - 1)*(t/((1 + 1/nrow(per_m_sum))*b))^2)
  
  mi_res$qbar + qt(0.975, df = mi_res$v)*
    sqrt(mi_res$t)







mcda_test_mi <- mi_weights(data = dt_final, vars_bl = c('bcva_bl', 'age_bl'), w_spec = l, num_m = 10)

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

