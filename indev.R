library(MASS)
library(dplyr)
library(purrr)
library(bin2mi)
library(mice)

source("funs/dt_bl.R")
source("funs/dt_outs.R")
source("funs/check_sim.R")
source("funs/pvf.R")
source("funs/bra_comp.R")

#from HAWK and HARRIER sttudies
m2 <- -4
n_arm <- 300 #originally calculate ss was 297, using rounded value
#sd_ass <- 15 #sd assumed for ss calculations

#baseline covariates
dis_bl_mean <- 61
dis_bl_sd <- 14
age_bl_mean <- 77
age_bl_sd <- 9
cst_bl_mean_t <- 463
cst_bl_sd_t <- 167
cst_bl_mean_c <- 458
cst_bl_sd_c <- 146

#assume correlation between BCVA and age (makes sense that we aging there is lower score of BCVA)
age_dis_r <- -0.4


#primary efficacy
e1_mean_t <- 6.6 
e1_sd_t <- 0.71
e1_mean_c <- 6.8 
e1_sd_c <- 0.71

#rates of reported ocular AEs(originally reported ae_t = 0.5, ae_c = 0.47)
ae_c <- 0.5
ae_t <- 0.35

#effect of b1, b2 at baseline on AE occurance in trt = 'c
b1_s1_c <- -0.1
b2_s1_c <-  0.05
#effect of b1, b2 at baseline on AE occurance in trt = 't
b1_s1_t <- -0.02
b2_s1_t <-  0.02


x1 <- parallel::mclapply(X = 1:1000,
                         mc.cores = 7,
                         FUN = function(x){
  
 set.seed(888*x)
 ############################
 ## Somulate baseline data ##
 ############################
  dt1 <- 
    dplyr::bind_rows(
      dt_bl(n_arm = n_arm, b1_mean = dis_bl_mean, b2_mean = age_bl_mean, 
            b1_sd = dis_bl_sd, b2_sd = age_bl_sd, rho_b1b2 = age_dis_r)%>%
        dplyr::mutate(trt = 'c'),
      dt_bl(n_arm = n_arm, b1_mean = dis_bl_mean, b2_mean = age_bl_mean, 
            b1_sd = dis_bl_sd, b2_sd = age_bl_sd, rho_b1b2 = age_dis_r)%>%
        dplyr::mutate(trt = 't',
                      pat_id = pat_id + n_arm))
  
  ##########################################################
  ## Simulate outcome data as a function of baseline data ##
  ##########################################################

  #parameter set-up for generating outcome variables: E1 and S1.  
  rho_dis_bl_ch <- -0.3
  b1_e1_t <- rho_dis_bl_ch*e1_sd_t/sd(dt1$dis_st[dt1$trt=='t']) #b1 = r*S_y/S_x
  b1_e1_c <- rho_dis_bl_ch*e1_sd_c/sd(dt1$dis_st[dt1$trt=='c']) #b1 = r*S_y/S_x
  
  dt2 <-
    dt1%>%
    tidyr::nest(-trt)%>%
    dplyr::mutate(data1 = 
                    ifelse(trt == 'c',
                           purrr::map(data, dt_outs, e1_mean = e1_mean_c, e1_sd = e1_sd_c, b1_e1 = b1_e1_c, 
                                      s1_mean = ae_c, b1_s1 = b1_s1_c, b2_s1 = b2_s1_c),
                           purrr::map(data, dt_outs, e1_mean = e1_mean_t, e1_sd = e1_sd_t, b1_e1 = b1_e1_t,
                                      s1_mean = ae_t, b1_s1 = b1_s1_t, b2_s1 = b2_s1_t)))%>%
    dplyr::select(-data)%>%
    tidyr::unnest()

  #some checks
  dt2_c <- dt2%>%dplyr::filter(trt == 'c')
  dt2_t <- dt2%>%dplyr::filter(trt == 't')
  
  ch_ble1 <- summary(lm(e1 ~ dis_st + age, dt2))$coefficients
  ch_bls1_c <- summary(glm(s1 ~ dis_st + age, dt2_c, family = 'binomial'))$coefficients
  ch_bls1_t <- summary(glm(s1 ~ dis_st + age, dt2_t, family = 'binomial'))$coefficients
  ch_out <- dt2%>%dplyr::group_by(trt)%>%summarise_at(.vars = c('e1', 's1'), .funs = c('mean', 'sd'))
  
  ####################################################
  ## Simulate weight as a function of baseline data ##
  ####################################################
  
  #assume that more severe patients 'care' less about the AEs
  mu_w2_st1 <- 90
  mu_w2_st2 <- 80
  mu_w2_st3 <- 70
  
  #assume that older patients also 'care'  less about AEs
  age_w2 <- -1
  
  #derive model parameters using the above settings
  int <- mu_w2_st3 - age_w2*mean(dt1$age) 
  b_mods <- mu_w2_st2 - int - age_w2*mean(dt1$age)
  b_milds <- mu_w2_st1 - int - age_w2*mean(dt1$age)
  
  
  #based on the above define w2 for each subject
  dt3 <- dt2%>%
    dplyr::mutate(mu_w2 = int + 
                    ifelse(dis_cat==1, b_milds, ifelse(dis_cat==2, b_mods, 0)) +
                    age_w2*age,
                  w2 = floor(rnorm(dplyr::n(), mean = mu_w2, sd = 1)),
                  w2 = ifelse(w2>100, 100, w2),
                  w1 = 100)%>% #in case the second weight is more then 100, make it 100
  dplyr::select(-c(mu_w2, mu_e1, mu_s1))
  #check the w2:
  w2_ch <- summary(lm(w2 ~ age + as.factor(dis_cat), dt3))$coefficients
  
  ##########################################
  #BRA for all/subset of the patients     ##
  ##########################################
  
  # for best and worst use 2.5 and 97.5 quantiles similarly to Li et al.
  dt4 <- dt3%>%
    dplyr::mutate(u1 = pvf(var = e1, best = quantile(dt3$e1,.975), worst = quantile(dt3$e1,.025), type = 'benefit'),
                  u2 = pvf(var = s1, best = 0, worst = 1, type = 'risk'),
                  w1_n = w1/(w1+w2),
                  w2_n = w2/(w1+w2),
                  mcda = u1*w1_n + u2*w2_n,
                  w_aval = rbinom(dplyr::n(), 1, 0.1))
  
  
  res_all <- t.test(dt4$mcda[dt4$trt == 'c'], dt4$mcda[dt4$trt == 't'])
  res_waval <- t.test(dt4$mcda[dt4$trt == 'c' & dt4$w_aval==1], dt4$mcda[dt4$trt == 't'  & dt4$w_aval==1])
  
  
  dt5 <- dt4%>%
    dplyr::mutate(mcda = ifelse(w_aval == 1, mcda, NA),
                  w2 = ifelse(w_aval == 1, w2, NA))%>%
    dplyr::select(pat_id, age, dis_st, w1, w2)
  
  
  #mice imputation
  predm <- mice::make.predictorMatrix(dt5)
  predm[, "pat_id"] <- 0
  predm[, "w1"] <- 0 
  

  imp <- mice::mice(data = dt5,
                    m = 5,
                    predictorMatrix = predm,
                    method = "cart",
                    seed = 666*x,
                    print = FALSE)
  
  mlist <- list(seq(1, 5, 1))
  
  dt_norm <- purrr::pmap_df(mlist, .f = function(x) {
    
    out <- mice::complete(imp, x)
    out <- out%>%
      dplyr::mutate(m_num = x)
  })

  dt_mi <- dt_norm%>%
    dplyr::left_join(dt4%>%
                       dplyr::select(pat_id, trt, u1, u2),
                     by = "pat_id")%>%
    dplyr::mutate(w1_n = w1/(w1+w2),
                  w2_n = w2/(w1+w2),
                  mcda = u1*w1_n + u2*w2_n)
  
  
  mi_sum1 <- dt_mi%>%
    dplyr::group_by(trt, m_num)%>%
    dplyr::summarise(qhat = mean(mcda), u = var(mcda), nobs = n())%>%
    dplyr::ungroup()%>%
    tidyr::gather('stat','val',-c(trt,m_num))%>%
    tidyr::unite(stat,c(stat,trt))%>%
    tidyr::spread(stat,val)%>%
    dplyr::mutate(qhat = qhat_c - qhat_t,
                  u = u_c/nobs_c + u_t/nobs_t)
  
  mi_res <-  mi_sum1%>%
    dplyr::summarise(qbar = mean(qhat), ubar = mean(u), b = var(qhat))%>%
    dplyr::mutate(t = ubar + (1 + 1/5)*b,
                  v = (5-1)*(t/((1 + 1/5)*b))^2,
                  lb = qbar - qt(1 - 0.025, v)*sqrt(t),
                  ub = qbar + qt(1 - 0.025, v)*sqrt(t))
  
    
  ###add more checks:
  dis_dis <- 
    dt4%>%
    dplyr::group_by(dis_cat)%>%
    dplyr::summarise(per = n()/length(dt4$dis_cat))

    out <- list(ch_ble1, ch_bls1_c, ch_bls1_t, ch_out, w2_ch, res_all, dis_dis, res_waval, mi_res)%>%
    purrr::set_names(c("ch_ble1","ch_bls1_c","ch_bls1_t","ch_out", 'w2_ch', 
                       'res_all', 'dis_dis', 'res_waval', "mi_res"))
  
  })
 

check_sim(x1, ch ='ble1')%>%dplyr::summarise_at(.vars = c('dis_est','age_est'), .funs = 'mean')
check_sim(x1, ch ='bls1_c')%>%dplyr::summarise_at(.vars = c('dis_est','age_est'), .funs = 'mean')
check_sim(x1, ch ='bls1_t')%>%dplyr::summarise_at(.vars = c('dis_est','age_est'), .funs = 'mean')
check_sim(x1, ch ='out')
check_sim(x1, ch ='w2')%>%dplyr::summarise_at(.vars = c('age_est','dis2v1_est','dis3v1_est'), .funs = 'mean')

bra_comp(x1)%>%dplyr::group_by(infavor)%>%dplyr::summarise(n())
bra_comp(x1, all_w = FALSE)%>%dplyr::group_by(infavor)%>%dplyr::summarise(n())
bra_comp(x1, mi = TRUE)%>%dplyr::group_by(infavor)%>%dplyr::summarise(n())


x1%>%purrr::map_df(.f = function(x) {
  xx <- x$dis_dis})%>%
  dplyr::group_by(dis_cat)%>%
  dplyr::summarise(mean(per))

  
#normality check across all simulaitons
x1%>%purrr::map_df(.f = function(x) {
  xx <- x$sh_test
   out <- tibble::tibble(pvalue_shw = xx,
                         n_ass = ifelse(xx<0.05, 0 , 1))
   return(out)
  xx}, .id = 'sim')%>%
  dplyr::summarise(mean(n_ass))

x1%>%purrr::map_df(.f = function(x) {
  xx <- x$sh_testw2
  out <- tibble::tibble(pvalue_shw = xx,
                        n_ass = ifelse(xx<0.05, 0 , 1))
  return(out)
  xx}, .id = 'sim')%>%
  dplyr::summarise(mean(n_ass))

                  
library(ggplot2)
dt4%>%
  ggplot(aes(x = w2, fill = trt)) +
  geom_histogram(bins = 10, alpha = 0.5)

dt4%>%
  ggplot(aes(x = mcda, fill = trt)) +
  geom_histogram(bins = 15, alpha = 0.5)


dt4%>%
  ggplot(aes(x = mcda, fill = trt)) +
  geom_density(alpha = 0.5)

sh_test <- shapiro.test(residuals(lm(mcda ~ trt, dt4)))$p.value
