source("dt_vals.R")
source("bl_corr_vals.R")
source("sigma_matrix.R")
source("funs/dt_bl.R")
source("funs/dt_outs.R")
source("funs/check_bl_fun_out.R")
library(magrittr)

make_list <- function(){
  ls_x <- ls(envir = parent.frame())
  setNames(lapply(ls_x,function(x,e) get(x = x,envir = e),e = parent.frame()),ls_x) 
}

dt_sim <- function(){

  #the reported ss was 297 without taking into account drop-out rate
  n_arm <- 300
  
  #BL and outcome values based on HAWK data
  list2env(dt_vals(),envir = environment())
  
  #correlations between BL variables, self-defined
  list2env(bi_corr_vals(),envir = environment())

  #sigma matrix using the above values and correlaitons
  list2env(sigma_matrix(make_list()),envir = environment())
  
  #define mu_t/mu_c for MASS::mvrnorm used in dt_bl function
  mu_t <- matrix(c(bcva_bl_m, age_bl_m, cst_bl_m_t, srf_bl_t, irf_bl, rpe_bl_t, male_t))
  mu_c <- matrix(c(bcva_bl_m, age_bl_m, cst_bl_m_c, srf_bl_c, irf_bl, rpe_bl_c, male_c))
  
  
  ####################                                                    
  # simulate BL data #
  ####################                                                                               
  dt_t <- dt_bl(n_arm = n_arm, mu_bl = mu_t, smat_bl = smat_t, trt = 't')
  dt_c <- dt_bl(n_arm = n_arm, mu_bl = mu_c, smat_bl = smat_c, trt = 'c')
  dt_bl <- dplyr::bind_rows(dt_t, dt_c)
  
  #########################
  # simulate outcome data #
  #########################
  
  #bcva outcome
  # for lower bl score - easier to get higher number of letters -> b2 should
  # be nagative
  dt_out1 <- dt_bl%>%
    tidyr::nest(-trt)%>%
    dplyr::mutate(data1 = ifelse(trt == 't', 
                                 purrr::map(data, dt_outs, 
                                            b1 = -0.3, 
                                            b2 = -0.2, 
                                            b2cov = 'bcva_bl',
                                            out_m = bcva_48w_m_t, 
                                            out_sd = bcva_48w_sd, 
                                            type = 'cont'),
                                 purrr::map(data, dt_outs, 
                                            b1 = -0.3, 
                                            b2= -0.2, 
                                            b2cov = 'bcva_bl',
                                            out_m = bcva_48w_m_c, 
                                            out_sd = bcva_48w_sd, 
                                            type = 'cont')))%>%
    dplyr::select(-data)%>%
    tidyr::unnest()%>%
    dplyr::rename(bcva_48w = out)
  
  #cst outcome
  # for higher cst score- it would be easier to drop down -> b2 should b2
  # should be negative
  dt_out2 <- dt_out1%>%
    tidyr::nest(-trt)%>%
    dplyr::mutate(data1 = ifelse(trt == 't', 
                                 purrr::map(data, dt_outs, 
                                            b1 = 2, 
                                            b2 = -0.1, 
                                            b2cov = 'cst_bl',
                                            out_m = cst_16w_m_t, 
                                            out_sd = cst_16w_sd, 
                                            type = 'cont'),
                                 purrr::map(data, dt_outs, 
                                            b1 = 2, 
                                            b2= -0.1, 
                                            b2cov = 'cst_bl',
                                            out_m = cst_16w_m_c, 
                                            out_sd = cst_16w_sd, 
                                            type = 'cont')))%>%
    dplyr::select(-data)%>%
    tidyr::unnest()%>%
    dplyr::rename(cst_16w = out)
  
  #srf/irf outcome
  # for higher levels of SRF/IRF it would be easier to improve b2/b3 should 
  # be negative
  dt_out3 <- dt_out2%>%
    tidyr::nest(-trt)%>%
    dplyr::mutate(data1 = ifelse(trt == 't', 
                                 purrr::map(data, dt_outs, 
                                            b1 = 0.01, 
                                            b2 = -0.15,
                                            b3 = -0.15,
                                            b2cov = 'srf',
                                            b3cov = 'irf',
                                            out_m = sirf_16w_t, 
                                            type = 'prop'),
                                 purrr::map(data, dt_outs, 
                                            b1 = 0.01, 
                                            b2 = -0.1,
                                            b3 = -0.1,
                                            b2cov = 'srf',
                                            b3cov = 'irf',
                                            out_m = sirf_16w_c, 
                                            type = 'prop')))%>%
    dplyr::select(-data)%>%
    tidyr::unnest()%>%
    dplyr::rename(sirf_16w = out)
  
  #rpe outcome
  # for higher levels of RPE it would be easier to improve b2 should 
  # be negative
  dt_out4 <- dt_out3%>%
    tidyr::nest(-trt)%>%
    dplyr::mutate(data1 = ifelse(trt == 't', 
                                 purrr::map(data, dt_outs, 
                                            b1 = 0.03, 
                                            b2 = -0.1,
                                            b2cov = 'rpe',
                                            out_m = rpe_16w_t, 
                                            type = 'prop'),
                                 purrr::map(data, dt_outs, 
                                            b1 = 0.04, 
                                            b2 = -0.1,
                                            b2cov = 'rpe',
                                            out_m = rpe_16w_c, 
                                            type = 'prop')))%>%
    dplyr::select(-data)%>%
    tidyr::unnest()%>%
    dplyr::rename(rpe_16w = out)
  
  #disease activity outcome- I decided to define it as a function of BCVA and age at BL 
  # patients with higher BCVA should have less DA -> b2 is negative
  dt_out5 <- dt_out4%>%
    tidyr::nest(-trt)%>%
    dplyr::mutate(data1 = ifelse(trt == 't', 
                                 purrr::map(data, dt_outs, 
                                            b1 = 0.02, 
                                            b2 = -0.01,
                                            b2cov = 'bcva_bl',
                                            out_m = da_16w_t, 
                                            type = 'prop'),
                                 purrr::map(data, dt_outs, 
                                            b1 = 0.02, 
                                            b2 = -0.01,
                                            b2cov = 'bcva_bl',
                                            out_m = da_16w_c, 
                                            type = 'prop')))%>%
    dplyr::select(-data)%>%
    tidyr::unnest()%>%
    dplyr::rename(da_16w = out)
  
  #non-ocular AEs outcome: function of age at BL only
  dt_out6 <- dt_out5%>%
    tidyr::nest(-trt)%>%
    dplyr::mutate(data1 = ifelse(trt == 't', 
                                 purrr::map(data, dt_outs, 
                                            b1 = 0.03, 
                                            b2 = 0,
                                            b2cov = 'bcva_bl',
                                            out_m = ae_noc_t, 
                                            type = 'prop'),
                                 purrr::map(data, dt_outs, 
                                            b1 = 0.03, 
                                            b2 = 0,
                                            b2cov = 'bcva_bl',
                                            out_m = ae_noc_c, 
                                            type = 'prop')))%>%
    dplyr::select(-data)%>%
    tidyr::unnest()%>%
    dplyr::rename(ae_noc = out)
  
  #ocular AEs outcome: function of age and bcva at BL
  dt_out7 <- dt_out6%>%
    tidyr::nest(-trt)%>%
    dplyr::mutate(data1 = ifelse(trt == 't', 
                                 purrr::map(data, dt_outs, 
                                            b1 = 0.02, 
                                            b2 = -0.05,
                                            b2cov = 'bcva_bl',
                                            out_m = ae_oc_t, 
                                            type = 'prop'),
                                 purrr::map(data, dt_outs, 
                                            b1 = 0.02, 
                                            b2 = -0.05,
                                            b2cov = 'bcva_bl',
                                            out_m = ae_oc_c, 
                                            type = 'prop')))%>%
    dplyr::select(-data)%>%
    tidyr::unnest()%>%
    dplyr::rename(ae_oc = out)
  
  #non-ocular SAEs outcome: function of age at BL only
  dt_out8 <- dt_out7%>%
    tidyr::nest(-trt)%>%
    dplyr::mutate(data1 = ifelse(trt == 't', 
                                 purrr::map(data, dt_outs, 
                                            b1 = 0.02, 
                                            b2 = 0,
                                            b2cov = 'bcva_bl',
                                            out_m = sae_noc_t, 
                                            type = 'prop'),
                                 purrr::map(data, dt_outs, 
                                            b1 = 0.02, 
                                            b2 = 0,
                                            b2cov = 'bcva_bl',
                                            out_m = sae_noc_c, 
                                            type = 'prop')))%>%
    dplyr::select(-data)%>%
    tidyr::unnest()%>%
    dplyr::rename(sae_noc = out)
  
  #ocular SAEs outcome: function of age and bcva at BL
  dt_out9 <- dt_out8%>%
    tidyr::nest(-trt)%>%
    dplyr::mutate(data1 = ifelse(trt == 't', 
                                 purrr::map(data, dt_outs, 
                                            b1 = 0.002, 
                                            b2 = -0.01,
                                            b2cov = 'bcva_bl',
                                            out_m = sae_oc_t, 
                                            type = 'prop'),
                                 purrr::map(data, dt_outs, 
                                            b1 = 0.002, 
                                            b2 = -0.01,
                                            b2cov = 'bcva_bl',
                                            out_m = sae_oc_c, 
                                            type = 'prop')))%>%
    dplyr::select(-data)%>%
    tidyr::unnest()%>%
    dplyr::rename(sae_oc = out)
  
  #>=15 letter loss outcome: function of age and bcva at BL
  dt_out10 <- dt_out9%>%
    tidyr::nest(-trt)%>%
    dplyr::mutate(data1 = ifelse(trt == 't', 
                                 purrr::map(data, dt_outs, 
                                            b1 = 0.002, 
                                            b2 = -0.01,
                                            b2cov = 'bcva_bl',
                                            out_m = ll_t, 
                                            type = 'prop'),
                                 purrr::map(data, dt_outs, 
                                            b1 = 0.002, 
                                            b2 = -0.01,
                                            b2cov = 'bcva_bl',
                                            out_m = ll_c, 
                                            type = 'prop')))%>%
    dplyr::select(-data)%>%
    tidyr::unnest()%>%
    dplyr::rename(ll = out)
  
  #categorise bcva_bl, age_bl and cst_bl using HAWK definitions presented in the baseline characteristics
  dt_out <- dt_out10%>%
    dplyr::mutate(
      bcvac_bl = dplyr::case_when(bcva_bl <= 55 ~ 'low',
                           bcva_bl >= 71 ~ 'high',
                           TRUE ~ 'med'),
      bcvac_bl = factor(bcvac_bl,levels = c('low','med','high')),
      agec_bl = cut(age_bl,c(0,65,75,85,95),include.lowest = TRUE,right = FALSE),
      cstc_bl = dplyr::case_when(cst_bl < 400 ~'low', TRUE ~ 'high'),
      cstc_bl = factor(cstc_bl,levels = c('low','high'))
    )

 return(dt_out) 
}
