check_sim <- function(dt, ch = c('bl_val','bl_cor', 'out','w2')){
  
  if(ch == 'bl_val'){
    
    ch_out <- dt%>%
      purrr::map_df(.f = function(x) x$check_val_bl, .id = 'sim')%>%
      dplyr::group_by(trt)%>%
      dplyr::summarise_at(dplyr::vars(bcva_m:sex_p), mean)%>%
      dplyr::mutate(ch_bcva_m_bl  = round(bcva_m - bcva_bl_m, 1),
                    ch_bcva_sd_bl = round(bcva_sd - bcva_bl_sd, 1),
                    ch_age_m_bl   = round(age_m - age_bl_m, 1),
                    ch_age_sd_bl  = round(age_sd - age_bl_sd, 1),
                    ch_cst_m_bl   = round(cst_m  - ifelse(trt == 't', cst_bl_m_t , cst_bl_m_c), 0),
                    ch_cst_sd_bl  = round(cst_sd  - ifelse(trt == 't', cst_bl_sd_t , cst_bl_sd_c), 0),
                    ch_srf        = round(srf_p - ifelse(trt == 't', srf_bl_t, srf_bl_c), 3),
                    ch_irf        = round(irf_p - irf_bl, 3),
                    ch_rpe        = round(rpe_p  - ifelse(trt == 't', rpe_bl_t , rpe_bl_c), 3),
                    ch_sex        = round(sex_p  - ifelse(trt == 't', male_t , male_c), 3))%>%
      dplyr::select(dplyr::starts_with('ch'))
    
  }
  
  if(ch == 'bl_cor'){
  
    ch_out <- dt%>%
      purrr::map_df(.f = function(x) x$check_cor_bl, .id = 'sim')%>%
      dplyr::group_by(i)%>%
      dplyr::summarise_at(dplyr::vars(bcva_bl:rpe), mean)
    
  }

  if(ch == 'out'){
    
    ch_out <- dt%>%
      purrr::map_df(.f = function(x) x$check_val_out, .id = 'sim')%>%
      dplyr::group_by(trt)%>%
      dplyr::summarise_at(dplyr::vars(bcva_48w:ll), mean)%>%
      dplyr::mutate(ch_bcva   = round(bcva_48w - ifelse(trt == 't', bcva_48w_m_t, bcva_48w_m_c), 1),
                    ch_cst    = round(cst_16w  - ifelse(trt == 't', cst_16w_m_t , cst_16w_m_c), 0),
                    ch_sirf   = round(sirf_16w - ifelse(trt == 't', sirf_16w_t, sirf_16w_c), 3),
                    ch_rpe    = round(rpe_16w  - ifelse(trt == 't', rpe_16w_t , rpe_16w_c), 3),
                    ch_da     = round(da_16w   - ifelse(trt == 't', da_16w_t , da_16w_c), 3),
                    ch_ae_noc = round(ae_noc   - ifelse(trt == 't', ae_noc_t , ae_noc_c),4),
                    ch_ae_oc  = round(ae_oc    - ifelse(trt == 't', ae_oc_t , ae_oc_c), 4),
                    ch_sae_noc = round(sae_noc - ifelse(trt == 't', sae_noc_t , sae_noc_c),4),
                    ch_sae_oc  = round(sae_oc  - ifelse(trt == 't', sae_oc_t , sae_oc_c), 4),
                    ch_ll      = round(ll      - ifelse(trt == 't', ll_t , ll_c), 4))%>%
      dplyr::select(trt, dplyr::starts_with('ch'))
    
    

  }
  
  if(ch == 'out_cor'){
    
    ch_out <- dt%>%
      purrr::map_df(.f = function(x) x$check_cor_val, .id = 'sim')%>%
      dplyr::group_by(var1, var2, trt)%>%
      dplyr::summarise_at(dplyr::vars(d), mean)
    
  }
  
  if(ch == 'w2'){
    
    ch_out <- dt%>%
      purrr::map_df(.f = function(x){
        xx <- tibble::tibble(age_est = x$w2_ch[2,1],
                             dis2v1_est = x$w2_ch[3,1],
                             dis3v1_est = x$w2_ch[4,1])
        
      }, .id = 'sim')
    
  }
  return(ch_out)
}


