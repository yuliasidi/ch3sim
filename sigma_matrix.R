sigma_matrix <- function(x){
  
list2env(x,envir = environment())  

#define sigma matrix for MASS::mvrnorm for trt = t
s_t <- sqrt(srf_bl_t*(1 - srf_bl_t)/n_arm) 
i_t <- sqrt(irf_bl*(1 - irf_bl)/n_arm)
r_t <- sqrt(rpe_bl_t*(1 - rpe_bl_t)/n_arm) 
sex_t <- sqrt(male_t*(1 - male_t)/n_arm)

smat_t <- matrix(c(bcva_bl_sd^2, rho_bcva_age*bcva_bl_sd*age_bl_sd, rho_bcva_cst*bcva_bl_sd*cst_bl_sd_t,
                   rho_bcva_srf*bcva_bl_sd*s_t, rho_bcva_irf*bcva_bl_sd*i_t, rho_bcva_rpe*bcva_bl_sd*r_t, 0,
                   rho_bcva_age*bcva_bl_sd*age_bl_sd, age_bl_sd^2, rho_age_cst*age_bl_sd*cst_bl_sd_t,
                   rho_age_srf*age_bl_sd*s_t, rho_age_irf*age_bl_sd*i_t, rho_age_rpe*age_bl_sd*r_t, 0,
                   rho_bcva_cst*bcva_bl_sd*cst_bl_sd_t, rho_age_cst*age_bl_sd*cst_bl_sd_t, cst_bl_sd_t^2,
                   rho_cst_srf*cst_bl_sd_t*s_t, rho_cst_irf*cst_bl_sd_t*i_t, rho_cst_rpe*cst_bl_sd_t*r_t, 0,
                   rho_bcva_srf*bcva_bl_sd*s_t, rho_age_srf*age_bl_sd*s_t, rho_cst_srf*cst_bl_sd_t*s_t,
                   s_t^2, rho_srf_irf*s_t*i_t, rho_srf_rpe*s_t*r_t, 0,
                   rho_bcva_irf*bcva_bl_sd*i_t, rho_age_irf*age_bl_sd*i_t, rho_cst_irf*cst_bl_sd_t*i_t,
                   rho_srf_irf*s_t*i_t, i_t^2, rho_irf_rpe*i_t*r_t, 0,
                   rho_bcva_rpe*bcva_bl_sd*r_t, rho_age_rpe*age_bl_sd*r_t, rho_cst_rpe*cst_bl_sd_t*r_t,
                   rho_srf_rpe*s_t*r_t, rho_irf_rpe*i_t*r_t, r_t^2, 0,
                   0, 0, 0, 0, 0, 0, sex_t^2), 7, 7, byrow = T)

#define sigma matrix for MASS::mvrnorm for trt = t
s_c <- sqrt(srf_bl_c*(1 - srf_bl_c)/n_arm) 
i_c <- sqrt(irf_bl*(1 - irf_bl)/n_arm)
r_c <- sqrt(rpe_bl_c*(1 - rpe_bl_c)/n_arm) 
sex_c <- sqrt(male_c*(1 - male_c)/n_arm)


smat_c <- matrix(c(bcva_bl_sd^2, rho_bcva_age*bcva_bl_sd*age_bl_sd, rho_bcva_cst*bcva_bl_sd*cst_bl_sd_c,
                   rho_bcva_srf*bcva_bl_sd*s_c, rho_bcva_irf*bcva_bl_sd*i_c, rho_bcva_rpe*bcva_bl_sd*r_c, 0,
                   rho_bcva_age*bcva_bl_sd*age_bl_sd, age_bl_sd^2, rho_age_cst*age_bl_sd*cst_bl_sd_c,
                   rho_age_srf*age_bl_sd*s_c, rho_age_irf*age_bl_sd*i_c, rho_age_rpe*age_bl_sd*r_c, 0,
                   rho_bcva_cst*bcva_bl_sd*cst_bl_sd_c, rho_age_cst*age_bl_sd*cst_bl_sd_c, cst_bl_sd_c^2,
                   rho_cst_srf*cst_bl_sd_c*s_c, rho_cst_irf*cst_bl_sd_c*i_c, rho_cst_rpe*cst_bl_sd_c*r_c, 0,
                   rho_bcva_srf*bcva_bl_sd*s_c, rho_age_srf*age_bl_sd*s_c, rho_cst_srf*cst_bl_sd_c*s_c,
                   s_c^2, rho_srf_irf*s_c*i_c, rho_srf_rpe*s_c*r_c, 0,
                   rho_bcva_irf*bcva_bl_sd*i_c, rho_age_irf*age_bl_sd*i_c, rho_cst_irf*cst_bl_sd_c*i_c,
                   rho_srf_irf*s_c*i_c, i_c^2, rho_irf_rpe*i_c*r_c, 0,
                   rho_bcva_rpe*bcva_bl_sd*r_c, rho_age_rpe*age_bl_sd*r_c, rho_cst_rpe*cst_bl_sd_c*r_c,
                   rho_srf_rpe*s_c*r_c, rho_irf_rpe*i_c*r_c, r_c^2, 0,
                   0, 0, 0, 0, 0, 0, sex_c^2), 7, 7, byrow = T)

setNames(lapply(ls(),function(x,e) get(x = x,envir = e),e = environment()),ls())
}
