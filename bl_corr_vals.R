
#correlations between BL values

#BCVA decreases with age
rho_bcva_age <- -0.4

#BCVA has negative corr with other 'fluid' variables (set the same value for all)
rho_bcva_cst <- -0.3
rho_bcva_srf <- -0.3
rho_bcva_irf <- -0.3
rho_bcva_rpe <- -0.3

#age has positive corr with other 'fluid' variables (set the same value for all)
rho_age_cst <- 0.4
rho_age_srf <- 0.4
rho_age_irf <- 0.4
rho_age_rpe <- 0.4

#fluid variables have positive pairwise corr among themselve (set the same value for all)
rho_cst_srf <- 0.6
rho_cst_irf <- 0.6
rho_cst_rpe <- 0.6
rho_srf_irf <- 0.6
rho_srf_rpe <- 0.6
rho_irf_rpe <- 0.6