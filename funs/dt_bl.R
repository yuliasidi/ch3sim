dt_bl <- function(n_arm, mu_bl, smat_bl, trt){

  dt <- MASS::mvrnorm(n = n_arm, mu = mu_bl, Sigma = smat_bl)
  colnames(dt) <- c( "bcva_bl", "age_bl", "cst_bl", "srf_raw", "irf_raw", "rpe_raw", "sex_raw")
  
  dt1 <- dt%>%
    tibble::as_tibble()%>%
    dplyr::mutate(sex_raw = ifelse(sex_raw < 0, 0, ifelse(sex_raw > 1, 1, sex_raw)),
                  srf_raw = ifelse(srf_raw < 0, 0, ifelse(srf_raw > 1, 1, srf_raw)),
                  irf_raw = ifelse(irf_raw < 0, 0, ifelse(irf_raw > 1, 1, irf_raw)),
                  rpe_raw = ifelse(rpe_raw < 0, 0, ifelse(rpe_raw > 1, 1, rpe_raw)))%>%
    dplyr::mutate(sex = stats::rbinom(dplyr::n(), 1, sex_raw),
                  srf = stats::rbinom(dplyr::n(), 1, srf_raw),
                  irf = stats::rbinom(dplyr::n(), 1, irf_raw),
                  rpe = stats::rbinom(dplyr::n(), 1, rpe_raw),
                  trt = trt,
                  bcva_bl = ifelse(bcva_bl > 0, bcva_bl, 0), # bcva cannot be negative
                  age_bl  = ifelse(age_bl < 50, 50, ifelse(age_bl > 95, 95, age_bl)), # only patients 50 or above were recruted,
                  #also cut-off patients older than 95 to make the data more realistic
                  cst_bl = ifelse(cst_bl > 0, cst_bl, 0))%>%
    dplyr::select(-c(srf_raw, irf_raw, rpe_raw, sex_raw))

return(dt1)

  }
                