#The below values are taken from HAWK study, where t = Brolucizumab 0.6mg
#and c = Aflibercept 2mg
#we use PE and key secondary endpoints that do not involve PE (BVCA)
#and key safety which are treatment emergent changes in ocular/non-ocular params
#acc. to the paper PE and key SE analysis were adjsuted to BCVA and and age at BL

#################################
# BL values based on appendix 6 #
#################################

#BCVA
bcva_bl_m <- 61
bcva_bl_sd <- 14
#age
age_bl_m <- 77
age_bl_sd <- 9

#CST defined per arm due to the numeric differences
cst_bl_m_t <- 463
cst_bl_sd_t <- 167
cst_bl_m_c <- 458
cst_bl_sd_c <- 146

#SRF fluid proportion
srf_bl_t <- 0.69
srf_bl_c <- 0.68

#IRF fluid proportion
irf_bl <- 0.54

#Sub-RPE fluid proportion
rpe_bl_t <- 0.47
rpe_bl_c <- 0.44

#sex male proportion
male_t <- 0.43
male_c <- 0.46

##################
# Outcome values #
##################

#PE: BCVA change fromn BL to 48W, higher BCVA = better => 
#higher change => better
bcva_48w_m_t <- 6.6
bcva_48w_m_c <- 6.8
bcva_48w_sd <- 0.71

#key SE: CST change from BL to 16W, lower CST = better =>
#lower change => better
cst_16w_m_t <- -161
cst_16w_m_c <- -134
cst_16w_sd <- 6.2

#key SE: SRF/IRF presence at 16W, lower = better
sirf_16w_t <-0.34
sirf_16w_c <-0.52

#key SE: sub-RPE presence at 16W, lower = better
rpe_16w_t <- 0.19 
rpe_16w_c <- 0.27

#key SE: disease-activity presence at 16W, lower = better
da_16w_t <- 0.24
da_16w_c <- 0.35

#Safety: Ocular AEs
ae_oc_t <- 0.5
ae_oc_c <- 0.47

#Safety: Nonocular AEs
ae_noc_t <- 0.64
ae_noc_c <- 0.72

#Safety: Ocular SAEs
sae_oc_t <- 0.031
sae_oc_c <- 0.008

#Safety: Nonocular SAEs
sae_noc_t <- 0.13
sae_noc_c <- 0.19

#Safety: >=15 letter loss, proportion
ll_t <- 0.064
ll_c <- 0.055

  







