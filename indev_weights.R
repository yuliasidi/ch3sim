#add weights


dt <- x1[[1]]$dt_out

# scenario 1: only two weights- BCVA and ocular AEs
# only one variable influences weights choice: BCVA at BL

#number of weights
n_weights <- 2
#weights names (need to match the patient df  names)
name_weights <- c('bcva_48w', 'ae_oc')
#probability to assign a weight of 100
p100_weights <- c(0.8, 0.2)
#first variable to be used in definition of weights 1-2 (CHANGE LATER PER WEIGHT)
v1_name <- 'bcvac_bl'
#mean and sds per weight per v1_name
v1_w1_mu  <- c(rev(seq(80, 95, 5)), rev(seq(70, 85, 5)), rev(seq(60, 75, 5))) 
v1_w1_sd  <- rep(5, 12)
v1_w2_mu <- c(20, 60, 80)
v1_w2_sd <- rep(5, 3)


source('funs/weight_define_each.R')
source('funs/make_spec.R')
source('funs/fun_a.R')
source('funs/fun_b.R')
source('funs/assign_weights.R')
source('funs/assign_w100.R')
source('funs/pvf.R')



w1_spec <- weight_define_each(data = dt_out, name_weight = 'bcva_48w', br_spec = 'benefit', 'bcvac_bl', 'agec_bl', w_mu = v1_w1_mu, w_sd = v1_w1_sd)
w2_spec <- weight_define_each(data = dt_out,name_weight = 'ae_oc', br_spec = 'risk', 'bcvac_bl', w_mu = v1_w2_mu, w_sd = v1_w2_sd)

l <- make_spec(w1_spec, w2_spec, p100_weights = c(0.8,0.2))

#assign initial weights based on the mean/sd specification provided by the user
dt_w <- assign_weights(data = dt_out, w_spec = l)
#assign 100 to one of the weights using probabilities specified in l
dt_final <- assign_w100(dt_w_init = dt_w, w_spec = l)

for (wnum in 1:length(unique(w_define$w))){
  
  dt_final[, paste0('u', wnum)] <- pvf(var = dt_final[, ww[ww$w==wnum,'wname'][[1,1]]],
                                       best = stats::quantile(dt_final[, ww[ww$w==wnum,'wname'][[1,1]]], 0.975),
                                       worst = stats::quantile(dt_final[, ww[ww$w==wnum,'wname'][[1,1]]], 0.025),
                                       type = ww[ww$w==wnum, 'br'][[1,1]])
  dt_final[, paste0('w', wnum, '_norm')] <- dt_final[, paste0('w', wnum)]/  
  
}



dt4 <- dt3%>%
  dplyr::mutate(u1 = pvf(var = e1, best = quantile(dt3$e1,.975), worst = quantile(dt3$e1,.025), type = 'benefit'),
                u2 = pvf(var = s1, best = 0, worst = 1, type = 'risk'),
                w1_n = w1/(w1+w2),
                w2_n = w2/(w1+w2),
                mcda = u1*w1_n + u2*w2_n,
                w_aval = rbinom(dplyr::n(), 1, 0.1))



#treatment arms comparison using all the weight, only XX% of the weights

#delete XX% of the weights and compare between treatment arms



# define probability to assign 100 weight to BCVA (main benefit)

#vector of probabilities for the most important important outcome
pw100 <- c(1, 0)

#assume that weights are functions baseline BCVA (how severe the disease is) only
#for example patients with lower BCVA at BL would be more likely to care more about BCVA outcome more
#than patients with higher BCVA
#for the second criteria, i.e., ocular AEs, patients with lower BCVA are more likely to care less about the AEs, 
#than patients with higher BCVA

crit1_bcva_low <- c(80, 10)
crit1_bcva_med <- c(60, 10)
crit1_bcva_high <- c(40, 10)

crit2_bcva_low <- c(30, 10)
crit2_bcva_med <- c(60, 10)
crit2_bcva_high <- c(90, 10)





#criteria 1 for weights: bcva at bl, it has 3 levels, define mean and sd for each level
unique(dt$bcvac_bl)

#assume that 
bcva_low_mean <- 


#names of the outcome variables that are being weighted
wnames <- c('bcvac_bl','oae')

#specify weight mean values for the benefit and risk criteria
w_mu <- c(100, 80)

#specify standard deviation values for the benefit and risk criterea
#we assume that there is no direct correlation between the weights, however
#due to their reliance on the same baseline values, these are actually correlated indirectly
w_sd <- c(0, 7)


#specify BL variable that 'participate' in weight determination- 10 maximum
w_varbl <- c('bcva_bl', 'cst_bl', 'age', 'sex')

#specify a vector of point estimates for each of the above BL variables
b1 <- c(0, 0.5)
b2 <- c(0, 0.1)
b3 <- c(0, -0.5)
b4 <- c(0, 0)

b <- rbind(b1,b2,b3)  

#severety of the condition defined based on the appendix table 6 of the published article
# dt <- dt_out%>%
#   dplyr::mutate(bcva_bl_cat = dplyr::case_when(bcva_bl < 55  ~ 1, #severe
#                                                bcva_bl >= 71 ~ 3, #mild
#                                                TRUE ~ 2))%>% #moderate
#   dplyr::mutate(cst_bl_cat = dplyr::case_when(cst_bl >= 400 ~ 1, #severe
#                                               TRUE ~ 2)) #mild
#use multinomial distirbution to assign 100 first

dt <- dt_out
tmp <- t(rmultinom(n = nrow(dt), size = 1, prob = pw100))
colnames(tmp) <- paste0('w100_', wnames)

dt1 <- dt%>%
  dplyr::bind_cols(tmp%>%
                     tibble::as_tibble())



for (j in seq(1, length(pw100), 1)){
  for (f in seq(1, length(w_varbl) - 1, 1)){
    
    dt1[ ,paste0('muw_', wnames[j])] <- w_mu[j] - 
      b[f, j]*(dt1[, w_varbl[f]] - mean(dt1[[w_varbl[f]]]))

    dt1[ ,paste0('muw_', wnames[j])] <- dt1[ ,paste0('muw_', wnames[j])] + 
      b[f + 1, j]*(dt1[, w_varbl[f + 1]] - mean(dt1[[w_varbl[f + 1]]]))


  }
  
  }
    dt1[ ,paste0('muw_', wnames[j])] <- int + 
      b1[i]*dt1[, w_varbl[1]] + 
      b2[i]*dt1[, w_varbl[2]] +
      b3[i]*dt1[, w_varbl[3]] + 
      b4[i]*dt1[, w_varbl[4]] + 
      b5[i]*dt1[, w_varbl[5]] + 
      b6[i]*dt1[, w_varbl[6]] +
      b7[i]*dt1[, w_varbl[7]] +  
      b8[i]*dt1[, w_varbl[8]] +  
      b9[i]*dt1[, w_varbl[9]] +  
      b10[i]*dt1[, w_varbl[10]] +  
      
      
  }
  
}


pb <- 1
ps1 <- 1 - pb

num_w <- 2

dt_out1 <- dt_out%>%
  dplyr::mutate(wb1 =stats::rbinom())



test <-
