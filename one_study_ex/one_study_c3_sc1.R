library(dplyr, warn.conflicts = F, quietly = T)
library(ggplot2)
library(patchwork)

source('dt_sim.R')

#weights related functions
source('funs/weight_define_each.R')
source('funs/fun_a.R')
source('funs/fun_b.R')
source('funs/assign_weights.R')
source('funs/pvf_apply.R')
source('funs/pvf.R')
source('funs/mi_weights.R')

i<-55

set.seed(888*i)

#simulate study data
dt_out <- dt_sim()


####################################################
#three criteria: PE, ocular AEs, non-ocular AEs ####
####################################################
#Scenario 1: patients care more about PE than AEs, since the 'benefit' here comes only from 
#occurance of non-ocular AEs, it is expected that using such criteria the new treatment is not
#beneficial overall of the standard of care



#################
#define weights #
#################

#assume that PE weights are affected only by BCVA at BL
#patients who have lower BCVA at BL would have higher weights on average that patients who have higher 
#BCVA values at BL 
v1_w1_mu  <- c(90, 60, 30) 
v1_w1_sd  <- rep(7, 3)

0.33*90 + 0.43*60 + 0.24*30
#assume that AEs weights are affected by sex, and that women would have lower weights than men 
v1_w2_mu <- c(50, 75)
v1_w2_sd <- rep(7, 2)
0.45*50 + 0.55*70
v1_w3_mu <- c(50, 75)
v1_w3_sd <- rep(7, 2)

#weights specification
w1_spec <- weight_define_each(data = dt_out, name_weight = 'bcva_48w', br_spec = 'benefit', 'bcvac_bl', w_mu = v1_w1_mu, w_sd = v1_w1_sd)
w2_spec <- weight_define_each(data = dt_out, name_weight = 'ae_oc', br_spec = 'risk', 'sex', w_mu = v1_w2_mu, w_sd = v1_w2_sd)
w3_spec <- weight_define_each(data = dt_out, name_weight = 'ae_noc', br_spec = 'risk', 'sex', w_mu = v1_w3_mu, w_sd = v1_w3_sd)

#cobmine weights into one list
l <- list(w1_spec, w2_spec, w3_spec)

#assign weights based on the mean/sd specification provided by the user
#for each patient, the highest weight will be assigned 100 
dt_w <- assign_weights(data = dt_out, w_spec = l)


###############################################################################
#graph of the criteria and weights
p1_up <- dt_out%>%
  ggplot(aes(x = trt, y = bcva_48w)) +
  geom_boxplot() + 
  coord_flip() + 
  labs(y = NULL,x= 'Treatment') + 
  theme_minimal() +
  theme(axis.title.y = element_text(size=rel(0.75))) +
  scale_x_discrete(labels = c('C','T')) 


p2_up <- dt_out%>%
  ggplot(aes(x = trt, y = ae_oc)) +
  stat_summary(fun.y = mean,geom='bar',width = 0.5) +
  coord_flip() + 
  labs(y = NULL,x=NULL) + 
  theme_minimal() +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
  theme(axis.title.y = element_blank(),axis.text.y = element_blank())

p3_up <- dt_out%>%
  ggplot(aes(x = trt, y = ae_noc)) +
  stat_summary(fun.y = mean,geom='bar',width = 0.5) +
  coord_flip() + 
  labs(y = NULL,x=NULL) + 
  theme_minimal() +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
  theme(axis.title.y = element_blank(),axis.text.y = element_blank())


p1 <- dt_w%>%
  ggplot(aes(x = w_01, fill = bcvac_bl)) +
  geom_histogram(alpha = 0.5, bins = 30) +
  theme(legend.position = 'bottom',
        legend.text = element_text(size=rel(0.75))) +
  labs(fill = 'BCVA', x='Weights BCVA',y='Count')
           
p2 <- dt_w%>%
  ggplot(aes(x = w_02, fill = as.factor(sex))) +
  geom_histogram(alpha = 0.5, bins = 30) +
  theme(legend.position = 'bottom',
        legend.text = element_text(size=rel(0.75))) +
  labs(y=NULL,fill = NULL, x = 'Weights ocular AEs') +
  scale_fill_discrete(labels = c('Female', 'Male'))

p3 <- dt_w%>%
  ggplot(aes(x = w_03, fill = as.factor(sex))) +
  geom_histogram(alpha = 0.5, bins = 30) +
  theme(legend.position = 'bottom',
        legend.text = element_text(size=rel(0.75))) +
  labs(y=NULL,fill = NULL, x = 'Weights non-ocular AEs') +
  scale_fill_discrete(labels = c('Female', 'Male'))

find_limit <- function(p,axis='y') purrr::map_dbl(p,.f=function(x) max(ggplot2::ggplot_build(x)$data[[1]][[axis]]))

reset_limits <- function(p,max_y=NULL,max_x=NULL){
  
  if(is.null(max_y))
    max_y <- ceiling(max(find_limit(p,'y')))
  
  if(is.null(max_x))
    max_x <- ceiling(max(find_limit(p,'x'))) + 10
  
  p <- purrr::map(p,function(x,new_val) x + scale_x_continuous(expand = expand_scale(add = 10),limits=c(0,new_val),breaks = seq(0,100,20)),new_val=max_x)
  p <- purrr::map(p,function(x,new_val) x + scale_y_continuous(expand = expand_scale(add = 10),limits=c(0,new_val)),new_val=max_y)
  p
}

p_down <- list(p1,p2,p3)%>%reset_limits()
p_up <- list(p1_up,p2_up,p3_up)

plot_criteria_weights <- purrr::map2(p_up,p_down,.f=function(p1,p2) (p1/p2) + plot_layout(heights = c(1,4)))%>%purrr::reduce(`|`)

pdf('one_study_ex/outs/plot_criteria_weights_c3_sc1.pdf')
plot_criteria_weights
dev.off()


###############################################################################

dt_final <- pvf_apply(data = dt_w, w_spec = l)

plot_mcda <- dt_final%>%
  ggplot(aes(x = trt, y = mcda)) +
  geom_boxplot()

plot_mcda

pdf('one_study_ex/outs/plot_mcda_c3_sc1.pdf')
plot_mcda
dev.off()

mcda_test_all <- stats::t.test(dt_final$mcda[dt_final$trt=='c'], dt_final$mcda[dt_final$trt=='t'])

#comparison with approach from Wen et al. 2014
source('wen2014_comp.R')

################################
#delete p_miss % of observations 
################################
p_miss <- 0.9
dt_final[, 'miss'] <- stats::rbinom(n = nrow(dt_final), 1, prob = p_miss)
mcda_test_obs <- stats::t.test(dt_final$mcda[dt_final$trt=='c' & dt_final$miss == 0],
                               dt_final$mcda[dt_final$trt=='t' & dt_final$miss == 0])

mcda_test_mi <- mi_weights(data = dt_final, 
                           vars_bl = c('bcva_bl', 'age_bl', 'sex', 'cst_bl', 'srf', 'irf', 'rpe'),
                           w_spec = l, num_m = 10, mi_method = 'cart')
br_comp <- tibble::tibble(meth = 'all',
                          mean_diff = mcda_test_all$estimate[1] - mcda_test_all$estimate[2],
                          se_diff = mean_diff/mcda_test_all$statistic)

br_comp[2, 'meth'] <- 'obs'
br_comp[2, 'mean_diff'] <- mcda_test_obs$estimate[1] - mcda_test_obs$estimate[2]
br_comp[2, 'se_diff'] <- (mcda_test_obs$estimate[1] - mcda_test_obs$estimate[2])/
  mcda_test_obs$statistic

br_comp[3, 'meth'] <- 'mi'
br_comp[3, 'mean_diff'] <- mcda_test_mi$qbar
br_comp[3, 'se_diff'] <- sqrt(mcda_test_mi$t)
br_comp[3, 'ubar'] <- mcda_test_mi$ubar
br_comp[3, 'b'] <- mcda_test_mi$b


w_spec <- l
data <- dt_final

for (wnum in seq_along(w_spec)){
  data[, sprintf('w_%02d', wnum)] <- ifelse(data[, 'miss'][[1]] == 1, 
                                            NA, 
                                            data[, sprintf('w_%02d', wnum)][[1]])  
  
  data[, sprintf('u_%02d', wnum)] <- NULL
  data[, sprintf('w_%02d_norm', wnum)] <- NULL
  data[, sprintf('uw_%02d_norm', wnum)] <- NULL
}

mean(data$miss)

data[, 'mcda'] <- NULL

predM <- mice::make.predictorMatrix(data)
vars_bl <- c('bcva_bl', 'age_bl', 'sex', 'cst_bl', 'srf', 'irf', 'rpe')
predM[, names(data)[!names(data) %in% c(grep('^w_', names(data), value = T), vars_bl)]] <- 0

#some attributes of the MI
trunc_range <- FALSE
num_m <- 10
n_iter <- 40 
mi_method <-'norm'

if (trunc_range){
  post <- mice::make.post(data) 
  for (wnum in seq_along(w_spec)){
    post[sprintf('w_%02d', wnum)] <- 
      "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(0, 100))"
  }
  
  mice_out <- mice::mice(data =  data, 
                         m = num_m,
                         predictorMatrix = predM,
                         method = mi_method,
                         maxit = n_iter,
                         printFlag = FALSE,
                         post = post)
}else{
  mice_out <- mice::mice(data =  data, 
                         m = num_m,
                         predictorMatrix = predM,
                         method = mi_method,
                         maxit = n_iter,
                         printFlag = FALSE,
                         minbucket = 10)
  
}

plot(mice_out)


