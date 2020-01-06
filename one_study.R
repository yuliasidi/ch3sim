library(dplyr, warn.conflicts = F, quietly = T)
library(ggplot2)

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

i<-55

set.seed(888*i)

#simulate study data
dt_out <- dt_sim()


####################################################
#three criteria: PE, ocular AEs, non-ocular AEs ####
####################################################

#look at the criteria

dt_out%>%
  ggplot(aes(x = trt, y = bcva_48w)) +
  geom_boxplot()
# -> very similar outcome measure, as expected


dt_out%>%
  dplyr::group_by(trt)%>%
  dplyr::summarise_at(c('ae_oc', 'ae_noc'), 'mean')%>%
  tidyr::gather(key = 'var', value = 'val', -c('trt'))%>%
  ggplot(aes(fill = trt, y = val, x = var)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(labels=scales::percent)
# -> Ocular AEs are very similar, while non-ocular AEs appeared more frequently in the control

#################
#define weights #
#################

#first assume that patients in general care more about PE than AEs, thus there is higher probability to 
#assign 100 to PE than to AEs
p_w100 <- c(0.8, 0.1, 0.1)

#given that a patient didn't assign 100 to PE, her weight could be affected by the BCVA at BL
#patients who have lower BCVA at BL would have higher weights on average that patients who have higher 
#BCVA values at BL 
v1_w1_mu  <- c(80, 60, 40) 
v1_w1_sd  <- rep(5, 3)

#assume that AEs weights are affected by sex, and that women would have lower weights than men 
v1_w2_mu <- c(30, 60)
v1_w2_sd <- rep(5, 2)
v1_w3_mu <- c(30, 60)
v1_w3_sd <- rep(5, 2)

p_miss <- 0.9


#weights specification
w1_spec <- weight_define_each(data = dt_out, name_weight = 'bcva_48w', br_spec = 'benefit', 'bcvac_bl', w_mu = v1_w1_mu, w_sd = v1_w1_sd)
w2_spec <- weight_define_each(data = dt_out, name_weight = 'ae_oc', br_spec = 'risk', 'sex', w_mu = v1_w2_mu, w_sd = v1_w2_sd)
w3_spec <- weight_define_each(data = dt_out, name_weight = 'ae_noc', br_spec = 'risk', 'sex', w_mu = v1_w3_mu, w_sd = v1_w3_sd)

#cobmine weights into one data and specify probability to assign 100 to each weight
l <- make_spec(w1_spec, w2_spec, w3_spec, p100_weights = p_w100)

#assign initial weights based on the mean/sd specification provided by the user
dt_w <- assign_weights(data = dt_out, w_spec = l)

#assign 100 to one of the weights using probabilities specified in l
dt_w_final <- assign_w100(dt_w_init = dt_w, w_spec = l)


#look at the weights distirbution
dt_w_final%>%
  ggplot(aes(x = w_01, fill = bcvac_bl)) +
  geom_histogram(alpha = 0.5, bins = 30)
           
dt_w_final%>%
  ggplot(aes(x = w_02, fill = as.factor(sex))) +
  geom_histogram(alpha = 0.5, bins = 30)

dt_w_final%>%
  ggplot(aes(x = w_03, fill = as.factor(sex))) +
  geom_histogram(alpha = 0.5, bins = 30)


dt_final <- pvf_apply(data = dt_w_final, w_spec = l)

dt_final%>%
  ggplot(aes(x = trt, y = mcda)) +
  geom_boxplot()

t.test(dt_final$mcda[dt_final$trt=='c'], dt_final$mcda[dt_final$trt=='t'])
