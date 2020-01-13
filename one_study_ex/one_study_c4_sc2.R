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


#########################################################
#four criteria: PE, CST, ocular AEs, non-ocular AEs ####
#########################################################
#Scenario 1: patients care more about PE and ocular AEs than non-ocular AEs or CST

#################
#define weights #
#################

#assume that PE weights are affected only by BCVA at BL
#patients who have lower BCVA at BL would have higher weights on average that patients who have higher 
#BCVA values at BL 
v1_w1_mu  <- c(90, 60, 30) 
v1_w1_sd  <- rep(7, 3)

0.33*v1_w1_mu[1] + 0.4*v1_w1_mu[2] + 0.24*v1_w1_mu[3] 

#assume that AEs weights are affected by sex, and that women would have lower weights than men 
v1_w2_mu <- c(70, 80)
v1_w2_sd <- rep(7, 2)
0.45*v1_w2_mu[1] + 0.55*v1_w2_mu[2]

v1_w3_mu <- c(30, 40)
v1_w3_sd <- rep(7, 2)
0.45*v1_w3_mu[1] + 0.55*v1_w3_mu[2]

#assume that CST weights are affected by CST at BL, patients with higher CST at BL, will give higher
#weights for the CST outcome
v1_w4_mu <- c(15, 30)
v1_w4_sd <- rep(7, 2)
0.35*v1_w4_mu[1] + 0.65*v1_w4_mu[2]



#weights specification
w1_spec <- weight_define_each(data = dt_out, name_weight = 'bcva_48w', br_spec = 'benefit', 'bcvac_bl', w_mu = v1_w1_mu, w_sd = v1_w1_sd)
w2_spec <- weight_define_each(data = dt_out, name_weight = 'ae_oc', br_spec = 'risk', 'sex', w_mu = v1_w2_mu, w_sd = v1_w2_sd)
w3_spec <- weight_define_each(data = dt_out, name_weight = 'ae_noc', br_spec = 'risk', 'sex', w_mu = v1_w3_mu, w_sd = v1_w3_sd)
w4_spec <- weight_define_each(data = dt_out, name_weight = 'cst_16w', br_spec = 'risk', 'cstc_bl', w_mu = v1_w4_mu, w_sd = v1_w4_sd)

#cobmine weights into one list
l <- list(w1_spec, w2_spec, w3_spec, w4_spec)

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

p4_up <- dt_out%>%
  ggplot(aes(x = trt, y = cst_16w)) +
  geom_boxplot() + 
  coord_flip() + 
  labs(y = NULL,x= 'Treatment') + 
  theme_minimal() +
  theme(axis.title.y = element_text(size=rel(0.75))) +
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

p4 <- dt_w%>%
  ggplot(aes(x = w_04, fill = cstc_bl)) +
  geom_histogram(alpha = 0.5, bins = 30) +
  theme(legend.position = 'bottom',
        legend.text = element_text(size=rel(0.75))) +
  labs(y=NULL,fill = NULL, x = 'Weights CST') +
  scale_fill_discrete(labels = c('Low','High'))

find_limit <- function(p,axis='y') purrr::map_dbl(p,.f=function(x) max(ggplot2::ggplot_build(x)$data[[1]][[axis]]))

reset_limits <- function(p,max_y=NULL,max_x=NULL){
  
  if(is.null(max_y))
    max_y <- ceiling(max(find_limit(p,'y')))
  
  if(is.null(max_x))
    max_x <- ceiling(max(find_limit(p,'x')))
  
  p <- purrr::map(p,function(x,new_val) x + scale_x_continuous(limits=c(0,new_val),breaks = seq(0,100,20)),new_val=max_x)
  p <- purrr::map(p,function(x,new_val) x + scale_y_continuous(limits=c(0,new_val)),new_val=max_y)
  p
}

p_down <- list(p1, p2, p3, p4)%>%reset_limits(max_x=110, max_y = 350)
p_up <- list(p1_up, p2_up, p3_up, p4_up)

plot_criteria_weights <- purrr::map2(p_up,p_down,.f=function(p1,p2) (p1/p2) + plot_layout(heights = c(1,4)))%>%purrr::reduce(`|`)


pdf('one_study_ex/outs/plot_criteria_weights_c4_sc2.pdf')
plot_criteria_weights
dev.off()
##################################################
dt_final <- pvf_apply(data = dt_w, w_spec = l)

plot_mcda <- dt_final%>%
  ggplot(aes(x = trt, y = mcda)) +
  geom_boxplot()

plot_mcda

pdf('one_study_ex/outs/plot_mcda_c4_sc2.pdf')
plot_mcda
dev.off()

t.test(dt_final$mcda[dt_final$trt=='c'], dt_final$mcda[dt_final$trt=='t'])
