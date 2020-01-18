library(dplyr)
library(ggplot2)

source('funs/plot_pp.R')
######################
# MDCA C3 Scenario 1 #
######################

setting <- list(p_miss = rep(seq(0.5, 0.9, 0.1), 3),
                mi_method = c(rep('norm', 10), rep('cart', 5)), 
                truncTF = c(rep(TRUE, 5), rep(FALSE, 5), rep(TRUE, 5))) 

setting <- list(p_miss = rep(seq(0.5, 0.9, 0.1), 1),
                mi_method = rep('norm', 5),
                truncTF = c(rep(TRUE, 5)))


sum_c3_sc1 <- 
  purrr::pmap_df(setting, .f = function(p_miss, mi_method, truncTF){
  
  xx <- readRDS(sprintf('results/mcda_c3_sc1_pmiss%d_%s%s.rds', 
                        100*p_miss, mi_method, truncTF))
  
  xx%>%
    purrr::map_df(.f = function(x) x$br_result, .id = 'sim')%>%
    dplyr::group_by(meth, res)%>%
    dplyr::summarise(pp = n()/length(xx))%>%
    dplyr::ungroup()%>%
    dplyr::mutate(p_obs = 1 - p_miss,
                  mi_method = mi_method,
                  truncTF = truncTF,
                  meth = ifelse(meth!='mi', meth, paste(mi_method, 'trunc =', truncTF)))%>%
    dplyr::filter(res=='benefit')
})%>%
  dplyr::filter(!(meth=='obs'&truncTF))%>%
  dplyr::mutate(
    p_obs = scales::percent(p_obs, accuracy = 1))

plot_sum_c3_sc1 <- plot_pp(sum_c3_sc1)

pdf('plots/plot_sum_c3_sc1.pdf')
plot_sum_c3_sc1
dev.off()

sum_c3_sc1d <- sum_c3_sc1%>%
  dplyr::filter(meth!='all')%>%
  dplyr::mutate(diff_all = 100*abs(pp - sum_c3_sc1$pp[sum_c3_sc1$meth=='all'][[1]]))

ch_sum_c3_sc1 <- 
  purrr::pmap_df(setting, .f = function(p_miss, mi_method, truncTF){
    
    xx <- readRDS(sprintf('results/mcda_c3_sc1_pmiss%d_%s%s.rds', 
                          100*p_miss, mi_method, truncTF))
    
    xx%>%
      purrr::map_df(.f = function(x) x$br_comp, .id = 'sim')%>%
      dplyr::group_by(meth)%>%
      dplyr::summarise_at(c('mean_diff', 'se_diff'), 'mean')%>%
      dplyr::mutate(p_obs = 1 - p_miss,
                    mi_method = mi_method,
                    truncTF = truncTF)
  })%>%
  dplyr::filter(!(meth=='obs'&truncTF))

######################
# MDCA C3 Scenario 2 #
######################

setting <- list(p_miss = rep(seq(0.5, 0.9, 0.1), 3),
                mi_method = c(rep('norm', 10), rep('cart', 5)), 
                truncTF = c(rep(TRUE, 5), rep(FALSE, 5), rep(TRUE, 5))) 

sum_c3_sc2 <- 
  purrr::pmap_df(setting, .f = function(p_miss, mi_method, truncTF){
    
    xx <- readRDS(sprintf('results/mcda_c3_sc2_pmiss%d_%s%s.rds', 
                          100*p_miss, mi_method, truncTF))
    
    xx%>%
      purrr::map_df(.f = function(x) x$br_result, .id = 'sim')%>%
      dplyr::group_by(meth, res)%>%
      dplyr::summarise(pp = n()/length(xx))%>%
      dplyr::ungroup()%>%
      dplyr::mutate(p_obs = 1 - p_miss,
                    mi_method = mi_method,
                    truncTF = truncTF,
                    meth = ifelse(meth!='mi', meth, paste(mi_method, 'trunc =', truncTF)))%>%
      dplyr::filter(res=='benefit')
  })%>%
  dplyr::filter(!(meth=='obs'&truncTF))%>%
  dplyr::mutate(
    p_obs = scales::percent(p_obs, accuracy = 1))

plot_sum_c3_sc2 <- plot_pp(sum_c3_sc2)

pdf('plots/plot_sum_c3_sc2.pdf')
plot_sum_c3_sc2
dev.off()

sum_c3_sc2d <- sum_c3_sc2%>%
  dplyr::filter(meth!='all')%>%
  dplyr::mutate(diff_all = 100*abs(pp - sum_c3_sc2$pp[sum_c3_sc2$meth=='all'][[1]]))

ch_sum_c3_sc2 <- 
  purrr::pmap_df(setting, .f = function(p_miss, mi_method, truncTF){
    
    xx <- readRDS(sprintf('results/mcda_c3_sc2_pmiss%d_%s%s.rds', 
                          100*p_miss, mi_method, truncTF))
    
    xx%>%
      purrr::map_df(.f = function(x) x$br_comp, .id = 'sim')%>%
      dplyr::group_by(meth)%>%
      dplyr::summarise_at(c('mean_diff', 'se_diff'), 'mean')%>%
      dplyr::mutate(p_obs = 1 - p_miss,
                    mi_method = mi_method,
                    truncTF = truncTF)
  })%>%
  dplyr::filter(!(meth=='obs'&truncTF))

######################
# MDCA C4 Scenario 1 #
######################

setting <- list(p_miss = rep(seq(0.5, 0.9, 0.1), 3),
                mi_method = c(rep('norm', 10), rep('cart', 5)), 
                truncTF = c(rep(TRUE, 5), rep(FALSE, 5), rep(TRUE, 5))) 

sum_c4_sc1 <- 
  purrr::pmap_df(setting, .f = function(p_miss, mi_method, truncTF){
    
    xx <- readRDS(sprintf('results/mcda_c4_sc1_pmiss%d_%s%s.rds', 
                          100*p_miss, mi_method, truncTF))
    
    xx%>%
      purrr::map_df(.f = function(x) x$br_result, .id = 'sim')%>%
      dplyr::group_by(meth, res)%>%
      dplyr::summarise(pp = n()/length(xx))%>%
      dplyr::ungroup()%>%
      dplyr::mutate(p_obs = 1 - p_miss,
                    mi_method = mi_method,
                    truncTF = truncTF,
                    meth = ifelse(meth!='mi', meth, paste(mi_method, 'trunc =', truncTF)))%>%
      dplyr::filter(res=='benefit')
  })%>%
  dplyr::filter(!(meth=='obs'&truncTF))%>%
  dplyr::mutate(
    p_obs = scales::percent(p_obs, accuracy = 1))

plot_sum_c4_sc1 <- plot_pp(sum_c4_sc1)

pdf('plots/plot_sum_c4_sc1.pdf')
plot_sum_c4_sc1
dev.off()

sum_c4_sc1d <- sum_c4_sc1%>%
  dplyr::filter(meth!='all')%>%
  dplyr::mutate(diff_all = 100*abs(pp - sum_c4_sc1$pp[sum_c4_sc1$meth=='all'][[1]]))

ch_sum_c4_sc1 <- 
  purrr::pmap_df(setting, .f = function(p_miss, mi_method, truncTF){
    
    xx <- readRDS(sprintf('results/mcda_c4_sc1_pmiss%d_%s%s.rds', 
                          100*p_miss, mi_method, truncTF))
    
    xx%>%
      purrr::map_df(.f = function(x) x$br_comp, .id = 'sim')%>%
      dplyr::group_by(meth)%>%
      dplyr::summarise_at(c('mean_diff', 'se_diff'), 'mean')%>%
      dplyr::mutate(p_obs = 1 - p_miss,
                    mi_method = mi_method,
                    truncTF = truncTF)
  })%>%
  dplyr::filter(!(meth=='obs'&truncTF))

######################
# MDCA C4 Scenario 2 #
######################

setting <- list(p_miss = rep(seq(0.5, 0.9, 0.1), 3),
                mi_method = c(rep('norm', 10), rep('cart', 5)), 
                truncTF = c(rep(TRUE, 5), rep(FALSE, 5), rep(TRUE, 5))) 

sum_c4_sc2 <- 
  purrr::pmap_df(setting, .f = function(p_miss, mi_method, truncTF){
    
    xx <- readRDS(sprintf('results/mcda_c4_sc2_pmiss%d_%s%s.rds', 
                          100*p_miss, mi_method, truncTF))
    
    xx%>%
      purrr::map_df(.f = function(x) x$br_result, .id = 'sim')%>%
      dplyr::group_by(meth, res)%>%
      dplyr::summarise(pp = n()/length(xx))%>%
      dplyr::ungroup()%>%
      dplyr::mutate(p_obs = 1 - p_miss,
                    mi_method = mi_method,
                    truncTF = truncTF,
                    meth = ifelse(meth!='mi', meth, paste(mi_method, 'trunc =', truncTF)))%>%
      dplyr::filter(res=='benefit')
  })%>%
  dplyr::filter(!(meth=='obs'&truncTF))%>%
  dplyr::mutate(
    p_obs = scales::percent(p_obs, accuracy = 1))

plot_sum_c4_sc2 <- plot_pp(sum_c4_sc2)

pdf('plots/plot_sum_c4_sc2.pdf')
plot_sum_c4_sc2
dev.off()

sum_c4_sc2d <- sum_c4_sc2%>%
  dplyr::filter(meth!='all')%>%
  dplyr::mutate(diff_all = 100*abs(pp - sum_c4_sc2$pp[sum_c4_sc2$meth=='all'][[1]]))

ch_sum_c4_sc2 <- 
  purrr::pmap_df(setting, .f = function(p_miss, mi_method, truncTF){
    
    xx <- readRDS(sprintf('results/mcda_c4_sc2_pmiss%d_%s%s.rds', 
                          100*p_miss, mi_method, truncTF))
    
    xx%>%
      purrr::map_df(.f = function(x) x$br_comp, .id = 'sim')%>%
      dplyr::group_by(meth)%>%
      dplyr::summarise_at(c('mean_diff', 'se_diff'), 'mean')%>%
      dplyr::mutate(p_obs = 1 - p_miss,
                    mi_method = mi_method,
                    truncTF = truncTF)
  })%>%
  dplyr::filter(!(meth=='obs'&truncTF))



#comparison with Wen et al.
x1 <- readRDS('results/mcda_c3_sc2_all_wencomp.rds')

x1%>%
  purrr::map_df(.f = function(x) x$br_result, .id = 'sim')%>%
  dplyr::group_by(meth, res)%>%
  dplyr::summarise(pp = n()/length(x1))%>%
  dplyr::filter(res=='benefit')

xx <- x1%>%
  purrr::map_dbl(.f = function(x) x$wen_uci)
mean(ifelse(xx < 0, 1, 0))


