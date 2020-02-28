
x1 <- readRDS('results/mcda_c4_sc2_pmiss50_normFALSE_mar_m20.rds')
x1 <- readRDS('results/mcda_c4_sc2_pmiss90_cartTRUE_mar.rds')
x1 <- readRDS('results/mcda_c4_sc2_pmiss50_normFALSE_mar2_2k.rds')


x1%>%
  purrr::map_df(.f = function(x) x$br_result, .id = 'sim')%>%
  dplyr::group_by(meth, res)%>%
  dplyr::summarise(pp = n()/length(x1))%>%
  dplyr::filter(res=='benefit')

x1%>%
  purrr::map_df(.f = function(x) x$br_comp, .id = 'sim')%>%
  dplyr::group_by(meth)%>%
  dplyr::summarise_at(c('mean_diff', 'se_diff'), 'mean')

x1%>%
  purrr::map_df(.f = function(x) x$do_t, .id = 'sim')%>%group_by(trt)%>%summarise(mean(do))

x1%>%
  purrr::map_df(.f = function(x) x$w_sum, .id = 'sim')%>%select(-sim)%>%group_by(miss)%>%summarise_all(mean)

x1%>%
  purrr::map_df(.f = function(x) x$do_bl, .id = 'sim')%>%
  filter(miss==0, cstc_bl == 'low')%>%
  summarise(mean_obs = mean(`n()`))
  
  