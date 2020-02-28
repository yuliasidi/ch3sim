x1 <- readRDS('results/mcda_c4_sc2_pmiss90_cartTRUE_mar.rds')
x2 <- readRDS('results/mcda_c4_sc2_pmiss90_cartTRUE_mar_5l.rds')


x1%>%
  purrr::map_df(.f = function(x) x$br_result, .id = 'sim')%>%
  dplyr::group_by(meth, res)%>%
  dplyr::summarise(pp = n()/length(x1))%>%
  dplyr::filter(res=='benefit')

x2%>%
  purrr::map_df(.f = function(x) x$br_result, .id = 'sim')%>%
  dplyr::group_by(meth, res)%>%
  dplyr::summarise(pp = n()/length(x2))%>%
  dplyr::filter(res=='benefit')
