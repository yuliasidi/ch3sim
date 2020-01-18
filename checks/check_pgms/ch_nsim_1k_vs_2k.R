x1 <- readRDS('results/mcda_c3_sc2_pmiss90_normFALSE.rds')
x2 <- readRDS('results/mcda_c3_sc2_pmiss90_normFALSE_2k.rds')

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
