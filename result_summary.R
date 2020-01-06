library(dplyr)

x1 <- readRDS('results/mcda_c2_sc1_pmiss90.rds')
x1 <- readRDS('results/mcda_c2_sc1_pmiss90_sdhigher.rds')
x1 <- readRDS('results/mcda_c2_sc1_pmiss90_sdlower.rds')
x1 <- readRDS('results/mcda_c2_sc1_pmiss90_m20.rds')

x1%>%
  purrr::map_df(.f = function(x) x$br_result, .id = 'sim')%>%
  dplyr::group_by(meth, res)%>%
  dplyr::summarise(pp = n()/length(x1))%>%
  dplyr::filter(res=='benefit')

do_rates <- seq(50, 90, 10)


sum <- 
  purrr::map_df(do_rates, .f = function(y){
  
  xx <- readRDS(sprintf('results/mcda_c2_sc1_pmiss%d.rds', y))
  
  xx%>%
    purrr::map_df(.f = function(x) x$br_result, .id = 'sim')%>%
    dplyr::group_by(meth, res)%>%
    dplyr::summarise(pp = n()/length(xx))%>%
    dplyr::mutate(dor = y)%>%
    dplyr::filter(res=='benefit')
})


