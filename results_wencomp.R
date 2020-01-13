library(dplyr)

#compare results from observing all the weights, and using only mean weights as fixed values and the method
#as described un Wen etl al.

################
## Scenario 1 ##
################

x1 <- readRDS('results/mcda_c3_sc1_all_wencomp.rds')

x1%>%
  purrr::map_df(.f = function(x) x$br_result, .id = 'sim')%>%
  dplyr::group_by(meth, res)%>%
  dplyr::summarise(pp = n()/length(x1))%>%
  dplyr::filter(res=='benefit')

xx <- x1%>%
  purrr::map_dbl(.f = function(x) x$wen_uci)
mean(ifelse(xx < 0, 1, 0))

# -> all the weights return 2.2% of benefit, while Wen et al 16.8%

################
## Scenario 2 ##
################

x1 <- readRDS('results/mcda_c3_sc2_all_wencomp.rds')

x1%>%
  purrr::map_df(.f = function(x) x$br_result, .id = 'sim')%>%
  dplyr::group_by(meth, res)%>%
  dplyr::summarise(pp = n()/length(x1))%>%
  dplyr::filter(res=='benefit')

xx <- x1%>%
  purrr::map_dbl(.f = function(x) x$wen_uci)
mean(ifelse(xx < 0, 1, 0))

# -> all the weights return 18.8% of benefit, while Wen et al 25.2%
