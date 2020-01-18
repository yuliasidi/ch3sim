
######################
# MDCA C3 Scenario 1 #
######################

setting <- list(p_miss = rep(seq(0.5, 0.9, 0.1), 3),
                          mi_method = c(rep('norm', 10), rep('cart', 5)), 
                          truncTF = c(rep(TRUE, 5), rep(FALSE, 5), rep(TRUE, 5))) 

purrr::pwalk(.l = setting,
             .f = function(p_miss, mi_method, truncTF){
               cat(
                 whisker::whisker.render(
                   readLines('tmpls/mcda_c3_sc1.tmpl'),
                   data = list(
                     p_miss = p_miss,
                     mi_method = mi_method,
                     truncTF = truncTF)
                 ),
                 file = file.path('pgms_sim',
                                  sprintf('mcda_c3_sc1_pmiss%d_%s%s.R', 
                    100*p_miss, mi_method, truncTF)
                 ),
                 sep='\n')
             })

######################
# MDCA C3 Scenario 2 #
######################

setting <- list(p_miss = rep(seq(0.5, 0.9, 0.1), 3),
                mi_method = c(rep('norm', 10), rep('cart', 5)), 
                truncTF = c(rep(TRUE, 5), rep(FALSE, 5), rep(TRUE, 5))) 

purrr::pwalk(.l = setting,
             .f = function(p_miss, mi_method, truncTF){
               cat(
                 whisker::whisker.render(
                   readLines('tmpls/mcda_c3_sc2.tmpl'),
                   data = list(
                     p_miss = p_miss,
                     mi_method = mi_method,
                     truncTF = truncTF)
                 ),
                 file = file.path('pgms_sim',
                                  sprintf('mcda_c3_sc2_pmiss%d_%s%s.R', 
                                          100*p_miss, mi_method, truncTF)
                 ),
                 sep='\n')
             })
######################
# MDCA C4 Scenario 1 #
######################

setting <- list(p_miss = rep(seq(0.5, 0.9, 0.1), 3),
                mi_method = c(rep('norm', 10), rep('cart', 5)), 
                truncTF = c(rep(TRUE, 5), rep(FALSE, 5), rep(TRUE, 5))) 

purrr::pwalk(.l = setting,
             .f = function(p_miss, mi_method, truncTF){
               cat(
                 whisker::whisker.render(
                   readLines('tmpls/mcda_c4_sc1.tmpl'),
                   data = list(
                     p_miss = p_miss,
                     mi_method = mi_method,
                     truncTF = truncTF)
                 ),
                 file = file.path('pgms_sim',
                                  sprintf('mcda_c4_sc1_pmiss%d_%s%s.R', 
                                          100*p_miss, mi_method, truncTF)
                 ),
                 sep='\n')
             })

######################
# MDCA C4 Scenario 2 #
######################

setting <- list(p_miss = rep(seq(0.5, 0.9, 0.1), 3),
                mi_method = c(rep('norm', 10), rep('cart', 5)), 
                truncTF = c(rep(TRUE, 5), rep(FALSE, 5), rep(TRUE, 5))) 

purrr::pwalk(.l = setting,
             .f = function(p_miss, mi_method, truncTF){
               cat(
                 whisker::whisker.render(
                   readLines('tmpls/mcda_c4_sc2.tmpl'),
                   data = list(
                     p_miss = p_miss,
                     mi_method = mi_method,
                     truncTF = truncTF)
                 ),
                 file = file.path('pgms_sim',
                                  sprintf('mcda_c4_sc2_pmiss%d_%s%s.R', 
                                          100*p_miss, mi_method, truncTF)
                 ),
                 sep='\n')
             })
