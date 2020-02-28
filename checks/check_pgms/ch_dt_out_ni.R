
library(dplyr, warn.conflicts = F, quietly = T)

source('dt_sim.R')



x1 <- parallel::mclapply(X = 1:1000,
                         mc.cores = 7,
                         FUN = function(i){
                           
#generate simulated data to be used with weights

set.seed(888*i)

dt_out <- dt_sim()

comp <- t.test(dt_out$bcva_48w[dt_out$trt=='c'], dt_out$bcva_48w[dt_out$trt=='t'])
out <- comp$conf.int[2]

out <- list(out)%>%
  purrr::set_names('out')

return(out)

})

x2 <- x1%>%purrr::map_dbl(.f = function(x) x$out)

sum(ifelse(x2 < 4 , 1, 0))
