pvf <- function(var, best, worst, type = c('benefit','risk')){
  
  if(type == 'benefit'){
    (var - worst) / (best - worst)  
  }
  
  else{
    (worst - var) / (worst - best)  
  }
  
  
}
