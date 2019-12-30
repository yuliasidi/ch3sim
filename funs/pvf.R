pvf <- function(var, high, low, type = c('benefit','risk')){
  
  if(type == 'benefit'){
    (var - low) / (high - low)  
  }

  else{
    (high - var) / (high - low)  
  }
  
  
}
