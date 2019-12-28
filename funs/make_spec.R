make_spec <- function(..., p100_weights){
  
  dots <- list(...)

  if(!identical(length(dots),length(p100_weights)))
    stop('Number of weights not equal to length of "p100_weights"')
  
  if(sum(p100_weights)!=1)
    stop(sprintf("Sum of 'p100_weights' equal to %s, instead of 1", sum(p100_weights)))
  
  purrr::map2(dots, p100_weights, .f = function(x, y){
    x$p100_weights = y
    x
  })
  
}




