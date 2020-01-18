map_prob <- function(data, probs, col){
  
  ret <- mapply(function(x, p) {
    x$miss <- stats::rbinom(nrow(x), 1, p)
    x
  },
  x = split(data,data[[col]]),
  p = probs,
  SIMPLIFY = FALSE)
  
  do.call('rbind', ret)  
}