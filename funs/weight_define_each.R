weight_define_each <- function(data, name_weight, br_spec, ..., w_mu, w_sd){
 
  #number of rows in the first tibble is equal to the number of weights
  #number of columns is affected by the number of variables used to define each weight
  
  if(!name_weight%in%names(data)){
    stop(sprintf("'%s' must be a column in input data",name_weight))
  }
  
  dots <- c(...)
  data_u <- unique(data[,dots])
  data_u <- eval(parse(text = sprintf('data_u[order(%s),]',paste0(sprintf("data_u[['%s']]",dots),collapse = ', '))))
  data_cross <- tidyr::crossing(data_u)
  data_cross$wname <- name_weight
  data_cross$br <- br_spec
  data_cross$w_mu <- w_mu
  data_cross$w_sd <- w_sd
  
  data_cross[,c('wname','br',dots,'w_mu','w_sd')]
  
}
