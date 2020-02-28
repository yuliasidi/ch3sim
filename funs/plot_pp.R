plot_pp <- function(data){
  
  ggplot() +
    geom_bar(position = position_dodge(),
             stat='identity',
             aes(x = p_obs, y = pp, fill = meth),
             data = data%>%dplyr::filter(meth!='all')) +
    geom_hline(aes(yintercept = pp), linetype=2,
               data = data%>%dplyr::filter(meth=='all')) +
    labs(x = '% of patients with observed preferences', 
         y = '% of trials with beneficial BR for Trt vs Con') +
    theme_bw() +
    theme(legend.position = 'bottom') +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_discrete(name = 'Method', labels = c("CART", 'NORM','NORM TRUNCATED',"CCA"))
  
}  

