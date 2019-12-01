check_bl_fun_out <- function(dt, grp, v){
  dt%>%
    dplyr::group_by(trt,!!rlang::sym(grp))%>%
    dplyr::summarise(m = mean(!!rlang::sym(v)))%>%
    dplyr::group_split()%>%
    purrr::map_df(.f = function(x){
      tibble::tibble(trt=x$trt[1], d = diff(x[['m']])) # the difference here is 1 - 0
    })%>%
    dplyr::mutate(
      var1 = grp,
      var2 = v
    )
}


