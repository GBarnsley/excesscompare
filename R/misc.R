convert_df_to_array <- function(df){
  target_var <- tail(names(df), 1)
  dim_vars <- setdiff(names(df), target_var)
  #sort df
  df <- arrange(df, across(all_of(dim_vars)))
  uniques <- map(dim_vars, ~unique(df[[.x]]))
  dim = rev(map_dbl(uniques, ~length(.x)))
  dimnames <- rev(uniques)
  out <- array(
    df[[target_var]],
    dim = dim,
    dimnames = dimnames
  ) %>%
    aperm(rev(seq_along(dimnames)))
  out
}
calculate_ve <- function(ves, vaccine_efficacies){
  pos <- function(x){
    if(x<0) {
      return(0)
    } else {
      return(x)
    }}
  purrr::map(vaccine_efficacies, function(eff){
    eff[1] - (pos((0.5-ves))/0.5)*(eff[1] - eff[2]) + (pos((ves-0.5))/0.5)*(eff[3] - eff[1])
  })
}
calculate_ve_map_func <- function(country_ve){
  ves <- map(country_ve$ve, ~calculate_ve(.x, country_ve[[2]]))
  #convert into a tibble
  map_dfr(ves, function(x) {map_dfc(x, ~.x)}, .id = "replicate")
}
