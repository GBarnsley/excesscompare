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
