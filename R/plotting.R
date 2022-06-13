
format_number <- function(x){
  stringr::str_trim(case_when(x >= 10000 ~ format(signif(x, digits = 4), scientific = FALSE, digits = 4, big.mark = ","),
            x >= 10 ~ format(round(x, -1), big.mark = ","),
            TRUE ~ format(round(x), big.mark = ",")))
}
format_per <- function(x){
  as.character(signif(x, digits = 4))
}
format_quantile <- function(central, lower, upper){
  paste0(central, " (", lower, " - ", upper, ")")
}
format_model_est <- function(x, format_func = format_number){
  if(all(is.na(x))){
    "-"
  } else if (length(x) == 1) {
    format_func(x)
  } else {
    format_quantile(
      format_func(median(x)),
      format_func(quantile(x, 0.025)),
      format_func(quantile(x, 0.975))
    )
  }
}
summarise_by <- function(df, grouping, per = "total"){
  df <- ungroup(df)
  #randomly choose 2000 replicate combinations
  temp <- map_dfr(seq_len(100), function(it){
    #for each type and country randomly choose a replicate
    reps <- df %>%
      group_by(iso3c, type) %>%
      select(iso3c, type, replicate) %>%
      summarise(
        replicate = sample(replicate, 1),
        .groups = "drop"
      ) %>%
      complete(
        iso3c = unique(temp$iso3c),
        type = unique(temp$type),
        fill = list(replicate = -999)
      ) %>%
      mutate(
        iso3c_type = paste0(iso3c, ":", type)
      ) %>%
      pull(replicate, iso3c_type)
    #calculate totals
    df %>%
      filter(replicate == reps[paste0(iso3c, ":", type)]) %>%
      group_by_at(c("type", grouping)) %>%
      summarise(across(c(deaths,deaths_averted),
                       ~sum(.x[.x>0])),
                pop = sum(pop),
                doses = sum(doses),
                .groups = "drop")
  }) %>%
    group_by_at(c("type", grouping))
  if(per == "total"){
    format_func <- format_number
  } else if(per == "capita"){
    format_func <- format_per
    temp <- temp %>%
      mutate(
        across(c(deaths,deaths_averted),
               ~.x/pop * 10000,
               .groups = "drop"
        )
      )
  } else if(per == "vaccine"){
    format_func <- format_per
    temp <- temp %>%
      mutate(
        across(c(deaths,deaths_averted),
               ~.x/doses * 10000,
               .groups = "drop"
        )
      )
  }
  temp <- temp %>%
    summarise(
      across(c(deaths,deaths_averted),
             ~format_model_est(unique(.x), format_func = format_func)
      ),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = type, values_from = c(deaths, deaths_averted))
  if(per == "capita") {
    temp <- temp %>%
      transmute(
        across(
          all_of(grouping), ~.x
        ),
        across(!all_of(grouping),
               ~.x,
               .names = "{.col}_per_capita"
        )
      )
  } else if (per == "vaccine") {
    temp <- temp %>%
      transmute(
        across(
          all_of(grouping), ~.x
        ),
        across(!all_of(grouping),
          ~.x,
          .names = "{.col}_per_vaccine"
        )
      )
  }
  if(!is.null(grouping)){
    temp %>%
      mutate(
        across(all_of(grouping),
               ~paste0("  ", as.character(.x)),
               .names = " ")
      ) %>%
      select(!all_of(grouping))
  } else {
    temp %>%
      mutate(
        ` ` = "Worldwide"
      )
  }
}
