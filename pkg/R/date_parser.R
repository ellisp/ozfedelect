
#' Parse a date column into start and end dates
#' 
#' @param data a data frame with a column called \code{dates}
#' @export
#' @importFrom stringr str_extract str_sub str_length str_squish
#' @importFrom dplyr if_else select
parse_dates <- function(data){
  fixed <- data %>%
    select(dates) %>%
    mutate(end_year =       stringr::str_extract(dates, "[0-9]*$"),
           end_year =       ifelse(stringr::str_length(end_year) == 2, paste0("20", end_year), end_year),
           month_one =      stringr::str_extract(dates, "[A-Z][a-z][a-z]"),
           month_two =      stringr::str_sub(str_extract(dates, "[A-Z][a-z][a-z].*[A-Z][a-z][a-z]"), start = -3),
           month_two =      dplyr::if_else(is.na(month_two), month_one, month_two),
           day_one =        stringr::str_extract(dates, "[0-9]+"),
           dates_without_day_one = gsub("^[0-9]+", "", dates),
           day_two =        stringr::str_extract(dates_without_day_one, "[0-9]+"),
           day_two =        stringr::str_squish(gsub("[-–]", "", day_two)),
           day_three_four = stringr::str_extract(dates, "[/ ].+[-–] *[0-9]+"),
           day_three =      stringr::str_extract(day_three_four, "[/ ] *[0-9]+"),
           day_three =      stringr::str_squish(gsub("/", "", day_three)),
           day_four =       stringr::str_extract(day_three_four, "[-–] *[0-9]+"),
           day_four =       stringr::str_squish(gsub("[-–]", "", day_four))
    ) %>%
    # dates that are only a single day:
    dplyr::mutate(day_two = dplyr::if_else(is.na(day_two) | as.numeric(day_two) > 1000, day_one, day_two)) %>%
    # dates that actually have four days:
    dplyr::mutate(day_one = ifelse(is.na(day_three),
                            day_one,
                            round((as.numeric(day_one) + as.numeric(day_two)) / 2)),
           day_two = ifelse(is.na(day_three),
                            day_two,
                            round((as.numeric(day_three) + as.numeric(day_four)) / 2))) %>%
    dplyr::select(-day_three_four, -dates_without_day_one) %>%
    
    dplyr::mutate(start_date = as.Date(paste(end_year,month_one, day_one, sep = "-"), format = "%Y-%b-%d"),
           end_date = as.Date(paste(end_year,month_two, day_two, sep = "-"), format = "%Y-%b-%d")) %>%
    dplyr::select(start_date, end_date) %>%
    cbind(data) %>%
    tibble::as_tibble() %>%
    rename(original_dates = dates)
  
  return(fixed)
}
  

