library(dplyr)

#' Get a simple test table
get_test_table_1 <- function() {
  # All different
  t <- tibble::tibble(
    LopNr = seq(1, 10),
    EDATUM = "2012-11-27",
    hrt_type = "Oral progestin"
  )
  # Add our VIPs:
  # * 1 buys two things on the same day
  # * 2 buys the second thing the day after
  # * 3 buys the second thing two days after
  t <- dplyr::bind_rows(
    t,
    tibble::tibble(
      LopNr = c(1, 2, 3),
      EDATUM = c("2012-11-27", "2012-11-28", "2012-11-29"),
      hrt_type = rep("Oral estrogen", 3)
    )
  )
  t <- dplyr::arrange(t, LopNr)
  t
}

#' Get a simple test table with bug report at 2022-03-14.
#' These two rows should not be combined
get_test_table_2 <- function() {
  t <- tibble::tibble(
    LopNr = c(42, 42),
    EDATUM = c("2006-10-10", "2007-02-19"),
    hrt_type = c("Oral estrogen", "Oral progestin")
  )
  t <- dplyr::arrange(t, LopNr)
  t
}



#' Calculate the average date between two dates, rounding down
#' @param date_1 the first date, in `Date` format (see `?as.Date`)
#' @param date_2 the second date, in `Date` format (see `?as.Date`)
#' @return a date, in `Date` format (see `?as.Date`)
get_average_date <- function(
  date_1,
  date_2
) {
  # They must be Dates
  testthat::expect_s3_class(date_1, "Date")
  testthat::expect_s3_class(date_2, "Date")

  starting_date <- as.Date("1800-01-01")
  yday_1 = date_1 - starting_date
  yday_2 = date_2 - starting_date
  average_yday <- floor((yday_1 + yday_2) / 2)
  average_date <- starting_date + lubridate::days(average_yday)
  average_date
}

date_a <- as.Date("1234-01-02")
date_b <- as.Date("1234-01-03")
date_c <- as.Date("1234-01-04")

# Same date
testthat::expect_equal(get_average_date(date_1 = date_a, date_2 = date_a), date_a)
# One day difference = use first date
testthat::expect_equal(get_average_date(date_1 = date_a, date_2 = date_b), date_a)
# Two day difference = use average date
testthat::expect_equal(get_average_date(date_1 = date_a, date_2 = date_c), date_b)

#' Get the first date
#' @param date_1 the first date, in `Date` format (see `?as.Date`)
#' @param date_2 the second date, in `Date` format (see `?as.Date`)
#' @return a date, in `Date` format (see `?as.Date`)
get_first_date <- function(
  date_1,
  date_2
) {
  # They must be Dates
  testthat::expect_s3_class(date_1, "Date")
  testthat::expect_s3_class(date_2, "Date")
  date_1
}

# Same date
testthat::expect_equal(get_first_date(date_1 = date_a, date_2 = date_a), date_a)
# One day difference = use first date
testthat::expect_equal(get_first_date(date_1 = date_a, date_2 = date_b), date_a)
# Two day difference = use first date
testthat::expect_equal(get_first_date(date_1 = date_a, date_2 = date_c), date_a)

#' Calculate the average date between two collections of dates, rounding down
#' @param dates_1 the first dates, in `Date` format (see `?as.Date`)
#' @param dates_2 the second dates, in `Date` format (see `?as.Date`)
#' @return average dates, in `Date` format (see `?as.Date`)
get_average_dates <- function(
  dates_1,
  dates_2
) {
  testthat::expect_equal(length(dates_1), length(dates_2))
  average_dates <- dates_1
  for (i in seq_along(dates_1)) {
    average_dates[i] <- get_average_date(dates_1[i], dates_2[i])
  }
  average_dates
}

# Use dates that differ in zero, one and two days
testthat::expect_equal(
  get_average_dates(
    dates_1 = c(date_a, date_a, date_a),
    dates_2 = c(date_a, date_b, date_c)
  ),
  c(date_a, date_a, date_b)
)

#' Get the first dates from a collection of dates
#' @param dates_1 the first dates, in `Date` format (see `?as.Date`)
#' @param dates_2 the second dates, in `Date` format (see `?as.Date`)
#' @return average dates, in `Date` format (see `?as.Date`)
get_first_dates <- function(
  dates_1,
  dates_2
) {
  testthat::expect_equal(length(dates_1), length(dates_2))
  dates_1
}

# Use dates that differ in zero, one and two days
testthat::expect_equal(
  get_first_dates(
    dates_1 = c(date_a, date_a, date_a),
    dates_2 = c(date_a, date_b, date_c)
  ),
  c(date_a, date_a, date_a)
)


#' Calculate the average dates of each two/more dates that are maximally
#' `max_n_days_apart` apart
#' @param dates the dates, must be sorted
calc_average_dates <- function(
  dates,
  max_n_days_apart
) {
  # The average of 1 date is that date
  if (length(dates) <= 1) return(dates)

  dates_as_dates <- as.Date(dates)
  testthat::expect_false(is.unsorted(dates_as_dates))
  starting_date <- as.Date("1800-01-01")
  # message("Got dates: ", paste0(dates, collapse = ", "))

  # Calculate the distance to the starting data
  t_dates <- tibble::tibble(
    date = dates_as_dates,
    yday = dates_as_dates - starting_date,
    average_date = dates_as_dates
  )
  # From point of view of first row
  t_dates$next_date_is_close <- (c(t_dates$yday[-1], Inf) - t_dates$yday) <
    max_n_days_apart

  from_indices <- which(t_dates$next_date_is_close)
  to_indices <- from_indices + 1
  average_dates <- get_average_dates(
    dates_1 = t_dates$date[from_indices],
    dates_2 = t_dates$date[to_indices]
  )
  t_dates$average_date[from_indices] <- average_dates
  t_dates$average_date[to_indices] <- average_dates
  as.character(t_dates$average_date)
}

testthat::expect_identical(
  calc_average_dates(
    dates = c("2012-11-28", "2012-11-28"),
    max_n_days_apart = 1
  ),
  c("2012-11-28", "2012-11-28")
)
testthat::expect_identical(
  calc_average_dates(
    dates = c("2012-11-28", "2012-11-29"),
    max_n_days_apart = 10
  ),
  c("2012-11-28", "2012-11-28")
)
testthat::expect_identical(
  calc_average_dates(
    dates = c("2012-11-28", "2012-11-30"),
    max_n_days_apart = 10
  ),
  c("2012-11-29", "2012-11-29")
)
testthat::expect_identical(
  calc_average_dates(
    dates = c("2012-11-28", "2012-11-30"),
    max_n_days_apart = 1
  ),
  c("2012-11-28", "2012-11-30")
)

#' Get the first dates of each two/more dates that are maximally
#' `max_n_days_apart` apart, so these can be merged
#' @param dates the dates, must be sorted
calc_first_dates <- function(
  dates,
  max_n_days_apart,
  verbose = FALSE
) {
  # The average of 1 date is that date
  if (length(dates) <= 1) return(dates)

  dates_as_dates <- as.Date(dates)
  testthat::expect_false(is.unsorted(dates_as_dates))
  starting_date <- as.Date("1800-01-01")
  if (verbose) {
    message("Got dates: ", paste0(dates, collapse = ", "))
  }

  # Calculate the distance to the starting data
  t_dates <- tibble::tibble(
    date = dates_as_dates,
    yday = dates_as_dates - starting_date,
    first_date = dates_as_dates
  )
  # From point of view of first row
  t_dates$next_date_is_close <- (c(t_dates$yday[-1], Inf) - t_dates$yday) <
    max_n_days_apart

  from_indices <- which(t_dates$next_date_is_close)
  to_indices <- from_indices + 1
  first_dates <- get_first_dates(
    dates_1 = t_dates$date[from_indices],
    dates_2 = t_dates$date[to_indices]
  )
  # They must be Dates
  testthat::expect_s3_class(first_dates, "Date")
  t_dates$first_date[from_indices] <- first_dates
  t_dates$first_date[to_indices] <- first_dates
  as.character(t_dates$first_date)
}

testthat::expect_identical(
  calc_first_dates(
    dates = c("2012-11-28", "2012-11-28"),
    max_n_days_apart = 1
  ),
  c("2012-11-28", "2012-11-28")
)
testthat::expect_identical(
  calc_first_dates(
    dates = c("2012-11-28", "2012-11-29"),
    max_n_days_apart = 10
  ),
  c("2012-11-28", "2012-11-28")
)
testthat::expect_identical(
  calc_first_dates(
    dates = c("2012-11-28", "2012-11-30"),
    max_n_days_apart = 10
  ),
  c("2012-11-28", "2012-11-28")
)
testthat::expect_identical(
  calc_first_dates(
    dates = c("2012-11-28", "2012-11-30"),
    max_n_days_apart = 1
  ),
  c("2012-11-28", "2012-11-30")
)

# Table 2
{
  t <- get_test_table_2()
  dates <- t$EDATUM
  # If they are close enough, the 'first_date's will be the same
  testthat::expect_identical(
    calc_first_dates(dates, max_n_days_apart = 10000),
    rep(dates[1], 2)
  )
  # Nothing happens when these are too far apart
  testthat::expect_identical(
    calc_first_dates(dates, max_n_days_apart = 10),
    dates
  )
}



#' Add an extra column, `average_date` that is
#' the average of two dates that are maximally 'max_n_days_apart' apart
#' @param t a tibble, with columns:
#'  * `LopNr`: unique person ID
#'  * `EDATUM`: date of the purchase
#' @param max_n_days_apart maximum number of two dates apart to be averaged
#' @return the same table with a column `average_date` added
add_average_date <- function(
  t,
  max_n_days_apart = 14
) {
  testthat::expect_true("LopNr" %in% names(t))
  testthat::expect_true("EDATUM" %in% names(t))

  # From the cheat sheet at https://dplyr.tidyverse.org/
  t %>%
    dplyr::select_all() %>%
    dplyr::group_by(LopNr) %>%
    dplyr::mutate(average_date = calc_average_dates(EDATUM, max_n_days_apart))
}

#' Add an extra column, `first_date` that is
#' the first of two dates that are maximally 'max_n_days_apart' apart
#' @param t a tibble, with columns:
#'  * `LopNr`: unique person ID
#'  * `EDATUM`: date of the purchase
#' @param max_n_days_apart maximum number of two dates apart to be averaged
#' @return the same table with a column `average_date` added
add_first_date <- function(
  t,
  max_n_days_apart = 14
) {
  testthat::expect_true("LopNr" %in% names(t))
  testthat::expect_true("EDATUM" %in% names(t))

  # From the cheat sheet at https://dplyr.tidyverse.org/
  t %>%
    dplyr::select_all() %>%
    dplyr::group_by(LopNr) %>%
    dplyr::mutate(first_date = calc_first_dates(EDATUM, max_n_days_apart))
}

# Table 2
{
  t <- get_test_table_2()
  t_with_first_date <- add_first_date(t, max_n_days_apart = 14)
  testthat::expect_identical(t$EDATUM, t_with_first_date$first_date)
}

# Combine treatments
combine_hrts <- function(hrt_types) {
  # Probably need to add more here :-)
  progesting_str <- "Oral progestin"
  estrogen_str <- "Oral estrogen"
  combined_str <- "Combined"
  valid_hrt_types <- c(progesting_str, estrogen_str, combined_str)
  testthat::expect_true(all(hrt_types %in% valid_hrt_types))
  if (length(hrt_types) <= 1) return(hrt_types)

  has_progestin <- any(progesting_str %in% hrt_types)
  has_estrogen <- any(estrogen_str %in% hrt_types)
  if (has_progestin && has_estrogen) return(combined_str)
  hrt_types[1]
}

testthat::expect_equal(
  combine_hrts(c("Oral progestin", "Oral progestin")),
  "Oral progestin"
)
testthat::expect_equal(
  combine_hrts(c("Oral estrogen", "Oral estrogen")),
  "Oral estrogen"
)
testthat::expect_equal(
  combine_hrts(c("Oral progestin", "Oral estrogen")),
  "Combined"
)


#' When women but two different oral contraceptives, this is a combined
#' treatment. However, these two different oral contraceptives can
#' possibly be purchaes over multiple days.
#'
#' This function combines those purchases, merging two purchases into one
#'
#' @param t a tibble, with columns:
#'  * `LopNr`: unique person ID
#'  * `EDATUM`: date of the purchase
#' @param max_n_days_apart maximum number of two dates apart to be averaged
#' @return the same table with the purchases combined
combine_purchases_by_average_date <- function(
  t,
  max_n_days_apart = 14
) {
  # Determine an average data, so we can combine purchases on dates that are
  # more or less similar
  t_with_average_date <- add_average_date(t)

  dplyr::select_all(t_with_average_date) %>%
    dplyr::group_by(LopNr, average_date) %>%
    dplyr::summarise(new_hrt_type = combine_hrts(hrt_type))
}

#' When women but two different oral contraceptives, this is a combined
#' treatment. However, these two different oral contraceptives can
#' possibly be purchaes over multiple days.
#'
#' This function combines those purchases, merging two purchases into one,
#' using the first date
#'
#' @param t a tibble, with columns:
#'  * `LopNr`: unique person ID
#'  * `EDATUM`: date of the purchase
#' @param max_n_days_apart maximum number of two dates apart to be averaged
#' @return the same table with the purchases combined
combine_purchases_by_first_date <- function(
  t,
  max_n_days_apart = 14
) {
  # Determine an average data, so we can combine purchases on dates that are
  # more or less similar
  t_with_first_date <- add_first_date(t)

  dplyr::select_all(t_with_first_date) %>%
    dplyr::group_by(LopNr, first_date) %>%
    dplyr::summarise(new_hrt_type = combine_hrts(hrt_type))
}


# Table 1
{
  t <- get_test_table_1()
  n_rows_before <- nrow(t)

  # We need to group by an approximate day
  t_fixed <- combine_purchases_by_first_date(t, max_n_days_apart = 14)

  n_rows_after <- nrow(t_fixed)
  testthat::expect_true(n_rows_after < n_rows_before)
}

# Table 2
{
  t <- get_test_table_2()
  n_rows_before <- nrow(t)

  # We need to group by an approximate day
  # As these days are far apart, no merge here
  t_fixed <- combine_purchases_by_first_date(t, max_n_days_apart = 14)

  n_rows_after <- nrow(t_fixed)
  testthat::expect_equal(n_rows_after, n_rows_before)

  # Should not be combined
  testthat::expect_equal(2, nrow(t_fixed[t_fixed$LopNr == 42, ]))
}

