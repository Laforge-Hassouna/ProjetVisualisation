library(ggplot2)
library(dplyr)

# dataset

airline_delay_cause <- read.csv("./airline-delay-cause/little_dataset.csv",sep=",")

summary(airline_delay_cause)

# 1/ delay trends
airline_delay_cause %>%
  # combine arr_flights, arr_del15, arr_diverted, and arr_cancelled by year (sum)
  group_by(year) %>%
  summarise(
    arr_flights = sum(arr_flights, na.rm = TRUE),
    arr_del15 = sum(arr_del15, na.rm = TRUE),
    arr_diverted = sum(arr_diverted, na.rm = TRUE),
    arr_cancelled = sum(arr_cancelled, na.rm = TRUE)
  ) %>%
  # normalize the numbers (arr_del15, arr_diverted, arr_cancelled) by arr_flights
  mutate(
    norm_del15 = arr_del15 / arr_flights,
    norm_diverted = arr_diverted / arr_flights,
    norm_cancelled = arr_cancelled / arr_flights
  ) %>%
  # plot arr_del15, arr_diverted, and arr_cancelled for each year (different colors bar plot)
  # ...

# 2/ top of airport strategies (top of airports that prioritize delaying over diverting/cancelling, or cancellation ratios ...)
airline_delay_cause %>%
  # 

# 3/ ranking airport by reliability (weather, nas, and security delays are reliability issues, delay < cancel in terms of reliability)


# 4/ causes impact on delays (mean delay impact on delay for each cause)


# 5/ cause analysis, which causes dominate others


