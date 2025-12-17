library(ggplot2)
library(dplyr)

# dataset

airline_delay_cause <- read.csv("./airline-delay-cause/little_dataset.csv",sep=",")

summary(airline_delay_cause)

# 1/ delay trends


# 2/ top of airport strategies (top of airports that prioritize delaying over diverting/cancelling, or cancellation ratios ...)


# 3/ ranking airport by reliability (weather, nas, and security delays are reliability issues, delay < cancel in terms of reliability)


# 4/ causes impact on delays (mean delay impact on delay for each cause)


# 5/ cause analysis, which causes dominate others


