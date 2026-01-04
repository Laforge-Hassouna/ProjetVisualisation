library(ggplot2)
library(dplyr)
library(tidyr)

# dataset

airline_delay_cause <- read.csv("./airline-delay-cause/little_dataset.csv",sep=",")

summary(airline_delay_cause)

# 1/ delay trends
# Évolution annuelle des retards / déroutements / annulations (normalisés)
airline_delay_cause %>%
  group_by(year) %>%
  summarise(
    arr_flights = sum(arr_flights, na.rm = TRUE),
    arr_del15 = sum(arr_del15, na.rm = TRUE),
    arr_diverted = sum(arr_diverted, na.rm = TRUE),
    arr_cancelled = sum(arr_cancelled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    delay_15 = arr_del15 / arr_flights,
    diverted = arr_diverted / arr_flights,
    cancelled = arr_cancelled / arr_flights
  ) %>%
  select(year, delay_15, cancelled, diverted) %>%
  pivot_longer(-year, names_to = "type", values_to = "ratio") %>%
  mutate(
    type = factor(type, levels = c("delay_15", "cancelled", "diverted"))
  ) %>%
  ggplot(aes(x = factor(year), y = ratio, fill = type)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Tendances annuelles des perturbations aériennes",
    x = "Année",
    y = "Ratio par vol",
    fill = "Type"
  ) +
  theme_minimal()



# 2/ top of airport strategies (top of airports that prioritize delaying over diverting/cancelling, or cancellation ratios ...)
 

 #calcul des ratios par aéroport : 
airport_strategies <- airline_delay_cause %>%
  group_by(airport, airport_name) %>%
  summarise(
    flights = sum(arr_flights, na.rm = TRUE),
    delayed = sum(arr_del15, na.rm = TRUE),
    cancelled = sum(arr_cancelled, na.rm = TRUE),
    diverted = sum(arr_diverted, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    delay_ratio = delayed / flights,
    cancel_ratio = cancelled / flights,
    divert_ratio = diverted / flights
  ) %>%
  filter(flights > 500)   # évite les petits aéroports non représentatifs


# Aéroports qui privilégient le retard (plutôt que annuler/dériver)
top_delay_airports <- airport_strategies %>%
  mutate(
    disruption_ratio = cancel_ratio + divert_ratio
  ) %>%
  arrange(desc(delay_ratio)) %>%
  slice_head(n = 10)

top_delay_airports %>%
  select(airport, airport_name, delay_ratio, cancel_ratio, divert_ratio)

# Aéroports avec fort taux d’annulation
top_cancel_airports <- airport_strategies %>%
  arrange(desc(cancel_ratio)) %>%
  slice_head(n = 10)

top_cancel_airports %>%
  select(airport, airport_name, cancel_ratio, delay_ratio, divert_ratio)

#Aéroports avec fort taux de déroutement

top_divert_airports <- airport_strategies %>%
  arrange(desc(divert_ratio)) %>%
  slice_head(n = 10)

top_divert_airports %>%
  select(airport, airport_name, divert_ratio, delay_ratio, cancel_ratio)

#Visualisation — comparaison des stratégies (Top 10)
top_delay_airports %>%
  select(airport, delay_ratio, cancel_ratio, divert_ratio) %>%
  pivot_longer(-airport, names_to = "type", values_to = "ratio") %>%
  ggplot(aes(x = reorder(airport, -ratio), y = ratio, fill = type)) +
  geom_col(position = "stack") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Stratégies de gestion des perturbations – Top aéroports (retard)",
    x = "Aéroport",
    y = "Ratio",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 3/ ranking airport by reliability (weather, nas, and security delays are reliability issues, delay < cancel in terms of reliability)
#Calcul des indicateurs par aéroport
airport_reliability <- airline_delay_cause %>%
  group_by(airport, airport_name) %>%
  summarise(
    flights = sum(arr_flights, na.rm = TRUE),
    
    cancelled = sum(arr_cancelled, na.rm = TRUE),
    
    weather_delay = sum(weather_delay, na.rm = TRUE),
    nas_delay = sum(nas_delay, na.rm = TRUE),
    security_delay = sum(security_delay, na.rm = TRUE),
    
    total_delay = sum(arr_delay, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(flights > 500)   # seuil de représentativité

#Calcul des indicateurs par aéroport

airport_reliability <- airport_reliability %>%
  mutate(
    cancel_ratio = cancelled / flights,
    
    weather_ratio = weather_delay / total_delay,
    nas_ratio = nas_delay / total_delay,
    security_ratio = security_delay / total_delay
  )

#Score de non-fiabilité 

airport_reliability <- airport_reliability %>%
  mutate(
    unreliability_score =
      2 * cancel_ratio +
      weather_ratio +
      nas_ratio +
      security_ratio
  )

# Classement des aéroports (les PLUS fiables)

most_reliable_airports <- airport_reliability %>%
  arrange(unreliability_score) %>%
  slice_head(n = 10)

most_reliable_airports %>%
  select(
    airport,
    airport_name,
    unreliability_score,
    cancel_ratio,
    weather_ratio,
    nas_ratio,
    security_ratio
  )

# Aéroports les MOINS fiables
least_reliable_airports <- airport_reliability %>%
  arrange(desc(unreliability_score)) %>%
  slice_head(n = 10)

least_reliable_airports %>%
  select(
    airport,
    airport_name,
    unreliability_score,
    cancel_ratio,
    weather_ratio,
    nas_ratio,
    security_ratio
  )


# Visualisation du ranking (Top 10 fiables)

most_reliable_airports %>%
  ggplot(aes(
    x = reorder(airport, unreliability_score),
    y = unreliability_score
  )) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 des aéroports les plus fiables",
    x = "Aéroport",
    y = "Score de non-fiabilité"
  ) +
  theme_minimal()

# 4/ causes impact on delays (mean delay impact on delay for each cause)

# Filter for delayed flights
delayed_flights <- airline_delay_cause %>%
  filter(arr_del15 > 0)

# Calculate mean delay for each cause
mean_delay_by_cause <- delayed_flights %>%
  summarise(
    mean_weather_delay = mean(weather_delay, na.rm = TRUE),
    mean_nas_delay = mean(nas_delay, na.rm = TRUE),
    mean_security_delay = mean(security_delay, na.rm = TRUE),
    mean_carrier_delay = mean(carrier_delay, na.rm = TRUE),
    mean_late_aircraft_delay = mean(late_aircraft_delay, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "cause",
    values_to = "mean_delay"
  )

# Plot
ggplot(mean_delay_by_cause, aes(x = cause, y = mean_delay, fill = cause)) +
  geom_col() +
  labs(
    title = "Mean Delay Impact by Cause",
    x = "Cause of Delay",
    y = "Mean Delay (minutes)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 5/ cause analysis, which causes dominate others

# Filter for delayed flights
delayed_flights <- airline_delay_cause %>%
  filter(arr_del15 > 0)

# Sum delays by cause
total_delays_by_cause <- delayed_flights %>%
  summarise(
    total_weather_delay = sum(weather_delay, na.rm = TRUE),
    total_nas_delay = sum(nas_delay, na.rm = TRUE),
    total_security_delay = sum(security_delay, na.rm = TRUE),
    total_carrier_delay = sum(carrier_delay, na.rm = TRUE),
    total_late_aircraft_delay = sum(late_aircraft_delay, na.rm = TRUE),
    total_delay = sum(arr_delay, na.rm = TRUE)
  )

# Calculate proportion for each cause
proportion_delays_by_cause <- total_delays_by_cause %>%
  pivot_longer(
    cols = -total_delay,
    names_to = "cause",
    values_to = "total_delay_by_cause"
  ) %>%
  mutate(
    proportion = total_delay_by_cause / total_delay
  )

# Plot
ggplot(proportion_delays_by_cause, aes(x = cause, y = proportion, fill = cause)) +
  geom_col() +
  labs(
    title = "Proportion of Delay Causes (Delayed Flights Only)",
    x = "Cause of Delay",
    y = "Proportion of Total Delays"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



