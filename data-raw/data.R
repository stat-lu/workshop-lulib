library(tidyverse)
library(lubridate)
library(titanic)

mammals <- msleep %>%
  select(-sleep_cycle, -awake, -conservation) %>%
  drop_na()

write_csv(mammals, "data/mammals.csv")

covid_raw <- read_csv(
  "https://raw.githubusercontent.com/stat-lu/dataviz/main/data/covid.csv"
)

covid <- covid_raw %>%
  filter(continent == "europe", indicator == "cases") %>%
  mutate(
    month = as.integer(date),
    year = as.integer(year),
    week = as.integer(week),
    weekly_count_prop = 1e5 * weekly_count / population,
    cumulative_count_prop = 1e5 * weekly_count / population
  ) %>%
  select(
    country = region,
    year,
    month,
    week,
    date,
    population = population,
    weekly_count,
    weekly_count_prop,
    cumulative_count,
    cumulative_count_prop
  ) %>%
  arrange(country, year, month) %>%
  drop_na()

write_csv(covid, "data/covid.csv")

titanic <-
  bind_rows(titanic_train, titanic_test) %>%
  as_tibble() %>%
  select(
    id = PassengerId,
    class = Pclass,
    age = Age,
    status = Survived,
    gender = Sex
  ) %>%
  mutate(
    status = factor(status, levels = c(0, 1), labels = c("Survived", "Died")),
    gender = factor(
      gender,
      levels = c("male", "female"),
      labels = c("Man", "Woman")
    )
  ) %>%
  drop_na()

write_csv(titanic, "data/titanic.csv")
