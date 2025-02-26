library(dplyr)
library(lubridate)
library(ggplot2)

set_env_vars("nasa")

end_date <- Sys.Date()
start_date <- end_date - months(24)

dir_sizes <- query_prometheus_range(
  query = "max(dirsize_total_size_bytes) by (directory, namespace)",
  start_time = start_date,
  end_time = end_date,
  step = 60 * 60 * 24
) |>
  create_range_df(value_name = "size") |>
  mutate(
    directory = unsanitize_dir_names(directory),
    size = size * 1e-9
  ) |>
  filter(!directory %in% c(".ipynb_checkpoints", "_shared")) |>
  filter(namespace != "staging")

n_users_over_time <- dir_sizes |>
  group_by(date, namespace) |>
  summarise(
    n_users = n_distinct(directory)
  )

n_users_over_time |>
  group_by(namespace) |>
  summarise(
    users_now = last(n_users, date),
    users_start = first(n_users, date, na_rm = TRUE)
  )
beginning <- n_users_over_time$n_users[
  n_users_over_time$date == min(n_users_over_time$date)
]

ever <- dir_sizes |>
  group_by(namespace) |>
  summarise(n_distinct_users = n_distinct(directory))

ggplot(n_users_over_time, aes(x = date, y = n_users)) +
  geom_line()

daily_users <- get_daily_users(start_time = start_date, end_time = end_date)
