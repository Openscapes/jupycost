library(tidyverse)
library(jupycost)

jupycost:::set_env_vars("nasa")

nasa_dirs <- dir_sizes(
  start_time = "2025-01-01",
  end_time = Sys.Date(),
  by_user = TRUE,
  step = "1h0m0s"
)

nasa_dirs |>
  group_by(namespace) |>
  summarise(n_users = n_distinct(directory))

nasa_workshop_users <- nasa_dirs |>
  filter(namespace == "workshop") |>
  distinct(directory) |>
  filter(
    str_detect(
      tolower(directory),
      "(teucher)|(mikala)|(thornton)|(yuvi)|(mahsa)|(bolch)|(steiker)|(luis\\.*lopez)|(beto)|(rupesh)|(^andy$)",
      negate = TRUE
    )
  ) |>
  nrow()

nasa_hourly_users <- get_hourly_users(
  start_time = as.POSIXct("2025-01-01 00:00", tz = "America/New_York"),
  end_time = Sys.time(),
  step = "1h0m0s"
)

nasa_hourly_users |>
  filter(namespace == "workshop") |>
  mutate(date = as.Date(date_time)) |>
  group_by(namespace, date) |>
  summarise(n_users = max(n_users)) |>
  ggplot(aes(x = date, y = n_users)) +
  geom_line() +
  labs(
    title = "Number of active users on the NASA workshop JupyterHub",
    x = "Date",
    y = "Number of active users"
  ) +
  theme_minimal(base_size = 16)


############ NOAA ###############
jupycost:::set_env_vars("nmfs")

noaa_dirs <- dir_sizes(
  grafana_url = "https://grafana.nmfs-openscapes.2i2c.cloud",
  start_time = "2025-01-01",
  end_time = Sys.Date(),
  by_user = TRUE,
  step = "1h0m0s"
)

noaa_users <- noaa_dirs |>
  group_by(namespace) |>
  summarise(n_users = n_distinct(directory))

noaa_hourly_users <- get_hourly_users(
  grafana_url = "https://grafana.nmfs-openscapes.2i2c.cloud",
  start_time = as.POSIXct("2025-01-01 00:00", tz = "America/New_York"),
  end_time = Sys.time(),
  step = "1h0m0s"
)

noaa_hourly_users |>
  filter(namespace == "workshop") |>
  mutate(date = as.Date(date_time)) |>
  group_by(namespace, date) |>
  summarise(n_users = max(n_users)) |>
  ggplot(aes(x = date, y = n_users)) +
  geom_line() +
  labs(
    title = "Number of active users on the NMFS workshop JupyterHub",
    x = "Date",
    y = "Number of active users"
  ) +
  theme_minimal(base_size = 16)
