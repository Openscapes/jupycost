user_costs_fixture <- function() {
  data.frame(
    date = c("2026-10-01", "2026-10-01", "2026-10-01", "2026-10-01"),
    user = c("alice", "alice", "alice", "bob"),
    hub = "prod",
    component = c("compute", "compute", "compute", "home storage"),
    value = c(1.5, 1.5, 1.5, 0.25),
    usergroup = c("group-a", "group-b", "multiple", "none")
  )
}

local_mock_cost_monitoring <- function(
  costs = user_costs_fixture(),
  hubs = c("support", "prod"),
  env = parent.frame()
) {
  calls <- new.env()
  calls$queries <- list()
  local_mocked_bindings(
    cost_monitoring_get = function(grafana_url, grafana_token, path, query) {
      if (path == "hub-names") {
        return(hubs)
      }
      calls$queries <- c(calls$queries, list(query))
      costs[costs$hub %in% query$hub, , drop = FALSE]
    },
    .env = env
  )
  calls
}
