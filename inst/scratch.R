# 'sum(
#   kube_pod_container_resource_requests{resource="memory", namespace=~"$hub", node=~"$instance"}
# ) by (pod, namespace)'

Sys.setenv(
  "GRAFANA_TOKEN" = Sys.getenv("NASA_GRAFANA_TOKEN"),
  "AWS_ACCESS_KEY_ID" = Sys.getenv("NASA_AWS_ACCESS_KEY_ID"),
  "AWS_SECRET_ACCESS_KEY" = Sys.getenv("NASA_AWS_SECRET_ACCESS_KEY"),
  "AWS_REGION" = "us-east-1"
)

query_prometheus_range(
  query = 'sum(
  kube_pod_container_resource_requests{resource="cpu", namespace="prod", instance=".*"}
) by (pod, namespace)',
  start_time = "2024-01-01",
  end_time = "2024-01-30",
  step = 1
)
