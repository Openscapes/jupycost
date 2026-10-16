# jupycost (development version)

# jupycost 0.2.0

- New `get_credits_remaining()` returns the balance of each AWS credit, from the Billing API.
- New `get_credits_used()` returns the AWS credits applied per month, from Cost Explorer.
- `get_daily_usage_costs()` gains a `start_date` argument.
- The `"fixed"` `service_component` (Amazon EKS and tagged `"EC2 - Other"` costs) is renamed `"core"` to match 2i2c's cost categories.
  It now also includes core node instance hours, which were previously classed as `"compute"`.
- `get_daily_usage_costs()` now splits costs into `service_component`s using 2i2c's cost allocation tags: `"home storage"` (EBS volumes and snapshots backing `jupyterhub-home-nfs`), `"core"` (core node instance hours and volumes, NAT gateway, hub database and support volumes), and `"compute"` (the remainder).
  Previously all `"EC2 - Other"` costs and core node instance hours were classed as `"compute"`.

# jupycost 0.1.0

## New Features

- Added `user_dir_snapshot()` function for retrieving user directory information at a point in time

- Added `dir_sizes()` function to query home directory sizes over time

- Added functions to track user CPU and memory requests and usage

- Added ability to query hub names and filter hubs by tags

- Improved Prometheus query handling with S3 classes and methods for range and instant queries

- Removed `create_range_df()` and use `format_prom_result()` to format results from the various prometheus query functions

## Minor Improvements

- Updated NASA hub names

- Improved documentation
