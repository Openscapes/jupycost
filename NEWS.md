# jupycost 0.1.0

## New Features

* Added `user_dir_snapshot()` function for retrieving user directory information at a point in time

* Added `dir_sizes()` function to query home directory sizes over time

* Added functions to track user CPU and memory requests and usage

* Added ability to query hub names and filter hubs by tags

* Improved Prometheus query handling with S3 classes and methods for range and instant queries

* Removed `create_range_df()` and use `format_prom_result()` to format results from the various prometheus query functions

## Minor Improvements

* Updated NASA hub names

* Improved documentation
