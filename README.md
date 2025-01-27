
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jupycost

<!-- badges: start -->

[![R-CMD-check](https://github.com/Openscapes/jupycost/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Openscapes/jupycost/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

jupycost provides functions for Openscapes to query and monitor patterns
of usage and costs of the [JupyterHubs](https://openscapes.cloud) that
we adminster in partnership with [2i2c](https://2i2c.org).

We monitor usage by querying a Prometheus time-series database, as well
as the AWS Cost-Explorer API. jupycost allows us to do automated
periodic reporting of usage. We also use Grafana to monitor usage
interactively.

## Installation

You can install the development version of jupycost from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("Openscapes/jupycost")
```

## Example

This is a basic example which shows you how to solve a common problem:
