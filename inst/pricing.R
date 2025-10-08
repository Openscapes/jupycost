library(readr)
library(dplyr)
library(paws)
library(duckplyr)

svc <- pricing()
# Retrieves the service for the given Service Code.
svc$describe_services(
  FormatVersion = "aws_v1",
  MaxResults = 10L,
  ServiceCode = "AmazonEC2"
)

price_lists <- svc$list_price_lists(
  ServiceCode = "AmazonEC2",
  EffectiveDate = as.POSIXct("2025-03-17"),
  CurrencyCode = "USD"
)

arn <- Filter(
  \(x) x$RegionCode == "us-west-2",
  price_lists$PriceLists
)[[1]]$PriceListArn

url <- svc$get_price_list_file_url(
  PriceListArn = arn,
  FileFormat = "csv"
)$Url

download.file(url[[1]], "inst/ec2-prices.csv")

db_exec("INSTALL httpfs")
db_exec("LOAD httpfs")

ret <- read_csv_duckdb(url) |>
  filter(
    `Instance Type` == "r5.xlarge",
    TermType == "OnDemand",
    `Operating System` == "Linux",
    CapacityStatus == "Used",
    (`Pre Installed S/W` == "NA" | is.na(`Pre Installed S/W`))
  ) |>
  collect()

# Standard Kubernetes version support	$0.10 per cluster per hour
# What does EBS cost?
