raw_credits <- function() {
  month <- function(start, amount) {
    list(
      TimePeriod = list(Start = start),
      Total = list(UnblendedCost = list(Amount = amount))
    )
  }
  list(
    ResultsByTime = list(
      month("2026-08-01", "-1243.38"),
      month("2026-09-01", "-1243.22")
    )
  )
}

raw_billing_credits <- function() {
  credit <- function(id, description, initial, remaining, exhaust = NULL) {
    list(
      creditId = id,
      description = description,
      initialAmount = list(currencyCode = "USD", currencyAmount = initial),
      remainingAmount = list(currencyCode = "USD", currencyAmount = remaining),
      estimatedAmount = list(currencyCode = "USD", currencyAmount = remaining),
      startDate = as.POSIXct("2025-09-01", tz = "UTC"),
      endDate = as.POSIXct("2027-09-01", tz = "UTC"),
      exhaustDate = exhaust
    )
  }
  list(
    credits = list(
      credit(
        "1",
        "Old",
        "18000.000000",
        "0.000000",
        as.POSIXct("2025-11-01", tz = "UTC")
      ),
      credit("2", "NASA", "30000.000000", "18311.720000")
    )
  )
}
