# aws_billing_daily errors when the response is paginated

    Code
      aws_billing_daily("2024-01-01", "2024-01-02")
    Condition
      Error in `map2()`:
      i In index: 1.
      i With name: unblended.
      Caused by error in `aws_billing_one()`:
      ! The Cost Explorer response was paginated, so some costs would be missing.
      i Try a shorter date range.

