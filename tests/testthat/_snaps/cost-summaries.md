# get_daily_usage_costs input validation works

    Code
      get_daily_usage_costs(end_date = "not a date")
    Condition
      Error in `get_daily_usage_costs()`:
      ! `end_date` must be a length 1 Date or POSIXt object, or character in a standard unambiguous date format

---

    Code
      get_daily_usage_costs(months_back = 13)
    Condition
      Error in `get_daily_usage_costs()`:
      ! `months_back` must be an integer <= 12.

---

    Code
      get_daily_usage_costs(months_back = 1.5)
    Condition
      Error in `get_daily_usage_costs()`:
      ! `months_back` must be an integer <= 12.

---

    Code
      get_daily_usage_costs(cost_type = "invalid")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "unblended", "blended", "all"

