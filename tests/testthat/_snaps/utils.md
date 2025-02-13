# check_valid_date errors informatively for invalid inputs

    Code
      check_valid_date("not a date")
    Condition
      Error:
      ! `"not a date"` must be a length 1 Date or POSIXt object, or character in a standard unambiguous date format

---

    Code
      check_valid_date("2024-35-19")
    Condition
      Error:
      ! `"2024-35-19"` must be a length 1 Date or POSIXt object, or character in a standard unambiguous date format

---

    Code
      check_valid_date(c("2024-01-01", "2024-01-02"))
    Condition
      Error:
      ! `c("2024-01-01", "2024-01-02")` must be a length 1 Date or POSIXt object, or character in a standard unambiguous date format

---

    Code
      check_valid_date(NA_character_)
    Condition
      Error:
      ! `NA_character_` must be a length 1 Date or POSIXt object, or character in a standard unambiguous date format

---

    Code
      check_valid_date(NULL)
    Condition
      Error:
      ! `NULL` must be a length 1 Date or POSIXt object, or character in a standard unambiguous date format

# check_valid_date includes argument name in error

    Code
      check_valid_date("not a date", arg = "my_date")
    Condition
      Error:
      ! `my_date` must be a length 1 Date or POSIXt object, or character in a standard unambiguous date format

