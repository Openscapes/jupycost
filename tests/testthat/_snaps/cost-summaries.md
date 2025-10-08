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

---

    Code
      get_daily_usage_costs(hub = "invalid")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "all", "prod", "staging", "workshop", "support"

---

    Code
      get_daily_usage_costs(cluster = "invalid")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "openscapeshub", "nmfs-openscapes"

# get_daily_usage_costs correctly combines cluster and hub filters

    Code
      filter_str
    Output
      [1] "list(And = list(list(Dimensions = list(Key = \"RECORD_TYPE\", Values = list("
      [2] "    \"Usage\"))), list(Tags = list(Key = \"2i2c:hub-name\", Values = list("  
      [3] "    \"prod\"), MatchOptions = list(\"EQUALS\")))))"                          

# get_daily_usage_costs filter structure is correct for support hub

    Code
      deparse(filter_capture)
    Output
      [1] "list(And = list(list(Dimensions = list(Key = \"RECORD_TYPE\", Values = list("    
      [2] "    \"Usage\"))), list(Tags = list(Key = \"2i2c:hub-name\", MatchOptions = list("
      [3] "    \"ABSENT\")))))"                                                             

# get_daily_usage_costs cluster filter includes all required tag keys

    Code
      deparse(filter_capture)
    Output
       [1] "list(And = list(list(Dimensions = list(Key = \"RECORD_TYPE\", Values = list("              
       [2] "    \"Usage\"))), list(Or = list(list(Tags = list(Key = \"alpha.eksctl.io/cluster-name\", "
       [3] "    Values = list(\"openscapeshub\"), MatchOptions = list(\"EQUALS\"))), "                 
       [4] "    list(Tags = list(Key = \"kubernetes.io/cluster/openscapeshub\", "                      
       [5] "        Values = list(\"owned\"), MatchOptions = list(\"EQUALS\"))), "                     
       [6] "    list(Tags = list(Key = \"2i2c.org/cluster-name\", Values = list("                      
       [7] "        \"openscapeshub\"), MatchOptions = list(\"EQUALS\"))), list("                      
       [8] "        Not = list(Tags = list(Key = \"2i2c:hub-name\", MatchOptions = list("              
       [9] "            \"ABSENT\")))), list(Not = list(Tags = list(Key = \"2i2c:node-purpose\", "     
      [10] "        MatchOptions = list(\"ABSENT\"))))))))"                                            

---

    Code
      deparse(filter_capture)
    Output
       [1] "list(And = list(list(Dimensions = list(Key = \"RECORD_TYPE\", Values = list("              
       [2] "    \"Usage\"))), list(Or = list(list(Tags = list(Key = \"alpha.eksctl.io/cluster-name\", "
       [3] "    Values = list(\"nmfs-openscapes\"), MatchOptions = list(\"EQUALS\"))), "               
       [4] "    list(Tags = list(Key = \"kubernetes.io/cluster/nmfs-openscapes\", "                    
       [5] "        Values = list(\"owned\"), MatchOptions = list(\"EQUALS\"))), "                     
       [6] "    list(Tags = list(Key = \"2i2c.org/cluster-name\", Values = list("                      
       [7] "        \"nmfs-openscapes\"), MatchOptions = list(\"EQUALS\"))), "                         
       [8] "    list(Not = list(Tags = list(Key = \"2i2c:hub-name\", MatchOptions = list("             
       [9] "        \"ABSENT\")))), list(Not = list(Tags = list(Key = \"2i2c:node-purpose\", "         
      [10] "        MatchOptions = list(\"ABSENT\"))))))))"                                            

