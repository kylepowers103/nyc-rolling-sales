library(tidyverse)

nyc_sales <- read_rds("../out/nyc_sales.rds") %>% 
  rename(id_sale = id)

nyc_double_check <- nyc_sales %>% 
  filter(
    id_sale != "5717_5", # Manually remove these because theydin't have address number
    id_sale != "7719_5",
    (loc_type == "GEOMETRIC CENTER") |
      google_status == "OVER_QUERY_LIMIT" |
      google_status == "UNKNOWN_ERROR" |
      google_status == "ZERO_RESULTS" |
      is.na(google_status) |
      is.na(loc_type)
  ) %>% 
  mutate(Street = stringi::stri_split(ADDRESS, regex = "^[A-Za-z0-9\\-]+ ", simplify = T)[,2],
         Street_Number = stringi::stri_extract_first(ADDRESS, regex = "^[A-Za-z0-9\\-]+", simplyfy = T))


  
write_csv(nyc_double_check, "../out/double_check.csv")
