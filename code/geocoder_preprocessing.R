# This script reads the original sales data, cleans it by filtering empty sales or too low sale prices; 
# also keeps registers that have Zip Code and size info.
# It creates four new columns:
# Street: street of the address so it can be fed into the Python geocoder
# Street Number: Number of the street so it can be fed into the Python geocoder
# Borough Name: so it can be fed into the Python and Google geocoder
# Complete Address: so it can be fed into the Google geocoder

read_csv("../data/nyc-rolling-sales.csv",
                      na = c("", "NA", "-")) %>% 
  make_names() %>% 
  mutate(id_sale = paste(X1, BOROUGH, sep = "_")) %>% 
  filter(SALE_PRICE > 7000, 
         !is.na(SALE_PRICE)) %>% 
  filter(!is.na(LAND_SQUARE_FEET),
         LAND_SQUARE_FEET > 0,
         !is.na(GROSS_SQUARE_FEET),
         GROSS_SQUARE_FEET > 0,
         ZIP_CODE != 0) %>% 
  mutate(Street = stringi::stri_split(ADDRESS, regex = "^[A-Za-z0-9\\-]+ ", simplify = T)[,2],
         Street_Number = stringi::stri_extract_first(ADDRESS, regex = "^[A-Za-z0-9\\-]+", simplyfy = T),
         BOROUGH_NAME = 
           ifelse(BOROUGH == 1, "Manhattan", ifelse(
             BOROUGH == 2, "Bronx", ifelse(
               BOROUGH == 3, "Brooklyn", ifelse(
                 BOROUGH == 4, "Queens", "Staten Island")))
           ),
         Complete_address = paste0(
           ADDRESS, 
           ", ", 
           NEIGHBORHOOD,
           ", ",
           BOROUGH_NAME, 
           ", New York, ", 
           ZIP_CODE, 
           ", USA")) %>% 
  write_csv(., "../out/nyc_sales_clean.csv")
